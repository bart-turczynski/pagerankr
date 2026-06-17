#' @title Audit Declared Canonical Links
#' @description Analyses a `rel=canonical` data frame and returns a diagnostic
#'   report covering chain lengths, loops, conflicting sources (a page declaring
#'   multiple distinct canonicals), self-referencing canonicals, and terminal
#'   destinations. Mirrors [audit_redirects()] for the canonical signal; useful
#'   as a pre-flight check before passing `canonicals_df` to [pagerank()].
#'
#'   Declared canonicals are an **advisory** signal, distinct from enforced 3xx
#'   redirects. To see how the two interact -- which one wins on a shared
#'   source,
#'   and which canonicals are ignored because their source also redirects -- use
#'   [audit_fold()].
#'
#' @param canonicals_df A data frame of declared canonical links, pairing a
#'   source URL with the canonical it declares.
#' @param edge_list_df Optional data frame of edges. If provided, orphaned
#'   canonicals (sources not present in the edge list) are identified.
#' @param canonical_from_col,canonical_to_col From/to columns in
#'   `canonicals_df`. Default `"from"` / `"to"`.
#' @param edge_from_col,edge_to_col From/to columns in `edge_list_df`. Default
#'   `"from"` / `"to"`.
#'
#' @return A list with class `"canonical_audit"` mirroring the structure of
#'   [audit_redirects()]: `n_rules`, `n_self_refs`, `self_refs`, `n_conflicts`,
#'   `conflicts`, `n_loops`, `loops`, `chains`, `max_chain_length`, and (when
#'   `edge_list_df` is given) `orphaned_redirects` (orphaned canonical sources).
#' @seealso [audit_redirects()], [audit_fold()], [build_fold_map()]
#' @export
#' @examples
#' canonicals <- data.frame(
#'   from = c("http://a?x=1", "http://b", "http://c"),
#'   to = c("http://a", "http://canon", "http://c"),
#'   stringsAsFactors = FALSE
#' )
#' audit_canonicals(canonicals)
audit_canonicals <- function(canonicals_df,
                             edge_list_df = NULL,
                             canonical_from_col = "from",
                             canonical_to_col = "to",
                             edge_from_col = "from",
                             edge_to_col = "to") {
  result <- .audit_signal(
    signal_df = canonicals_df,
    edge_list_df = edge_list_df,
    from_col = canonical_from_col,
    to_col = canonical_to_col,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    what = "canonicals_df"
  )
  class(result) <- "canonical_audit"
  result
}


#' @export
print.canonical_audit <- function(x, ...) {
  cat("=== Canonical Audit Report ===\n\n")
  cat("Total rules (after NA removal):", x$n_rules, "\n")
  cat("Self-referencing canonicals:    ", x$n_self_refs, "\n")
  cat("Conflicting sources:            ", x$n_conflicts, "\n")
  cat("Canonical loops:                ", x$n_loops, "\n")
  cat("Max chain length:               ", x$max_chain_length, "\n")

  if (x$n_self_refs > 0) {
    cat("\n--- Self-referencing canonicals ---\n")
    print(x$self_refs, row.names = FALSE)
  }

  if (x$n_conflicts > 0) {
    cat("\n--- Conflicting sources ---\n")
    print(x$conflicts, row.names = FALSE)
  }

  if (x$n_loops > 0) {
    cat("\n--- Loops ---\n")
    for (i in seq_along(x$loops)) {
      cat("  ", x$loops[[i]], "\n")
    }
  }

  if (!is.null(x$orphaned_redirects) && nrow(x$orphaned_redirects) > 0) {
    cat(
      "\nOrphaned canonicals (not in edge list):",
      nrow(x$orphaned_redirects), "\n"
    )
  }

  if (nrow(x$chains) > 0) {
    long <- x$chains[x$chains$chain_length > 1, , drop = FALSE]
    if (nrow(long) > 0) {
      cat("\n--- Long chains (>1 hop) ---\n")
      print(long, row.names = FALSE)
    }
  }

  invisible(x)
}


#' @title Combined Cross-Signal Fold Audit (Redirects + Canonicals)
#' @description Audits how 3xx **redirects** and declared **rel=canonical**
#'   links combine into a single fold map, surfacing exactly where the two
#'   signals interact. Wraps [audit_redirects()] and [audit_canonicals()] for
#'   the per-signal views and adds the cross-signal tables from
#'   [build_fold_map()]: same-source disagreements, canonicals ignored because
#'   their source also redirects, and the `canonical_conflict_policy` outcome.
#'
#'   Disagreements are never silently resolved -- they are always reported here,
#'   regardless of which policy decides the winner.
#'
#' @inheritParams build_fold_map
#' @param edge_list_df Optional edge list, passed to the per-signal audits for
#'   orphan detection.
#' @param edge_from_col,edge_to_col From/to columns in `edge_list_df`.
#'
#' @return A list with class `"fold_audit"` containing:
#'   \describe{
#'     \item{redirects}{The [audit_redirects()] result (or `NULL`).}
#'     \item{canonicals}{The [audit_canonicals()] result (or `NULL`).}
#'     \item{conflicts}{Data frame of same-source redirect-vs-canonical cases:
#'       `source`, `redirect_to`, `canonical_to`, `disagrees`, `resolution`.}
#'     \item{ignored_canonicals}{Data frame of canonicals dropped because their
#'       source also redirects (populated under `"redirect_wins"`).}
#'     \item{conflict_policy}{The `canonical_conflict_policy` in effect.}
#'   }
#' @seealso [audit_redirects()], [audit_canonicals()], [build_fold_map()]
#' @export
#' @examples
#' redirects <- data.frame(from = "http://a", to = "http://b")
#' canonicals <- data.frame(from = "http://a", to = "http://d")
#' # a redirects to b but also declares canonical d => disagreement
#' audit_fold(redirects, canonicals)
audit_fold <- function(redirects_df = NULL,
                       canonicals_df = NULL,
                       edge_list_df = NULL,
                       redirect_from_col = "from",
                       redirect_to_col = "to",
                       canonical_from_col = "from",
                       canonical_to_col = "to",
                       duplicate_from_policy = c(
                         "strict", "first_wins", "last_wins",
                         "most_frequent", "prune_source",
                         "resolve_if_consistent"
                       ),
                       loop_handling = c(
                         "error", "prune_loop", "break_arrow"
                       ),
                       canonical_duplicate_from_policy = c(
                         "strict", "first_wins", "last_wins",
                         "most_frequent", "prune_source",
                         "resolve_if_consistent"
                       ),
                       canonical_loop_handling = c(
                         "error", "prune_loop", "break_arrow"
                       ),
                       canonical_conflict_policy = c(
                         "redirect_wins", "error", "canonical_wins"
                       ),
                       edge_from_col = "from",
                       edge_to_col = "to") {
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)
  canonical_duplicate_from_policy <- match.arg(canonical_duplicate_from_policy)
  canonical_loop_handling <- match.arg(canonical_loop_handling)
  canonical_conflict_policy <- match.arg(canonical_conflict_policy)

  result <- list(
    redirects = NULL,
    canonicals = NULL,
    conflicts = NULL,
    ignored_canonicals = NULL,
    conflict_policy = canonical_conflict_policy
  )

  if (!is.null(redirects_df) && nrow(redirects_df) > 0) {
    result$redirects <- audit_redirects(
      redirects_df,
      edge_list_df = edge_list_df,
      redirect_from_col = redirect_from_col,
      redirect_to_col = redirect_to_col,
      edge_from_col = edge_from_col,
      edge_to_col = edge_to_col
    )
  }
  if (!is.null(canonicals_df) && nrow(canonicals_df) > 0) {
    result$canonicals <- audit_canonicals(
      canonicals_df,
      edge_list_df = edge_list_df,
      canonical_from_col = canonical_from_col,
      canonical_to_col = canonical_to_col,
      edge_from_col = edge_from_col,
      edge_to_col = edge_to_col
    )
  }

  # Cross-signal tables come from the same composition engine pagerank() uses.
  # Under "error", a genuine disagreement throws; surface it as the audit's job
  # is to make conflicts visible, so we report rather than abort here.
  fold <- tryCatch(
    .compose_fold_map(
      redirects_df = redirects_df,
      canonicals_df = canonicals_df,
      redirect_from_col = redirect_from_col,
      redirect_to_col = redirect_to_col,
      canonical_from_col = canonical_from_col,
      canonical_to_col = canonical_to_col,
      duplicate_from_policy = duplicate_from_policy,
      loop_handling = loop_handling,
      canonical_duplicate_from_policy = canonical_duplicate_from_policy,
      canonical_loop_handling = canonical_loop_handling,
      # Report conflicts without aborting, even when the active policy is
      # "error"; the disagreement is still flagged via `disagrees`.
      canonical_conflict_policy = if (canonical_conflict_policy == "error") {
        "redirect_wins"
      } else {
        canonical_conflict_policy
      }
    ),
    error = function(e) NULL
  )
  if (!is.null(fold)) {
    result$conflicts <- fold$conflicts
    result$ignored_canonicals <- fold$ignored_canonicals
  }

  class(result) <- "fold_audit"
  result
}


#' @export
print.fold_audit <- function(x, ...) {
  cat("=== Combined Fold Audit (redirects + canonicals) ===\n\n")
  cat("Conflict policy:", x$conflict_policy, "\n")

  n_redirect_rules <- if (is.null(x$redirects)) 0L else x$redirects$n_rules
  n_canonical_rules <- if (is.null(x$canonicals)) 0L else x$canonicals$n_rules
  cat("Redirect rules: ", n_redirect_rules, "\n")
  cat("Canonical rules:", n_canonical_rules, "\n")

  n_disagree <- if (is.null(x$conflicts)) 0L else sum(x$conflicts$disagrees)
  n_ignored <- if (is.null(x$ignored_canonicals)) {
    0L
  } else {
    nrow(x$ignored_canonicals)
  }
  cat("Same-source disagreements:", n_disagree, "\n")
  cat("Ignored canonicals (source also redirects):", n_ignored, "\n")

  if (!is.null(x$conflicts) && nrow(x$conflicts) > 0) {
    cat("\n--- Same-source redirect/canonical cases ---\n")
    print(x$conflicts, row.names = FALSE)
  }

  invisible(x)
}
