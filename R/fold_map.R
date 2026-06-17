#' @title Build a Composed Fold Map from Redirects and Canonicals
#' @description Composes a single URL fold map from two distinct web signals --
#'   3xx **redirects** and declared **rel=canonical** links -- and reports, per
#'   folded URL, which signal caused the fold. This is the source of truth that
#'   [pagerank()] uses to fold edge endpoints and TIPR prior URLs, and that the
#'   downstream `semantic` bridge consumes to build its `graph_fold` table
#'   without duplicating the composition logic.
#'
#'   The two signals are kept separate internally for auditability and resolved
#'   with their own duplicate/loop policies, then composed with explicit
#'   precedence (see Details). Self-referential pairs (self-redirects,
#'   self-canonicals) are dropped as no-ops.
#'
#' @param redirects_df Optional data frame of 3xx redirect rules, or `NULL`.
#' @param canonicals_df Optional data frame of declared rel=canonical links, or
#'   `NULL`. Each row pairs a source URL with the canonical it declares.
#' @param redirect_from_col,redirect_to_col From/to columns in `redirects_df`.
#'   Default `"from"` / `"to"`.
#' @param canonical_from_col,canonical_to_col From/to columns in
#'   `canonicals_df`. Default `"from"` / `"to"`.
#' @param duplicate_from_policy How to handle a redirect source with multiple
#'   distinct targets. See [resolve_redirects()]. Default `"strict"`.
#' @param loop_handling How to handle redirect cycles. See
#'   [resolve_redirects()]. Default `"error"`. Also governs cross-signal cycles
#'   in the composed graph.
#' @param canonical_duplicate_from_policy How to handle a canonical source with
#'   multiple distinct declared canonicals. Reuses the `duplicate_from_policy`
#'   enum. Default `"strict"`.
#' @param canonical_loop_handling How to handle cycles among declared
#'   canonicals. Reuses the `loop_handling` enum. Default `"error"`.
#' @param canonical_conflict_policy How to resolve a redirect-vs-canonical
#'   disagreement on the **same source** URL. One of:
#'   \describe{
#'     \item{`"redirect_wins"`}{(Default) The 3xx redirect wins; a canonical
#'       declared on a URL that itself redirects is ignored and flagged, never
#'       transferred onto the redirect target.}
#'     \item{`"error"`}{Error when a redirect and a canonical disagree on the
#'       same source (after audit context is computed). Sources where the two
#'       signals agree do not error.}
#'     \item{`"canonical_wins"`}{The declared canonical wins for that source;
#'       still flagged in the audit. The explicit exception to the default
#'       ignored-canonical-on-redirecting-source rule.}
#'   }
#'
#' @details
#' ## Composition semantics
#'
#' 1. The redirect rules are resolved to terminal destinations using
#'    `duplicate_from_policy` / `loop_handling`; the canonical rules are
#'    resolved **independently** using `canonical_duplicate_from_policy` /
#'    `canonical_loop_handling`. The two terminal maps are kept separate.
#' 2. They are then composed into one graph and resolved to terminals, so that:
#'    a **canonical target is itself redirect-resolved** before folding (a
#'    canonical may point at a URL that 3xx's), and chains spanning both signals
#'    collapse to a single representative.
#' 3. For the **same source**, `canonical_conflict_policy` decides the winner.
#'    Under the default `"redirect_wins"`, the canonical declared on a
#'    redirecting source is dropped and recorded in the audit.
#'
#' Inputs are expected to be **already canonicalized** to the node namespace
#' (e.g. via the same `rurl` profile used for edges). [pagerank()] cleans
#' redirects and canonicals before composing; call this directly only when your
#' URLs already share that namespace.
#'
#' @return A data frame with one row per folded source URL (rows where the URL
#'   actually changes), with columns:
#'   \describe{
#'     \item{from}{The source URL.}
#'     \item{to}{Its final composed representative.}
#'     \item{signal}{Which signal folded this source: `"redirect"` or
#'       `"canonical"`.}
#'   }
#'   The data frame additionally carries the cross-signal conflict tables as
#'   attributes `"conflicts"` and `"ignored_canonicals"` (see
#'   [audit_canonicals()] / `audit_fold()`).
#' @seealso [resolve_redirects()], [audit_canonicals()], [pagerank()]
#' @export
#' @examples
#' redirects <- data.frame(from = "http://a", to = "http://b")
#' canonicals <- data.frame(from = "http://c", to = "http://a")
#' # c declares canonical a, a redirects to b => c folds to b via both signals
#' build_fold_map(redirects, canonicals)
build_fold_map <- function(redirects_df = NULL,
                           canonicals_df = NULL,
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
                           )) {
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)
  canonical_duplicate_from_policy <- match.arg(canonical_duplicate_from_policy)
  canonical_loop_handling <- match.arg(canonical_loop_handling)
  canonical_conflict_policy <- match.arg(canonical_conflict_policy)

  fold <- .compose_fold_map(
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
    canonical_conflict_policy = canonical_conflict_policy
  )

  out <- data.frame(
    from = names(fold$map),
    to = unname(fold$map),
    signal = unname(fold$signal[names(fold$map)]),
    stringsAsFactors = FALSE
  )
  attr(out, "conflicts") <- fold$conflicts
  attr(out, "ignored_canonicals") <- fold$ignored_canonicals
  out
}


# --- Internal: compose redirect + canonical signals into one fold map ---

#' Compose redirect and canonical signals into a single terminal fold map
#'
#' The shared engine behind [build_fold_map()], [pagerank()] (edge + TIPR prior
#' folding), and `audit_fold()`. Keeps the redirect and canonical terminal maps
#' separate (for auditability), applies the conflict policy on shared sources,
#' then resolves the combined graph to idempotent terminal representatives.
#'
#' @inheritParams build_fold_map
#' @return A list with components:
#'   \describe{
#'     \item{map}{Named character vector (source -> representative) covering
#'       only sources whose URL actually changes. Idempotent: applying it to its
#'       own values is a no-op. Use [.apply_fold_map()] to fold URLs with it.}
#'     \item{signal}{Named character vector (source -> "redirect"|"canonical")
#'       over the same keys as `map`.}
#'     \item{redirect_terminal}{The standalone redirect terminal map.}
#'     \item{canonical_terminal}{The standalone canonical terminal map.}
#'     \item{conflicts}{Data frame of same-source redirect-vs-canonical cases:
#'       source, redirect_to, canonical_to, disagrees, resolution.}
#'     \item{ignored_canonicals}{Data frame of canonicals dropped because their
#'       source also redirects (only populated under `"redirect_wins"`).}
#'   }
#' @noRd
.compose_fold_map <- function(redirects_df = NULL,
                              canonicals_df = NULL,
                              redirect_from_col = "from",
                              redirect_to_col = "to",
                              canonical_from_col = "from",
                              canonical_to_col = "to",
                              duplicate_from_policy = "strict",
                              loop_handling = "error",
                              canonical_duplicate_from_policy = "strict",
                              canonical_loop_handling = "error",
                              canonical_conflict_policy = "redirect_wins") {
  empty_map <- stats::setNames(character(0), character(0))
  empty_conflicts <- data.frame(
    source = character(0), redirect_to = character(0),
    canonical_to = character(0), disagrees = logical(0),
    resolution = character(0), stringsAsFactors = FALSE
  )
  empty_ignored <- data.frame(
    source = character(0), canonical_to = character(0),
    redirect_to = character(0), stringsAsFactors = FALSE
  )

  # --- Build each signal's standalone terminal map (separate, auditable) ---
  r_term <- if (!is.null(redirects_df) && nrow(redirects_df) > 0) {
    .build_terminal_map(
      redirects_df[[redirect_from_col]], redirects_df[[redirect_to_col]],
      duplicate_from_policy = duplicate_from_policy,
      loop_handling = loop_handling
    )
  } else {
    empty_map
  }

  c_term <- if (!is.null(canonicals_df) && nrow(canonicals_df) > 0) {
    .build_terminal_map(
      canonicals_df[[canonical_from_col]], canonicals_df[[canonical_to_col]],
      duplicate_from_policy = canonical_duplicate_from_policy,
      loop_handling = canonical_loop_handling
    )
  } else {
    empty_map
  }

  # Effective sources: those whose URL actually changes under each signal.
  redirect_sources <- names(r_term)[r_term != names(r_term)]
  canonical_sources <- names(c_term)[c_term != names(c_term)]
  conflict_sources <- intersect(redirect_sources, canonical_sources)

  # --- Cross-signal conflict audit (same-source disagreements) ---
  conflicts <- empty_conflicts
  ignored_canonicals <- empty_ignored
  if (length(conflict_sources) > 0) {
    r_to <- unname(r_term[conflict_sources])
    c_to <- unname(c_term[conflict_sources])
    # Redirect-resolve the canonical terminal so disagreement is judged on the
    # final destinations, not the raw canonical hop.
    c_resolved <- .apply_fold_map(c_to, r_term)
    disagrees <- c_resolved != r_to

    resolution <- switch(canonical_conflict_policy,
      redirect_wins = "redirect",
      canonical_wins = "canonical",
      error = ifelse(disagrees, "error", "redirect")
    )
    conflicts <- data.frame(
      source = conflict_sources,
      redirect_to = r_to,
      canonical_to = c_to,
      disagrees = disagrees,
      resolution = resolution,
      stringsAsFactors = FALSE
    )

    if (canonical_conflict_policy == "error" && any(disagrees)) {
      bad <- conflicts[conflicts$disagrees, , drop = FALSE]
      stop(
        "Redirect/canonical conflict on ", nrow(bad),
        " source(s) under canonical_conflict_policy = \"error\". First: '",
        bad$source[1], "' redirects to '", bad$redirect_to[1],
        "' but declares canonical '", bad$canonical_to[1], "'.",
        call. = FALSE
      )
    }

    # Under redirect_wins, the canonical on a redirecting source is ignored.
    if (canonical_conflict_policy == "redirect_wins") {
      ignored_canonicals <- data.frame(
        source = conflict_sources,
        canonical_to = c_to,
        redirect_to = r_to,
        stringsAsFactors = FALSE
      )
    }
  }

  # --- Build the combined one-hop graph with per-source precedence ---
  # redirect edges: every effective redirect source, unless canonical_wins
  #   hands a conflicting source to the canonical signal.
  # canonical edges: every effective canonical source, unless redirect_wins
  #   (default) drops the canonical on a conflicting (redirecting) source.
  redirect_use <- redirect_sources
  canonical_use <- canonical_sources
  if (length(conflict_sources) > 0) {
    if (canonical_conflict_policy == "canonical_wins") {
      redirect_use <- setdiff(redirect_sources, conflict_sources)
    } else {
      # redirect_wins, and the no-disagreement branch of "error"
      canonical_use <- setdiff(canonical_sources, conflict_sources)
    }
  }

  combined_from <- c(redirect_use, canonical_use)
  combined_to <- c(
    unname(r_term[redirect_use]),
    unname(c_term[canonical_use])
  )
  signal_by_source <- stats::setNames(
    c(
      rep("redirect", length(redirect_use)),
      rep("canonical", length(canonical_use))
    ),
    combined_from
  )

  if (length(combined_from) == 0) {
    return(list(
      map = empty_map, signal = stats::setNames(character(0), character(0)),
      redirect_terminal = r_term, canonical_terminal = c_term,
      conflicts = conflicts, ignored_canonicals = ignored_canonicals
    ))
  }

  # Resolve the combined graph to idempotent terminals. Within-signal loops were
  # already handled by each signal's own policy; this pass resolves cross-signal
  # chains (and any residual cross-signal cycle) under `loop_handling`.
  composed <- .resolve_via_graph(combined_from, combined_to,
    loop_handling = loop_handling
  )

  # Keep only sources that actually change; attach the per-source signal.
  changed <- names(composed)[composed != names(composed)]
  map <- composed[changed]
  signal <- signal_by_source[changed]

  list(
    map = map,
    signal = signal,
    redirect_terminal = r_term,
    canonical_terminal = c_term,
    conflicts = conflicts,
    ignored_canonicals = ignored_canonicals
  )
}
