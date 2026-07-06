#' @title Report how stable a PageRank ranking is across damping factors
#' @description Sweeps [pagerank()] over a grid of damping factors with
#'   [damping_sensitivity()] and compares each \eqn{\alpha}'s ranking against a
#'   reference \eqn{\alpha} with [compare_pagerank()], returning a one-row-per-
#'   \eqn{\alpha} stability summary. It answers the open question flagged in the
#'   "Damping factor" section of [pagerank()]: on *your* graph, how much does
#'   the ranking actually move as \eqn{\alpha} varies?
#'
#' @details
#' A Spearman rank correlation near 1 across the whole grid means the choice of
#' damping factor is immaterial for this graph — the conventional `0.85` is as
#' good as any nearby value. A low correlation, or a top-\eqn{k} overlap well
#' below 1, flags a graph whose ranking is genuinely \eqn{\alpha}-sensitive and
#' worth investigating before trusting any single solve.
#'
#' This is a thin orchestration layer: it performs no PageRank math of its own,
#' delegating the solves to [damping_sensitivity()] and the rank-comparison
#' statistics to [compare_pagerank()]. The `reference` factor is always included
#' in the sweep (even if absent from `alphas`) so it can serve as the comparison
#' baseline; its own row is a sanity anchor (`spearman_rho = 1`,
#' `mean_abs_delta = 0`, `top_k_overlap = 1`).
#'
#' @inheritParams damping_sensitivity
#' @param reference The baseline damping factor every other \eqn{\alpha} is
#'   compared against. A single number strictly between 0 and 1, default `0.85`.
#'   Included in the sweep automatically if not already in `alphas`.
#' @param top_k Size of the top-scoring set used for the `top_k_overlap`
#'   churn metric. Positive integer, default `10`.
#' @param ... Additional arguments forwarded to [damping_sensitivity()] and on
#'   to [pagerank()] (e.g. `redirects_df`, `weight_col`, `algo`, `prior_df`).
#'   Passing `damping` is an error, since `alphas` drives the damping factor.
#'
#' @return A data frame with one row per swept \eqn{\alpha} (ascending), with
#'   columns:
#'   \describe{
#'     \item{`alpha`}{The damping factor.}
#'     \item{`spearman_rho`}{Spearman rank correlation of this \eqn{\alpha}'s
#'       ranking against the reference, on their common nodes (`NA` if fewer
#'       than 3 common nodes).}
#'     \item{`mean_abs_delta`}{Mean absolute score difference vs the reference
#'       on common nodes.}
#'     \item{`top_k_overlap`}{Fraction in `[0, 1]` of the reference's top-`k`
#'       pages that are also in this \eqn{\alpha}'s top-`k` (1 = identical top
#'       set). The effective `k` shrinks to the node count on small graphs.}
#'     \item{`nodes_gained`, `nodes_lost`}{Nodes present at this \eqn{\alpha}
#'       but not the reference, and vice versa. Normally 0: varying
#'       \eqn{\alpha} changes scores, not the node set.}
#'     \item{`algo`, `iters`, `iters_estimate`, `residual`, `tol`, `converged`,
#'       `n_nodes`}{The per-\eqn{\alpha} convergence metadata carried over from
#'       [damping_sensitivity()].}
#'   }
#'
#'   The full per-(URL, \eqn{\alpha}) sensitivity frame from
#'   [damping_sensitivity()] is attached as the `"sensitivity"` attribute, and
#'   the `reference` and `top_k` used are attached as same-named attributes.
#'
#' @seealso [damping_sensitivity()], [compare_pagerank()],
#'   [pagerank()] (the "Damping factor" section)
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "A", "D"),
#'   to = c("B", "C", "A", "C", "A")
#' )
#' stab <- pagerank_stability(edges, clean_edge_urls = FALSE)
#' print(stab)
#'
#' # Drill into the per-(url, alpha) scores behind the summary.
#' head(attr(stab, "sensitivity"))
pagerank_stability <- function(edge_list_df,
                               alphas = c(0.75, 0.80, 0.85, 0.90, 0.95),
                               reference = 0.85,
                               top_k = 10,
                               ...) {
  .check_stability_args(edge_list_df, reference, top_k)
  top_k <- as.integer(top_k)

  # The reference must be one of the solved alphas so it can serve as baseline.
  # `alphas` itself is validated inside damping_sensitivity().
  swept <- sort(unique(c(alphas, reference)))
  sens <- damping_sensitivity(edge_list_df, alphas = swept, ...)
  out <- attr(sens, "convergence") # one row per swept alpha, ascending

  ref_rows <- sens[sens$alpha == reference, , drop = FALSE] # score-descending
  ref_frame <- data.frame(
    node_name = ref_rows$url,
    pagerank = ref_rows$score
  )

  out$spearman_rho <- NA_real_
  out$mean_abs_delta <- NA_real_
  out$top_k_overlap <- NA_real_
  out$nodes_gained <- NA_integer_
  out$nodes_lost <- NA_integer_

  for (i in seq_len(nrow(out))) {
    out <- .stability_fill_row(out, i, sens, ref_rows, ref_frame,
                               reference, top_k)
  }

  .finalize_stability_out(out, sens, reference, top_k)
}

#' Validate `pagerank_stability()` scalar arguments.
#'
#' Guards are delegated to sequential-`if` predicates (rather than `||`-chains)
#' so each check stays cheap for cyclomatic-complexity budgeting; validation
#' order and error text are preserved verbatim.
#' @return Invisibly `NULL`; called for its side effect of `stop()`ing.
#' @noRd
.check_stability_args <- function(edge_list_df, reference, top_k) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!.is_valid_reference(reference)) {
    stop(
      "`reference` must be a single number strictly between 0 and 1.",
      call. = FALSE
    )
  }
  if (!.is_valid_top_k(top_k)) {
    stop("`top_k` must be a single positive integer.", call. = FALSE)
  }
  invisible(NULL)
}

#' Is `reference` a single number strictly in (0, 1)?
#'
#' Sequential guards (returning `FALSE` on the first failure) keep each test
#' cheap for cyclomatic complexity while preserving the original condition set.
#' @return `TRUE` if valid, `FALSE` otherwise.
#' @noRd
.is_valid_reference <- function(reference) {
  if (!is.numeric(reference)) return(FALSE)
  if (length(reference) != 1) return(FALSE)
  if (is.na(reference)) return(FALSE)
  if (reference <= 0) return(FALSE)
  if (reference >= 1) return(FALSE)
  TRUE
}

#' Is `top_k` a single positive number?
#'
#' Sequential guards keep each test cheap for cyclomatic complexity while
#' preserving the original condition set.
#' @return `TRUE` if valid, `FALSE` otherwise.
#' @noRd
.is_valid_top_k <- function(top_k) {
  if (!is.numeric(top_k)) return(FALSE)
  if (length(top_k) != 1) return(FALSE)
  if (is.na(top_k)) return(FALSE)
  if (top_k < 1) return(FALSE)
  TRUE
}

#' Fill the stability metrics for a single row (one swept alpha).
#'
#' @return The `out` data frame with row `i` populated.
#' @noRd
.stability_fill_row <- function(out, i, sens, ref_rows, ref_frame,
                                reference, top_k) {
  a <- out$alpha[i]
  rows_a <- sens[sens$alpha == a, , drop = FALSE] # score-descending
  if (nrow(rows_a) == 0) return(out)

  k_eff <- min(top_k, nrow(ref_rows), nrow(rows_a))

  if (isTRUE(all.equal(a, reference))) {
    out$spearman_rho[i] <- 1
    out$mean_abs_delta[i] <- 0
    out$nodes_gained[i] <- 0L
    out$nodes_lost[i] <- 0L
    out$top_k_overlap[i] <- if (k_eff > 0) 1 else NA_real_
    return(out)
  }

  frame_a <- data.frame(
    node_name = rows_a$url,
    pagerank = rows_a$score
  )
  cmp <- attr(compare_pagerank(ref_frame, frame_a), "summary")
  out$spearman_rho[i] <- cmp$spearman_rho
  out$mean_abs_delta[i] <- cmp$mean_abs_delta
  out$nodes_gained[i] <- cmp$nodes_gained
  out$nodes_lost[i] <- cmp$nodes_lost

  if (k_eff > 0) {
    top_ref <- ref_rows$url[seq_len(k_eff)]
    top_a <- rows_a$url[seq_len(k_eff)]
    out$top_k_overlap[i] <- length(intersect(top_ref, top_a)) / k_eff
  }
  out
}

#' Reorder columns and attach the `pagerank_stability()` result attributes.
#'
#' @return The finalized one-row-per-alpha stability data frame.
#' @noRd
.finalize_stability_out <- function(out, sens, reference, top_k) {
  # Lead with the stability metrics, then the convergence metadata.
  conv_cols <- c(
    "algo", "iters", "iters_estimate", "residual", "tol", "converged",
    "n_nodes"
  )
  out <- out[, c(
    "alpha", "spearman_rho", "mean_abs_delta", "top_k_overlap",
    "nodes_gained", "nodes_lost", conv_cols
  ), drop = FALSE]
  row.names(out) <- NULL

  attr(sens, "convergence") <- NULL # lives on `out` now; avoid duplication
  attr(out, "sensitivity") <- sens
  attr(out, "reference") <- reference
  attr(out, "top_k") <- top_k
  out
}
