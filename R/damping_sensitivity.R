#' @title Sweep PageRank across a range of damping factors
#' @description Runs [pagerank()] at each damping factor \eqn{\alpha} in
#'   `alphas` and returns a tidy data frame of per-URL scores alongside the
#'   convergence metadata for each solve. This makes the sensitivity of the
#'   ranking to \eqn{\alpha} directly inspectable on *your* graph, rather than
#'   relying on the field default of `0.85` (see the "Damping factor" section of
#'   [pagerank()] for why that default is only an empirical convention).
#'
#' @details
#' The helper is the empirical companion to the closed-form
#' \eqn{\alpha}-derivative analysis of Boldi, Santini & Vigna (\emph{PageRank as
#' a Function of the Damping Factor}, WWW 2005): instead of differentiating the
#' PageRank vector with respect to \eqn{\alpha} analytically, it samples the
#' vector at a grid of \eqn{\alpha} values so you can see how much each page's
#' score (and the overall ranking) actually moves. Pair it with
#' [compare_pagerank()] to quantify the rank churn between any two \eqn{\alpha}
#' values.
#'
#' Each row also carries the convergence metadata for that \eqn{\alpha}'s solve.
#' The empirical `iters` count is only reported by the ARPACK solver; under the
#' default PRPACK direct solver it is `NA` (PRPACK exposes no iteration count).
#' To populate it, forward `algo = "arpack"` (or an `eps` / `niter` control)
#' through `...`. The solver-independent `iters_estimate` column is always
#' populated: it is the power-iteration rule of thumb
#' \eqn{\lceil \log_{10}(\tau) / \log_{10}(\alpha) \rceil} (Langville & Meyer,
#' 2004) at the convergence tolerance \eqn{\tau}, and shows how the required
#' iteration count climbs as \eqn{\alpha} approaches 1 regardless of solver.
#'
#' @param edge_list_df A data frame representing the edge list, passed to every
#'   [pagerank()] call. (Named for consistency with the rest of the package;
#'   it is an edge list, not a constructed graph object.)
#' @param alphas Numeric vector of damping factors to sweep, each strictly
#'   between 0 and 1. Default `c(0.75, 0.80, 0.85, 0.90, 0.95)`. Duplicate
#'   values are dropped.
#' @param ... Additional arguments forwarded to [pagerank()] (e.g.
#'   `redirects_df`, `weight_col`, `algo`, `eps`, `niter`, `prior_df`). Passing
#'   `damping` here is an error, since `alphas` is what drives the damping
#'   factor.
#'
#' @return A tidy data frame with one row per (URL, \eqn{\alpha}) pair, sorted
#'   by `alpha` ascending then `score` descending, with columns:
#'   \describe{
#'     \item{`url`}{Node / page identifier.}
#'     \item{`alpha`}{The damping factor used for this solve.}
#'     \item{`score`}{The page's PageRank score at this `alpha`.}
#'     \item{`iters`}{Iterations the solver used (ARPACK only; `NA` under
#'       PRPACK).}
#'     \item{`iters_estimate`}{Power-iteration iteration-count estimate at the
#'       convergence tolerance (solver-independent).}
#'     \item{`residual`}{Post-hoc L1 residual \eqn{\|G x - x\|_1} of the solve.}
#'     \item{`converged`}{Whether the residual met the tolerance.}
#'   }
#'
#'   A `"convergence"` attribute is attached: a compact one-row-per-`alpha` data
#'   frame (`alpha`, `algo`, `iters`, `iters_estimate`, `residual`, `tol`,
#'   `converged`, `n_nodes`) summarizing each solve.
#'
#' @seealso [pagerank()] (the "Damping factor" section), [pagerank_convergence],
#'   [compare_pagerank()], [pagerank_grid()]
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "A", "D"),
#'   to = c("B", "C", "A", "C", "A")
#' )
#' sens <- damping_sensitivity(edges, clean_edge_urls = FALSE)
#' print(sens)
#' attr(sens, "convergence")
#'
#' # Populate the empirical iteration count by using the ARPACK solver.
#' sens_ar <- suppressMessages(
#'   damping_sensitivity(edges, algo = "arpack", clean_edge_urls = FALSE)
#' )
#' attr(sens_ar, "convergence")
damping_sensitivity <- function(edge_list_df,
                                alphas = c(0.75, 0.80, 0.85, 0.90, 0.95),
                                ...) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(alphas) || length(alphas) == 0 || anyNA(alphas)) {
    stop(
      "`alphas` must be a non-empty numeric vector with no missing values.",
      call. = FALSE
    )
  }
  if (any(alphas <= 0 | alphas >= 1)) {
    stop(
      "`alphas` must lie strictly between 0 and 1.",
      call. = FALSE
    )
  }
  dots <- list(...)
  if ("damping" %in% names(dots)) {
    stop(
      "Do not pass `damping` to `damping_sensitivity()`; `alphas` drives the ",
      "damping factor.",
      call. = FALSE
    )
  }

  # Sweep in ascending order so both the result and the summary attribute are
  # ordered by alpha (documented contract); duplicates collapse to one solve.
  alphas <- sort(unique(alphas))

  empty_cols <- c(
    "url", "alpha", "score", "iters", "iters_estimate", "residual", "converged"
  )

  per_alpha <- vector("list", length(alphas))
  summary_rows <- vector("list", length(alphas))

  for (i in seq_along(alphas)) {
    alpha <- alphas[i]
    res <- do.call(pagerank, c(list(edge_list_df, damping = alpha), dots))
    conv <- attr(res, "convergence")

    # Convergence attribute is absent for empty graphs (no nodes scored).
    tol <- if (is.null(conv)) NA_real_ else conv$tol
    iters <- if (is.null(conv)) NA_integer_ else conv$iters
    residual <- if (is.null(conv)) NA_real_ else conv$residual
    converged <- if (is.null(conv)) NA else conv$tol_met
    algo <- if (is.null(conv)) NA_character_ else conv$algo
    iters_estimate <- if (is.na(tol)) {
      NA_real_
    } else {
      ceiling(log10(tol) / log10(alpha))
    }

    if (nrow(res) > 0) {
      node_col <- names(res)[1]
      value_col <- names(res)[2]
      ord <- order(-res[[value_col]], res[[node_col]])
      per_alpha[[i]] <- data.frame(
        url = as.character(res[[node_col]])[ord],
        alpha = alpha,
        score = as.numeric(res[[value_col]])[ord],
        iters = iters,
        iters_estimate = iters_estimate,
        residual = residual,
        converged = converged
      )
    }

    summary_rows[[i]] <- data.frame(
      alpha = alpha,
      algo = algo,
      iters = iters,
      iters_estimate = iters_estimate,
      residual = residual,
      tol = tol,
      converged = converged,
      n_nodes = nrow(res)
    )
  }

  per_alpha <- per_alpha[!vapply(per_alpha, is.null, logical(1))]
  result <- if (length(per_alpha) > 0) {
    out <- do.call(rbind, per_alpha)
    row.names(out) <- NULL
    out
  } else {
    empty <- data.frame(
      url = character(0), alpha = numeric(0), score = numeric(0),
      iters = integer(0), iters_estimate = numeric(0),
      residual = numeric(0), converged = logical(0)
    )
    empty[, empty_cols, drop = FALSE]
  }

  attr(result, "convergence") <- do.call(rbind, summary_rows)
  result
}
