#' @title PageRank convergence diagnostic object
#' @name pagerank_convergence
#' @description Records how the PageRank stationary vector was obtained: which
#'   solver ran, how many iterations it used (when that is observable), and how
#'   well the returned vector satisfies the PageRank fixed-point equation. It is
#'   attached to the result of [compute_pagerank()] / [pagerank()] as the
#'   `"convergence"` attribute and is the companion to the
#'   [transition_audit][transition_audit] provenance record.
#'
#' @details
#' ## Why the solver matters
#'
#' `igraph::page_rank()` offers two back-ends:
#' \describe{
#'   \item{`"prpack"`}{(default) A direct/sparse solver (the PRPACK library).
#'     It is fast and exact to machine precision, but it is **not** iterative in
#'     any way it exposes: there is no iteration count and no tolerance knob, so
#'     `iters` is reported as `NA` and `eps` / `niter` have no effect.}
#'   \item{`"arpack"`}{An iterative eigensolver. It honors a tolerance and a
#'     maximum iteration count and reports the iterations it actually used and
#'     whether it converged. This is the only back-end on which `eps` and
#'     `niter` take effect; supplying either to [compute_pagerank()] /
#'     [pagerank()] therefore transparently selects it.}
#' }
#'
#' The old `igraph` `eps` / `niter` arguments to `page_rank()` were removed in
#' modern `igraph` (2.x); this package re-introduces them as friendly aliases
#' that map onto the ARPACK `options$tol` / `options$maxiter` controls.
#'
#' ## The residual is solver-independent
#'
#' Regardless of back-end, `residual` is computed here directly from the
#' returned vector as the L1 norm of one PageRank operator application,
#' \eqn{\|G x - x\|_1}, where \eqn{G} is the Google operator implied by the
#' scored graph, the damping factor, and the teleport vector (uniform, or the
#' supplied TIPR prior). This is the standard Kamvar, Haveliwala & Golub (2004)
#' stopping criterion, evaluated *post hoc* so it is a genuine, comparable
#' quality check across both solvers (a converged solution sits near machine
#' precision). `tol_met` reports whether `residual` is at or below `tol` (the
#' supplied `eps`, or the conventional default of `1e-3` when `eps` is `NULL`),
#' additionally requiring `info == 0` for the ARPACK back-end.
#'
#' ## Iteration-count rule of thumb
#'
#' Power-iteration PageRank needs about
#' \eqn{\log_{10}(\tau) / \log_{10}(\alpha)}
#' iterations to reach residual \eqn{\tau} at damping \eqn{\alpha} (Langville &
#' Meyer, 2004). At \eqn{\tau = 10^{-8}}: \eqn{\alpha = 0.85} needs ~114,
#' \eqn{\alpha = 0.95} ~362, and \eqn{\alpha = 0.99} ~1,833 iterations — so a
#' high damping factor degrades convergence sharply. ARPACK is not plain power
#' iteration, so its reported `iters` is typically far lower, but the same
#' qualitative warning applies: raise `niter` if you raise the damping factor
#' toward 1.
#'
#' @seealso [compute_pagerank()], [pagerank()], [transition_audit]
NULL

#' Construct a pagerank_convergence object
#'
#' Internal constructor used by [compute_pagerank()].
#'
#' @param algo Character, the solver used: `"prpack"` or `"arpack"`.
#' @param iters Integer or `NA_integer_`, iterations used (ARPACK only).
#' @param residual Numeric, the L1 residual \eqn{\|G x - x\|_1} of the returned
#'   vector.
#' @param tol Numeric, the tolerance against which `tol_met` is judged.
#' @param tol_met Logical, whether `residual <= tol` (and, for ARPACK,
#'   `info == 0`).
#' @param info Integer or `NA_integer_`, the ARPACK return code (`0` = success).
#' @param eps Numeric or `NA_real_`, the user-supplied tolerance (ARPACK `tol`).
#' @param niter Integer or `NA_integer_`, the user-supplied iteration cap
#'   (ARPACK `maxiter`).
#' @return An object of class `"pagerank_convergence"`.
#' @noRd
new_pagerank_convergence <- function(algo = "prpack",
                                     iters = NA_integer_,
                                     residual = NA_real_,
                                     tol = 1e-3,
                                     tol_met = NA,
                                     info = NA_integer_,
                                     eps = NA_real_,
                                     niter = NA_integer_) {
  conv <- list(
    algo = algo,
    iters = iters,
    residual = residual,
    tol = tol,
    tol_met = tol_met,
    info = info,
    eps = eps,
    niter = niter
  )
  class(conv) <- "pagerank_convergence"
  conv
}

#' Print a pagerank_convergence object
#'
#' @param x A `pagerank_convergence` object.
#' @param ... Unused; for S3 compatibility.
#' @return `x`, invisibly.
#' @export
print.pagerank_convergence <- function(x, ...) {
  cat("=== PageRank Convergence ===\n\n")
  cat("  Solver:    ", x$algo,
    if (identical(x$algo, "prpack")) "(direct; no iteration count)" else "",
    "\n"
  )
  cat(
    "  Iterations:",
    if (is.na(x$iters)) "NA (not exposed by this solver)" else x$iters,
    "\n"
  )
  cat(
    "  Residual:  ",
    if (is.na(x$residual)) {
      "NA"
    } else {
      formatC(x$residual, format = "e", digits = 3)
    },
    "(L1 |Gx - x|)\n"
  )
  cat(
    "  Tolerance: ",
    formatC(x$tol, format = "e", digits = 3),
    if (!is.na(x$eps)) "(from eps)" else "(default)",
    "\n"
  )
  cat(
    "  Converged: ",
    if (is.na(x$tol_met)) "NA" else if (isTRUE(x$tol_met)) "yes" else "NO",
    "\n"
  )
  if (identical(x$algo, "arpack") && !is.na(x$info) && x$info != 0L) {
    cat("  ARPACK info:", x$info, "(non-zero: did not fully converge)\n")
  }
  invisible(x)
}
