#' @title Structural Smoothing of Empirical Page Transitions
#' @description Shrinks sparse empirical transition shares toward the
#'   crawl-graph link structure, so that no valid crawled link is assigned a
#'   probability of exactly zero. Observed page-transition data (e.g. from
#'   [ga4_page_transitions()]) is sparse: a low-traffic but real internal link
#'   may simply never have been traversed in the measured window, which leaves
#'   its raw empirical share at zero and destabilises the stationary PageRank
#'   vector. This helper combines the empirical distribution with a structural
#'   prior using a per-source shrinkage weight that grows with the source page's
#'   sample size.
#'
#' @details
#' ## The shrinkage model
#'
#' For each source page `i`, the smoothed transition probability to target `j`
#' is the convex combination
#'
#' \deqn{P(i \to j) = \lambda_i \cdot \mathrm{emp}(i \to j) +
#'   (1 - \lambda_i) \cdot \mathrm{prior}(i \to j)}
#'
#' where `emp(i -> j)` is the empirical share `count(i -> j) / n_i`
#' (`n_i` = total observed out-transitions from `i`), `prior(i -> j)` is the
#' structural prior share (the crawl-graph out-link distribution of `i`), and
#' \eqn{\lambda_i} is the per-source trust placed in the empirical data.
#'
#' ## Sample-size-dependent shrinkage (`lambda_i`)
#'
#' By default \eqn{\lambda_i = n_i / (n_i + k)}, the standard
#' Dirichlet / pseudocount shrinkage rule: it is monotonically increasing in the
#' source-page sample size `n_i` and equals `1/2` at `n_i = k`. Equivalently,
#' the model adds `k` pseudo-observations distributed according to the
#' structural prior, then renormalizes — a Dirichlet prior with concentration
#' `k`. A high-traffic source (large `n_i`) is trusted almost entirely to its
#' own behavior; a barely-sampled source leans on the crawl structure. `k`
#' must be strictly positive: this is precisely what guarantees
#' \eqn{\lambda_i < 1} for any sampled source, hence a non-zero
#' \eqn{(1 - \lambda_i)} weight on every crawled link (see *Guarantees*).
#'
#' ## Per-source special cases
#'
#' Sources are matched between the two inputs and resolved as follows:
#' - **No empirical data** (`n_i = 0`; crawled link absent from behavioral
#'   data): \eqn{\lambda_i = 0}, so `P(i -> .)` is the pure structural
#'   prior. The crawl link still receives mass.
#' - **No structural prior** (observed transition whose source has no crawled
#'   out-links): \eqn{\lambda_i = 1}, so `P(i -> .)` is the pure empirical
#'   distribution — there is nothing to shrink toward.
#' - **Insufficient support** (`0 < n_i < min_support`):
#'   \eqn{\lambda_i = 0}. The
#'   empirical sample is treated as too small to trust, and the source falls
#'   back to its structural prior (when one exists).
#' - **Otherwise**: \eqn{\lambda_i = n_i / (n_i + k)} (or `lambda_fn(n_i)`).
#'
#' ## Edge universe
#'
#' The output covers the **union** of empirical and structural out-edges per
#' source, with an `origin` column flagging each as `"both"`,
#' `"empirical_only"` (an observed transition absent from the crawl graph), or
#' `"structural_only"` (a crawled link never observed behaviorally). Edges
#' whose smoothed probability is exactly zero — only possible for an
#' `empirical_only` edge from a below-`min_support` source — are dropped, since
#' they carry no transition mass.
#'
#' ## Time decay and segmentation
#'
#' Time decay and device / template / channel segmentation are handled
#' **upstream** by shaping the count input, keeping this function focused on the
#' shrinkage itself. For time decay, supply decayed (fractional) counts in
#' `count_col` — the `n_i` totals and \eqn{\lambda_i} then reflect effective
#' sample
#' size, and `count_col` need not be integer. For segmentation, partition the
#' empirical counts by segment and call `smooth_transitions()` once per segment
#' (optionally against a segment-specific structural prior), then combine the
#' results.
#'
#' ## Guarantees
#'
#' For any `k > 0`, every crawled link present in `structural_df` receives a
#' strictly positive smoothed probability: such an edge has
#' `prior(i -> j) > 0`, and either \eqn{\lambda_i = 0} (pure prior) or
#' \eqn{\lambda_i < 1} (since `n_i / (n_i + k) < 1`), so the
#' \eqn{(1 - \lambda_i)}
#' weight on the prior is positive. Within each source, the returned
#' probabilities sum to 1.
#'
#' @param empirical_df A data frame of observed transitions: a `from`/`to` edge
#'   list with a numeric count column. Typically the output of
#'   [ga4_page_transitions()] (or [aggregate_edges()] over behavioral counts).
#'   Duplicate `from`/`to` rows are summed.
#' @param structural_df A data frame of crawl-graph links forming the structural
#'   prior: a `from`/`to` edge list, optionally with a numeric structural-weight
#'   column. Duplicate `from`/`to` rows are summed (so repeated link instances
#'   raise the prior weight). When `structural_weight_col` is `NULL`, each row
#'   contributes weight 1, i.e. a uniform prior over a source's crawled links.
#' @param k Positive numeric. The pseudocount / Dirichlet concentration
#'   controlling shrinkage strength in
#'   \eqn{\lambda_i = n_i / (n_i + k)}. Larger `k`
#'   pulls under-sampled sources more strongly toward the structural prior.
#'   Must be `> 0`. Default `5`. Ignored for a source if `lambda_fn` is given.
#' @param min_support Non-negative numeric. Sources with `0 < n_i < min_support`
#'   are treated as having insufficient empirical support and fall back fully to
#'   the structural prior (\eqn{\lambda_i = 0}). Default `0` (no minimum).
#' @param lambda_fn Optional function mapping a source's sample size `n_i` (a
#'   single numeric) to a shrinkage weight in `[0, 1]`. Overrides the default
#'   `n_i / (n_i + k)` rule for sources that have both empirical data and a
#'   structural prior and meet `min_support`. Must return values `< 1` to
#'   preserve the non-zero-crawled-link guarantee. Default `NULL`.
#' @param count_col Name of the numeric empirical-count column in
#'   `empirical_df`. Default `"n"` (the [ga4_page_transitions()] default).
#' @param structural_weight_col Name of an optional numeric structural-weight
#'   column in `structural_df`, or `NULL` (default) for a uniform prior.
#' @param from_col,to_col Names of the source / target columns, shared by both
#'   inputs and the output. Defaults `"from"` / `"to"`.
#' @param prob_col Name of the output smoothed-probability column. Default
#'   `"transition_probability"`. Pass this to `pagerank(weight_col = ...)`.
#'
#' @return A data frame with one row per surviving source-target edge (the
#'   per-source union of empirical and structural edges, zero-probability edges
#'   dropped), ordered by `from` then `to`, carrying:
#'   \describe{
#'     \item{`from_col`, `to_col`}{the edge endpoints (character).}
#'     \item{`prob_col`}{the smoothed transition probability; sums to 1 within
#'       each source.}
#'     \item{`empirical_count`}{the observed count for this edge (0 if the edge
#'       is `structural_only`).}
#'     \item{`empirical_share`}{`count / n_i`, the raw empirical share (0 if the
#'       source had no empirical data for this edge).}
#'     \item{`structural_prior`}{the structural prior share for this edge (0 if
#'       absent from the crawl graph).}
#'     \item{`support`}{`n_i`, the source page's total empirical out-count.}
#'     \item{`lambda`}{the per-source shrinkage weight applied.}
#'     \item{`origin`}{`"both"`, `"empirical_only"`, or `"structural_only"`.}
#'   }
#'
#' @seealso [ga4_page_transitions()] for the empirical input,
#'   [aggregate_edges()] for collapsing behavioral counts, and [pagerank()]
#'   for consuming the smoothed probabilities via `weight_col = prob_col`.
#'
#' @export
#' @examples
#' # Sparse behavioral data: only A->B was ever observed.
#' empirical <- data.frame(
#'   from = c("A", "A"),
#'   to = c("B", "C"),
#'   n = c(8, 0)
#' )[1, ]
#' # Crawl graph: A links to both B and C.
#' structural <- data.frame(
#'   from = c("A", "A"),
#'   to = c("B", "C")
#' )
#' smoothed <- smooth_transitions(empirical, structural, k = 5)
#' smoothed
#' # A->C keeps a non-zero probability despite never being observed.
#'
#' # Feed to pagerank() as a smoothed behavioral transition model:
#' # pagerank(smoothed, weight_col = "transition_probability",
#' #          clean_edge_urls = FALSE)
smooth_transitions <- function(empirical_df,
                               structural_df,
                               k = 5,
                               min_support = 0,
                               lambda_fn = NULL,
                               count_col = "n",
                               structural_weight_col = NULL,
                               from_col = "from",
                               to_col = "to",
                               prob_col = "transition_probability") {
  # --- Validation ---
  .validate_smooth_frames(empirical_df, structural_df)
  .validate_smooth_char_args(count_col, from_col, to_col, prob_col)
  .validate_smooth_shrinkage(k, min_support, lambda_fn)
  .validate_smooth_weight_col_arg(structural_weight_col)
  .validate_smooth_columns(
    empirical_df, structural_df, from_col, to_col, count_col,
    structural_weight_col
  )

  # --- Normalise both inputs to from/to/value triples (NA-dropped, summed) ---
  emp <- .collapse_edge_values(
    empirical_df, from_col, to_col, empirical_df[[count_col]]
  )
  if (is.null(structural_weight_col)) {
    struct_vals <- rep(1, nrow(structural_df))
  } else {
    struct_vals <- structural_df[[structural_weight_col]]
  }
  struct <- .collapse_edge_values(
    structural_df, from_col, to_col, struct_vals
  )

  # Structural weights must be positive to act as a prior; non-positive or NA
  # weights carry no prior mass and are dropped.
  struct <- struct[is.finite(struct$value) & struct$value > 0, , drop = FALSE]

  if (nrow(emp) == 0 && nrow(struct) == 0) {
    return(.empty_smooth_result(from_col, to_col, prob_col))
  }

  # --- Per-source smoothing over the union of sources ---
  sources <- unique(c(emp$from, struct$from))

  # Explicit lambda kept for readability over positional `...` forwarding.
  pieces <- lapply(sources, function(src) { # nolint: unnecessary_lambda_linter.
    .smooth_one_source(src, emp, struct, min_support, lambda_fn, k)
  })

  out <- do.call(rbind, pieces)

  # Drop edges that carry no transition mass (only possible for an
  # empirical_only edge from a below-min_support source).
  out <- out[out$prob > 0, , drop = FALSE]

  # Stable output order.
  out <- out[order(out$from, out$to), , drop = FALSE]
  row.names(out) <- NULL

  names(out) <- c(
    from_col, to_col, prob_col, "empirical_count", "empirical_share",
    "structural_prior", "support", "lambda", "origin"
  )
  out
}

#' Validate that both smoothing inputs are data frames.
#' @keywords internal
#' @noRd
.validate_smooth_frames <- function(empirical_df, structural_df) {
  if (!is.data.frame(empirical_df)) {
    stop("`empirical_df` must be a data frame.", call. = FALSE)
  }
  if (!is.data.frame(structural_df)) {
    stop("`structural_df` must be a data frame.", call. = FALSE)
  }
  invisible(NULL)
}

#' Validate that the column-name arguments are single non-NA strings.
#' @keywords internal
#' @noRd
.validate_smooth_char_args <- function(count_col, from_col, to_col, prob_col) {
  char_args <- list(
    count_col = count_col, from_col = from_col,
    to_col = to_col, prob_col = prob_col
  )
  for (nm in names(char_args)) {
    val <- char_args[[nm]]
    if (!is.character(val) || length(val) != 1 || is.na(val)) {
      stop("`", nm, "` must be a single non-NA character string.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Validate the shrinkage-strength arguments (`k`, `min_support`, `lambda_fn`).
#' @keywords internal
#' @noRd
.validate_smooth_shrinkage <- function(k, min_support, lambda_fn) {
  .validate_positive_k(k)
  .validate_min_support(min_support)
  if (!is.null(lambda_fn)) {
    if (!is.function(lambda_fn)) {
      stop("`lambda_fn` must be a function or NULL.", call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Validate that `k` is a single strictly-positive number.
#' @keywords internal
#' @noRd
.validate_positive_k <- function(k) {
  bad <- !is.numeric(k) || length(k) != 1
  if (bad || is.na(k) || k <= 0) {
    stop(
      "`k` must be a single positive number. A positive `k` is what ",
      "guarantees every crawled link keeps non-zero probability.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Validate that `min_support` is a single non-negative number.
#' @keywords internal
#' @noRd
.validate_min_support <- function(min_support) {
  bad <- !is.numeric(min_support) || length(min_support) != 1
  if (bad || is.na(min_support) || min_support < 0) {
    stop("`min_support` must be a single non-negative number.", call. = FALSE)
  }
  invisible(NULL)
}

#' Validate that `structural_weight_col` is a single non-NA string or NULL.
#' @keywords internal
#' @noRd
.validate_smooth_weight_col_arg <- function(structural_weight_col) {
  if (!is.null(structural_weight_col) &&
        (!is.character(structural_weight_col) ||
           length(structural_weight_col) != 1 ||
           is.na(structural_weight_col))) {
    stop(
      "`structural_weight_col` must be a single non-NA character string ",
      "or NULL.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Validate that required columns are present and typed correctly.
#' @keywords internal
#' @noRd
.validate_smooth_columns <- function(empirical_df, structural_df, from_col,
                                     to_col, count_col,
                                     structural_weight_col) {
  emp_required <- c(from_col, to_col, count_col)
  emp_missing <- emp_required[!emp_required %in% names(empirical_df)]
  if (length(emp_missing) > 0) {
    stop(
      "`empirical_df` is missing required column(s): ",
      toString(emp_missing), ".",
      call. = FALSE
    )
  }
  if (!is.numeric(empirical_df[[count_col]])) {
    stop("`count_col` (", count_col, ") must be numeric.", call. = FALSE)
  }

  struct_required <- c(from_col, to_col)
  if (!is.null(structural_weight_col)) {
    struct_required <- c(struct_required, structural_weight_col)
  }
  struct_missing <- struct_required[!struct_required %in% names(structural_df)]
  if (length(struct_missing) > 0) {
    stop(
      "`structural_df` is missing required column(s): ",
      toString(struct_missing), ".",
      call. = FALSE
    )
  }
  if (!is.null(structural_weight_col) &&
        !is.numeric(structural_df[[structural_weight_col]])) {
    stop(
      "`structural_weight_col` (", structural_weight_col,
      ") must be numeric.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Build the zero-row result data frame with the requested column names.
#' @keywords internal
#' @noRd
.empty_smooth_result <- function(from_col, to_col, prob_col) {
  out <- data.frame(
    a = character(0), b = character(0),
    p = numeric(0), ec = numeric(0), es = numeric(0),
    sp = numeric(0), su = numeric(0), lam = numeric(0),
    origin = character(0)
  )
  names(out) <- c(
    from_col, to_col, prob_col, "empirical_count", "empirical_share",
    "structural_prior", "support", "lambda", "origin"
  )
  out
}

#' Validate a `lambda_fn` return value is a single number in [0, 1].
#' @keywords internal
#' @noRd
.validate_lambda_value <- function(lambda, src) {
  bad <- !is.numeric(lambda) || length(lambda) != 1
  if (bad || is.na(lambda) || lambda < 0 || lambda > 1) {
    stop(
      "`lambda_fn` must return a single value in [0, 1]; got an invalid ",
      "result for source '", src, "'.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Resolve the per-source shrinkage weight lambda.
#'
#' Applies the special cases (no empirical data, no prior, insufficient
#' support), then a caller-supplied `lambda_fn` if given, else the default
#' `n_i / (n_i + k)` rule.
#' @keywords internal
#' @noRd
.compute_source_lambda <- function(n_i, has_emp, has_struct, min_support,
                                   lambda_fn, k, src) {
  if (!has_emp) {
    return(0)
  }
  if (!has_struct) {
    return(1)
  }
  if (n_i < min_support) {
    return(0)
  }
  if (!is.null(lambda_fn)) {
    lambda <- lambda_fn(n_i)
    .validate_lambda_value(lambda, src)
    return(lambda)
  }
  n_i / (n_i + k)
}

#' Sum an aligned value vector per requested target.
#'
#' Returns a numeric vector aligned to `targets`; a target with no matching
#' rows contributes 0.
#' @keywords internal
#' @noRd
.sum_by_target <- function(targets, to_vec, value_vec) {
  vapply(targets, function(t) {
    v <- value_vec[to_vec == t]
    if (length(v) == 0) 0 else sum(v)
  }, numeric(1))
}

#' Compute the smoothed transition rows for a single source page.
#' @keywords internal
#' @noRd
.smooth_one_source <- function(src, emp, struct, min_support, lambda_fn, k) {
  e <- emp[emp$from == src, , drop = FALSE]
  s <- struct[struct$from == src, , drop = FALSE]

  n_i <- sum(e$value)
  has_emp <- n_i > 0
  struct_total <- sum(s$value)
  has_struct <- struct_total > 0

  lambda <- .compute_source_lambda(
    n_i, has_emp, has_struct, min_support, lambda_fn, k, src
  )

  # Union of targets for this source.
  targets <- unique(c(e$to, s$to))

  emp_count <- .sum_by_target(targets, e$to, e$value)
  emp_share <- if (has_emp) {
    emp_count / n_i
  } else {
    rep(0, length(targets))
  }
  prior <- if (has_struct) {
    .sum_by_target(targets, s$to, s$value) / struct_total
  } else {
    rep(0, length(targets))
  }

  prob <- lambda * emp_share + (1 - lambda) * prior

  in_emp <- targets %in% e$to
  in_struct <- targets %in% s$to
  origin <- rep("structural_only", length(in_emp))
  origin[in_emp] <- "empirical_only"
  origin[in_emp & in_struct] <- "both"

  data.frame(
    from = rep(src, length(targets)),
    to = targets,
    prob = prob,
    empirical_count = emp_count,
    empirical_share = emp_share,
    structural_prior = prior,
    support = rep(n_i, length(targets)),
    lambda = rep(lambda, length(targets)),
    origin = origin
  )
}

#' Collapse a from/to edge list with an aligned value vector to summed triples.
#'
#' Drops rows with an NA `from` or `to`, coerces endpoints to character, and
#' sums the supplied value per unique `from`/`to` pair. Returns a data frame
#' with columns `from`, `to`, `value`.
#' @keywords internal
#' @noRd
.collapse_edge_values <- function(df, from_col, to_col, value) {
  if (nrow(df) == 0) {
    return(data.frame(
      from = character(0), to = character(0), value = numeric(0)
    ))
  }
  from <- as.character(df[[from_col]])
  to <- as.character(df[[to_col]])
  value <- as.numeric(value)

  keep <- !is.na(from) & !is.na(to)
  from <- from[keep]
  to <- to[keep]
  value <- value[keep]
  # NA values contribute nothing to a sum.
  value[is.na(value)] <- 0

  if (length(from) == 0) {
    return(data.frame(
      from = character(0), to = character(0), value = numeric(0)
    ))
  }

  agg <- stats::aggregate(
    list(value = value),
    by = list(from = from, to = to),
    FUN = sum
  )
  agg$from <- as.character(agg$from)
  agg$to <- as.character(agg$to)
  agg
}
