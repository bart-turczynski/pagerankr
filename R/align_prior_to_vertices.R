#' @title Align a Per-URL Prior to a PageRank Vertex Set (TIPR)
#' @description Builds a personalization / teleport vector for
#'   \code{igraph::page_rank(personalized = )} from a per-URL external-authority
#'   prior (e.g. Ahrefs referring domains), aligned to the \emph{final} graph
#'   vertex set. This is the core of TIPR ("topic/true internal PageRank"),
#'   where the random surfer's teleport mass is distributed in proportion to
#'   external authority instead of uniformly.
#'
#'   The prior URLs are expected to already share the vertex namespace (i.e.
#'   canonicalized with the same \code{rurl} settings and folded through the
#'   same redirect map as the edges). [pagerank()] performs that
#'   canonicalization and redirect-fold before calling this function; call it
#'   directly only when your prior URLs already match \code{vertex_names}.
#'
#' @param vertex_names Character vector of the graph's vertex names, in graph
#'   order (typically \code{igraph::V(graph)$name}).
#' @param prior_df A data frame with one row per URL carrying a raw authority
#'   weight (e.g. referring-domain counts). Multiple rows for the same URL are
#'   summed (raw counts are additive — summing happens before any transform).
#' @param prior_url_col Name of the URL column in \code{prior_df}. Default
#'   \code{"url"}.
#' @param prior_weight_col Name of the numeric weight column in \code{prior_df}.
#'   Default \code{"weight"}. \strong{Contract: this must be an additive raw
#'   count.} URLs that coalesce (duplicate rows here, redirect variants folded
#'   upstream by [pagerank()]) are combined by \emph{summation}, which is only
#'   meaningful for counts — quantities that genuinely add when two URLs merge.
#'   This makes the prior \strong{source-agnostic}: referring domains (the
#'   default metric), links-to-target, or dofollow-only referring domains from
#'   Ahrefs are interchangeable swaps, as are equivalent counts from other
#'   sources (SEMrush, GA4 entrances, …). Do \strong{not} pass a calculated
#'   authority \emph{score} (Ahrefs UR / DR, or any 0–100 rating): scores do not
#'   sum across redirect variants (the correct fold would be \code{max}), and a
#'   per-URL score such as UR is itself a PageRank-flavored metric, so using it
#'   as the teleport prior for PageRank is circular.
#' @param transform Character, how to shape the raw authority before it becomes
#'   teleport mass. Passed to [transform_weights()]; one of \code{"none"}
#'   (default, faithful linear share), \code{"log"}, \code{"percentile"},
#'   \code{"minmax"}, \code{"zipf"}, \code{"rank_linear"}. The transform is
#'   applied only to vertices that actually carry authority; vertices with no
#'   prior contribute zero to the authority component.
#' @param alpha Numeric in \code{[0, 1]}, the mixture weight between a uniform
#'   teleport and the authority-weighted teleport:
#'   \code{p = alpha * uniform + (1 - alpha) * authority_share}.
#'   \code{alpha = 0} (default) is pure authority teleport (pages with no
#'   external authority get no teleport mass, though they still receive rank
#'   via inlinks);
#'   \code{alpha = 1} reproduces standard uniform PageRank. \code{alpha} is a
#'   smoothing knob, \emph{not} a dead-node mechanism — isolate/self-loop
#'   handling owns dead nodes.
#' @param exclude_nodes Character vector of vertex names that must receive
#'   \strong{zero} teleport in both components (e.g. the synthetic
#'   \code{"__pr_nofollow_sink__"}). Real pages — including robots-blocked or
#'   404 self-loop nodes — should \emph{not} be excluded.
#' @param verbose Logical, whether to emit coverage diagnostics via
#'   \code{message()} (vertices receiving authority, unmatched prior URLs and
#'   their dropped weight, and the realized uniform mass fraction). Default
#'   \code{TRUE}.
#'
#' @return A numeric vector the same length as \code{vertex_names}, in the same
#'   order, summing to 1 (suitable for
#'   \code{igraph::page_rank(personalized = )}).
#'   Excluded vertices get exactly 0. If the prior matches no vertex and
#'   \code{alpha = 0}, the function falls back to a uniform vector over the
#'   non-excluded vertices and warns.
#'
#' @details
#' Alignment proceeds as: sum raw weights per URL -> match onto
#' \code{vertex_names} (unmatched vertices get raw 0) -> apply \code{transform}
#' to the vertices that carry authority -> normalize to an authority share ->
#' mix with a uniform-over-real-vertices vector via \code{alpha} -> normalize to
#' sum 1. Because \code{igraph} re-normalizes the personalization vector
#' internally, only the \emph{relative} weights matter; normalization here is
#' for interpretability and to make \code{alpha} and \code{exclude_nodes} behave
#' predictably.
#'
#' @seealso [pagerank()], [transform_weights()]
#' @export
#' @examples
#' v <- c("https://x/a", "https://x/b", "https://x/c", "__pr_nofollow_sink__")
#' prior <- data.frame(
#'   url = c("https://x/a", "https://x/b"),
#'   weight = c(900, 100)
#' )
#' # Pure linear authority share; sink excluded
#' align_prior_to_vertices(v, prior,
#'   exclude_nodes = "__pr_nofollow_sink__",
#'   verbose = FALSE
#' )
#' # Compress the dynamic range
#' align_prior_to_vertices(v, prior,
#'   transform = "log",
#'   exclude_nodes = "__pr_nofollow_sink__", verbose = FALSE
#' )
#' # Authority-tilted uniform (every real page keeps a baseline)
#' align_prior_to_vertices(v, prior,
#'   alpha = 0.15,
#'   exclude_nodes = "__pr_nofollow_sink__", verbose = FALSE
#' )
align_prior_to_vertices <- function(vertex_names,
                                    prior_df,
                                    prior_url_col = "url",
                                    prior_weight_col = "weight",
                                    transform = c(
                                      "none", "log", "percentile",
                                      "minmax", "zipf",
                                      "rank_linear"
                                    ),
                                    alpha = 0,
                                    exclude_nodes = character(0),
                                    verbose = TRUE) {
  transform <- match.arg(transform)

  # --- Validation ---
  vertex_names <- as.character(vertex_names)
  .align_validate(prior_df, prior_url_col, prior_weight_col, alpha)

  n <- length(vertex_names)
  if (n == 0) {
    return(numeric(0))
  }

  exclude_nodes <- as.character(exclude_nodes)
  real <- !(vertex_names %in% exclude_nodes)
  n_real <- sum(real)

  # --- Aggregate raw prior weights per URL (sum; raw counts are additive) ---
  agg <- .align_aggregate_prior(prior_df, prior_url_col, prior_weight_col)

  # --- Match, transform, and normalize the authority component ---
  auth <- .align_compute_authority(
    vertex_names, agg$urls, agg$w, real, transform
  )

  # --- Uniform component over real (non-excluded) vertices ---
  uniform <- rep(0, n)
  if (n_real > 0) uniform[real] <- 1 / n_real

  # --- Mixture + normalization (with uniform fallback) ---
  p <- .align_mix_normalize(
    auth$auth_share, uniform, n_real, n, alpha, verbose
  )

  # --- Diagnostics ---
  if (verbose) {
    .align_emit_diagnostics(
      vertex_names, agg$urls, agg$w, auth$has_auth, n_real,
      auth$auth_sum, transform, alpha
    )
  }

  p
}

#' Validate align_prior_to_vertices inputs
#' @noRd
.align_validate <- function(prior_df, prior_url_col, prior_weight_col, alpha) {
  if (!is.data.frame(prior_df)) {
    stop("`prior_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(prior_df) > 0) {
    if (!all(c(prior_url_col, prior_weight_col) %in% names(prior_df))) {
      stop("`prior_df` must have '", prior_url_col, "' and '", prior_weight_col,
        "' columns.",
        call. = FALSE
      )
    }
  }
  .align_validate_alpha(alpha)
  invisible(NULL)
}

#' Validate the alpha mixture knob
#' @noRd
.align_validate_alpha <- function(alpha) {
  msg <- "`alpha` must be a single number between 0 and 1."
  if (!is.numeric(alpha)) stop(msg, call. = FALSE)
  if (length(alpha) != 1) stop(msg, call. = FALSE)
  if (is.na(alpha)) stop(msg, call. = FALSE)
  if (alpha < 0) stop(msg, call. = FALSE)
  if (alpha > 1) stop(msg, call. = FALSE)
  invisible(NULL)
}

#' Aggregate raw prior weights per URL (sum; raw counts are additive)
#' @noRd
.align_aggregate_prior <- function(prior_df, prior_url_col, prior_weight_col) {
  prior_urls <- character(0)
  prior_w <- numeric(0)
  if (nrow(prior_df) > 0) {
    urls <- as.character(prior_df[[prior_url_col]])
    wts <- suppressWarnings(as.numeric(prior_df[[prior_weight_col]]))
    keep <- !is.na(urls) & !is.na(wts)
    urls <- urls[keep]
    wts <- wts[keep]
    if (length(urls) > 0) {
      agg <- tapply(wts, urls, sum)
      prior_urls <- names(agg)
      prior_w <- as.numeric(agg)
    }
  }
  list(urls = prior_urls, w = prior_w)
}

#' Match prior onto vertices and build the normalized authority share
#' @noRd
.align_compute_authority <- function(vertex_names, prior_urls, prior_w, real,
                                     transform) {
  n <- length(vertex_names)
  # --- Match onto vertices (absent vertices -> raw 0) ---
  idx <- match(vertex_names, prior_urls)
  raw <- ifelse(is.na(idx), 0, prior_w[idx])
  raw[!real] <- 0 # excluded nodes never carry authority

  # --- Transform authority (only where authority is present) ---
  authority <- rep(0, n)
  has_auth <- raw > 0
  if (any(has_auth)) {
    tw <- transform_weights(raw[has_auth], method = transform)
    tw[is.na(tw) | tw < 0] <- 0
    authority[has_auth] <- tw
  }
  auth_sum <- sum(authority)
  auth_share <- if (auth_sum > 0) authority / auth_sum else rep(0, n)
  list(auth_share = auth_share, auth_sum = auth_sum, has_auth = has_auth)
}

#' Mix uniform + authority teleport and normalize to sum 1
#' @noRd
.align_mix_normalize <- function(auth_share, uniform, n_real, n, alpha,
                                 verbose) {
  p <- alpha * uniform + (1 - alpha) * auth_share
  s <- sum(p)
  if (s > 0) {
    return(p / s)
  }
  # alpha == 0 and nothing matched: fall back to uniform over real vertices.
  if (n_real > 0) {
    if (verbose) {
      warning("Prior matched no vertices; falling back to uniform teleport ",
        "over the ", n_real, " non-excluded vertices.",
        call. = FALSE
      )
    }
    return(uniform)
  }
  rep(1 / n, n) # degenerate: everything excluded
}

#' Emit TIPR coverage diagnostics via message()
#' @noRd
.align_emit_diagnostics <- function(vertex_names, prior_urls, prior_w, has_auth,
                                    n_real, auth_sum, transform, alpha) {
  n_auth <- sum(has_auth)
  unmatched_mask <- !(prior_urls %in% vertex_names)
  n_unmatched <- sum(unmatched_mask)
  w_unmatched <- sum(prior_w[unmatched_mask])
  uniform_mass <- if (auth_sum > 0) alpha else 1
  message(sprintf(
    paste0(
      "TIPR prior aligned: %d/%d real vertices carry authority; ",
      "transform='%s', alpha=%.3g (uniform mass ~%.1f%%). ",
      "%d prior URL(s) (sum weight %.0f) did not fold onto any vertex ",
      "and were dropped."
    ),
    n_auth, n_real, transform, alpha, uniform_mass * 100,
    n_unmatched, w_unmatched
  ))
}
