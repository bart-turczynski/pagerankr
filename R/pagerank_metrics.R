#' @title PageRank Distribution Metrics
#' @description
#' Compute summary statistics for a vector of PageRank scores. These metrics
#' help characterise how concentrated or dispersed the PageRank distribution
#' is, which is useful when comparing different models or parameter
#' configurations.
#' @name pagerank_metrics
NULL

#' @describeIn pagerank_metrics Gini coefficient (0 = perfectly equal,
#'   1 = maximally concentrated).
#' @param x Numeric vector of non-negative values (typically PageRank scores).
#' @return A single numeric value.
#' @export
#' @examples
#' pr_gini(c(0.5, 0.3, 0.2))
#' pr_gini(c(1, 0, 0))
pr_gini <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0 || all(x == 0)) {
    return(NA_real_)
  }
  n <- length(x)
  x_sorted <- sort(x)
  (2 * sum(seq_len(n) * x_sorted)) / (n * sum(x_sorted)) - (n + 1) / n
}

#' @describeIn pagerank_metrics Shannon entropy (higher = more uniform
#'   distribution).
#' @param x Numeric vector of non-negative values (typically PageRank scores).
#'   Values are internally normalised to sum to 1.
#' @return A single numeric value (in nats). Returns `NA` for empty or
#'   all-zero inputs.
#' @export
#' @examples
#' pr_entropy(c(1 / 3, 1 / 3, 1 / 3)) # maximum entropy for 3 nodes
#' pr_entropy(c(1, 0, 0)) # minimum entropy
pr_entropy <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0 || all(x == 0)) {
    return(NA_real_)
  }
  # Normalise to probability distribution
  p <- x / sum(x)
  # Drop zeros (0 * log(0) = 0 by convention)
  p <- p[p > 0]
  -sum(p * log(p))
}

#' @describeIn pagerank_metrics Share of total PageRank held by the top-k
#'   fraction of nodes (e.g., top 10 percent).
#' @param x Numeric vector of non-negative values (typically PageRank scores).
#' @param k Fraction of nodes to consider (0 < k <= 1). Default `0.1` (top 10
#'   percent).
#' @return A single numeric value between 0 and 1 representing the cumulative
#'   share.
#' @export
#' @examples
#' pr_top_k_share(c(0.5, 0.3, 0.1, 0.05, 0.05))
#' pr_top_k_share(c(0.5, 0.3, 0.1, 0.05, 0.05), k = 0.4)
pr_top_k_share <- function(x, k = 0.1) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0 || all(x == 0)) {
    return(NA_real_)
  }
  if (k <= 0 || k > 1) {
    stop(
      "`k` must be between 0 (exclusive) and 1 (inclusive).",
      call. = FALSE
    )
  }
  n <- length(x)
  top_count <- max(1L, ceiling(n * k))
  sorted_x <- sort(x, decreasing = TRUE)
  sum(sorted_x[seq_len(top_count)]) / sum(x)
}
