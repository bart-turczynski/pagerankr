#' @title Transform Edge Weights for PageRank
#' @description Applies a transformation strategy to a numeric vector of edge
#'   weights before passing them to \code{\link{pagerank}}. Useful for
#'   converting link positions, click counts, or other raw signals into weights
#'   suitable for the PageRank random surfer model.
#'
#' @param x Numeric vector of raw weights (e.g., link positions on a page,
#'   GA4 click counts, or any positive numeric signal).
#' @param method Character, the transformation strategy. One of:
#'   \describe{
#'     \item{\code{"none"}}{Return \code{x} unchanged.}
#'     \item{\code{"rank_linear"}}{Convert to rank order (1 = highest value),
#'       then assign linearly decreasing weights:
#'       \code{weight = (n - rank + 1) / n}. Position 1 gets 1.0, position
#'       \emph{n} gets \code{1/n}.}
#'     \item{\code{"zipf"}}{Convert to rank order, then apply Zipf's law:
#'       \code{weight = 1 / rank^alpha}. Position 1 gets 1.0, position 2
#'       gets \code{1/2^alpha}, etc. Controlled by the \code{alpha} parameter
#'       (default 1).}
#'     \item{\code{"log"}}{Apply \code{log(x + offset)} to compress large
#'       ranges (e.g., GA4 click counts spanning 1 to 100,000). The
#'       \code{offset} parameter (default 1) avoids \code{log(0)}.}
#'     \item{\code{"minmax"}}{Scale to the \code{[0, 1]} range using
#'       min-max normalisation. A small floor (\code{floor_value}, default
#'       0.01) is added so that the lowest-weighted edge still carries some
#'       weight rather than zero.}
#'     \item{\code{"percentile"}}{Map values to their empirical percentile
#'       (0--1). Robust to extreme outliers.}
#'   }
#' @param alpha Numeric, exponent for the \code{"zipf"} method.
#'   Default \code{1.0}. Higher values make the drop-off steeper
#'   (position 1 dominates more).
#' @param offset Numeric, added to \code{x} before the \code{"log"}
#'   transform. Default \code{1} (so that zero-valued inputs produce
#'   \code{log(1) = 0} rather than \code{-Inf}).
#' @param floor_value Numeric, minimum weight for the \code{"minmax"}
#'   method. Default \code{0.01}.
#' @param descending Logical. For rank-based methods (\code{"rank_linear"},
#'   \code{"zipf"}), whether higher input values get higher weights.
#'   Default \code{TRUE} (e.g., if the input is click counts, more clicks
#'   = higher weight). Set to \code{FALSE} when the input is link position
#'   on a page (position 1 = most valuable, but numerically smallest).
#'
#' @return Numeric vector of the same length as \code{x} with transformed
#'   weights. \code{NA} values in \code{x} are preserved as \code{NA} in
#'   the output.
#'
#' @export
#' @examples
#' # Link positions on a page (1 = top, most valuable)
#' positions <- c(1, 2, 3, 4, 5)
#' transform_weights(positions, "rank_linear", descending = FALSE)
#' transform_weights(positions, "zipf", alpha = 1, descending = FALSE)
#' transform_weights(positions, "zipf", alpha = 2, descending = FALSE)
#'
#' # GA4 click counts (wide range)
#' clicks <- c(50000, 12000, 800, 150, 3)
#' transform_weights(clicks, "log")
#' transform_weights(clicks, "minmax")
#' transform_weights(clicks, "zipf")
#'
#' # Use with pagerank()
#' edges <- data.frame(
#'   from = c("Home", "Home", "Home"),
#'   to = c("About", "Blog", "Contact"),
#'   position = c(1, 2, 5)
#' )
#' edges$weight <- transform_weights(edges$position, "zipf",
#'   descending = FALSE
#' )
#' # pagerank(edges, weight_col = "weight", clean_edge_urls = FALSE)
transform_weights <- function(x,
                              method = c(
                                "none", "rank_linear", "zipf",
                                "log", "minmax", "percentile"
                              ),
                              alpha = 1.0,
                              offset = 1,
                              floor_value = 0.01,
                              descending = TRUE) {
  method <- match.arg(method)

  .tw_validate(x, alpha, offset, floor_value, descending)

  # Handle all-NA or empty input
  non_na <- !is.na(x)
  if (sum(non_na) == 0) {
    return(x)
  }

  if (method == "none") {
    return(x)
  }

  vals <- x[non_na]
  result <- rep(NA_real_, length(x))
  result[non_na] <- .tw_compute(
    method, vals, alpha, offset, floor_value, descending
  )
  result
}

#' Validate transform_weights parameters
#'
#' Preserves the original short-circuit order and error-message text.
#'
#' @noRd
.tw_validate <- function(x, alpha, offset, floor_value, descending) {
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }
  msg_alpha <- "`alpha` must be a single positive number."
  if (!is.numeric(alpha)) stop(msg_alpha, call. = FALSE)
  if (length(alpha) != 1) stop(msg_alpha, call. = FALSE)
  if (alpha <= 0) stop(msg_alpha, call. = FALSE)

  msg_offset <- "`offset` must be a single number."
  if (!is.numeric(offset)) stop(msg_offset, call. = FALSE)
  if (length(offset) != 1) stop(msg_offset, call. = FALSE)

  msg_floor <- "`floor_value` must be a single non-negative number."
  if (!is.numeric(floor_value)) stop(msg_floor, call. = FALSE)
  if (length(floor_value) != 1) stop(msg_floor, call. = FALSE)
  if (floor_value < 0) stop(msg_floor, call. = FALSE)

  msg_desc <- "`descending` must be TRUE or FALSE."
  if (!is.logical(descending)) stop(msg_desc, call. = FALSE)
  if (length(descending) != 1) stop(msg_desc, call. = FALSE)
  invisible(NULL)
}

#' Dispatch a single transform method over the non-NA values
#'
#' @return Numeric vector aligned to the non-NA entries of `x`.
#' @noRd
.tw_compute <- function(method, vals, alpha, offset, floor_value, descending) {
  if (method == "rank_linear") {
    return(.tw_rank_linear(vals, descending))
  }
  if (method == "zipf") {
    return(.tw_zipf(vals, alpha, descending))
  }
  if (method == "log") {
    return(log(vals + offset))
  }
  if (method == "minmax") {
    return(.tw_minmax(vals, floor_value))
  }
  if (method == "percentile") {
    return(.tw_percentile(vals, descending))
  }
  vals # nocov
}

#' Rank helper for rank-based transforms
#'
#' Returns ranks where, when `descending` is TRUE, higher input values get
#' lower rank numbers (rank 1 = highest value).
#'
#' @noRd
.tw_rank <- function(vals, descending) {
  if (descending) {
    return(rank(-vals, ties.method = "average"))
  }
  rank(vals, ties.method = "average")
}

#' rank_linear transform
#' @noRd
.tw_rank_linear <- function(vals, descending) {
  n <- length(vals)
  r <- .tw_rank(vals, descending)
  (n - r + 1) / n
}

#' zipf transform
#' @noRd
.tw_zipf <- function(vals, alpha, descending) {
  r <- .tw_rank(vals, descending)
  1 / (r^alpha)
}

#' minmax transform
#' @noRd
.tw_minmax <- function(vals, floor_value) {
  mn <- min(vals)
  mx <- max(vals)
  if (mx == mn) {
    # All values identical
    return(1.0)
  }
  scaled <- (vals - mn) / (mx - mn)
  scaled * (1 - floor_value) + floor_value
}

#' percentile transform
#' @noRd
.tw_percentile <- function(vals, descending) {
  n <- length(vals)
  # Percentile uses the inverse rank orientation of the other rank methods.
  r <- .tw_rank(vals, !descending)
  r / n
}
