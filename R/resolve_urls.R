#' @title Resolve URLs Through Redirects
#' @description Given a character vector of URLs and a redirect data frame,
#'   resolves each URL to its final destination by following redirect chains.
#'   Unlike \code{\link{resolve_redirects}}, this function does not require an
#'   edge list -- it works directly on a list of URLs.
#'
#' @param urls Character vector of URLs to resolve.
#' @param redirects_df A data frame containing redirect rules.
#' @param redirect_from_col Character, name of the source column. Default
#'   \code{"from"}.
#' @param redirect_to_col Character, name of the target column. Default
#'   \code{"to"}.
#' @param duplicate_from_policy How to handle conflicting redirects. Passed
#'   through to redirect preprocessing. Default \code{"strict"}.
#' @param loop_handling How to handle redirect cycles. Default \code{"error"}.
#'   Use \code{"prune_loop"} or \code{"break_arrow"} to resolve despite loops.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{original}{The input URL.}
#'     \item{resolved}{The final destination after following all redirects.}
#'     \item{changed}{Logical, whether the URL was modified by a redirect.}
#'   }
#'
#' @export
#' @examples
#' redirects <- data.frame(
#'   from = c("A", "B", "C"),
#'   to   = c("B", "C", "Final"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Resolve specific URLs
#' resolve_urls(c("A", "B", "X"), redirects)
#'
#' # X is not in the redirect map, so it stays as-is
resolve_urls <- function(urls,
                         redirects_df,
                         redirect_from_col = "from",
                         redirect_to_col = "to",
                         duplicate_from_policy = c("strict",
                                                   "first_wins",
                                                   "last_wins",
                                                   "most_frequent",
                                                   "prune_source",
                                                   "resolve_if_consistent"),
                         loop_handling = c("error",
                                           "prune_loop",
                                           "break_arrow")) {

  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)

  # --- Validation ---
  if (!is.character(urls)) {
    stop("`urls` must be a character vector.", call. = FALSE)
  }
  if (!is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(redirects_df) > 0 &&
      !all(c(redirect_from_col, redirect_to_col) %in% names(redirects_df))) {
    stop("`redirects_df` must have '", redirect_from_col, "' and '",
         redirect_to_col, "' columns.", call. = FALSE)
  }

  original <- urls

  # Handle empty inputs
  if (length(urls) == 0 || nrow(redirects_df) == 0) {
    return(data.frame(
      original = original,
      resolved = original,
      changed = rep(FALSE, length(original)),
      stringsAsFactors = FALSE
    ))
  }

  # --- Clean redirects (same preprocessing as resolve_redirects) ---
  r_sources <- as.character(redirects_df[[redirect_from_col]])
  r_targets <- as.character(redirects_df[[redirect_to_col]])

  valid <- !is.na(r_sources) & !is.na(r_targets)
  r_sources <- r_sources[valid]
  r_targets <- r_targets[valid]

  # Remove self-refs
  self_ref <- r_sources == r_targets
  r_sources <- r_sources[!self_ref]
  r_targets <- r_targets[!self_ref]

  if (length(r_sources) == 0) {
    return(data.frame(
      original = original,
      resolved = original,
      changed = rep(FALSE, length(original)),
      stringsAsFactors = FALSE
    ))
  }

  # Handle conflicts
  clean_df <- data.frame(from = r_sources, to = r_targets,
                         stringsAsFactors = FALSE)
  clean_df <- .preprocess_redirects(clean_df, "from", "to",
                                    policy = duplicate_from_policy)

  if (nrow(clean_df) == 0) {
    return(data.frame(
      original = original,
      resolved = original,
      changed = rep(FALSE, length(original)),
      stringsAsFactors = FALSE
    ))
  }

  # --- Build canonical map ---
  canonical_map <- .resolve_via_graph(clean_df$from, clean_df$to,
                                     loop_handling = loop_handling)

  # --- Apply map ---
  resolved <- urls
  idx <- match(urls, names(canonical_map))
  resolved[!is.na(idx)] <- canonical_map[idx[!is.na(idx)]]
  # Preserve NAs
  resolved[is.na(urls)] <- NA_character_

  data.frame(
    original = original,
    resolved = resolved,
    changed = !is.na(original) & original != resolved,
    stringsAsFactors = FALSE
  )
}
