#' @title Trace Redirect Path to Final Destination
#' @description Given a starting URL and a map of redirects, trace the path to
#'   its final destination. Detects redirect cycles and stops.
#'
#' @param url The starting URL (character string) to trace.
#' @param redirect_map A named character vector where names are source URLs and
#'   values are their direct target URLs (already checked for ambiguities).
#' @param path A character vector used internally to track the current redirect
#'   path to detect cycles. Users should not set this.
#'
#' @return The final destination URL (character string). If the URL is not in
#'   the redirect map or has no further redirects, it returns the URL itself.
#'   If a cycle is detected, it stops and returns the URL where the cycle was
#'   detected, and issues a warning.
#' @noRd
.trace_redirect_path <- function(url, redirect_map, path = character(0)) {
  # Add current url to path for cycle detection
  if (url %in% path) {
    # Cycle detected
    # Corrected cycle representation (start from the cycled URL)
    path_start_index <- match(url, path)
    actual_cycle_str <- paste(
      c(path[path_start_index:length(path)], url),
      collapse = " -> "
    )

    stop(
      "Redirect cycle detected for URL '", url, "'. Path: ", actual_cycle_str,
      call. = FALSE
    )
  }

  current_path <- c(path, url)

  if (url %in% names(redirect_map)) {
    next_url <- redirect_map[[url]]
    # Recursive call to trace further
    .trace_redirect_path(next_url, redirect_map, current_path)
  } else {
    # No further redirect for this URL, it's a final destination in this chain
    url
  }
}


#' @title Check if URLs in Data Frame Columns Contain Query Parameters
#' @description Checks specified columns of a data frame for URLs that contain
#' query parameters ('?' or '&').
#'
#' @param data_frame A data frame.
#' @param columns A character vector of column names to check.
#' @return Logical. TRUE if any URL in the specified columns contains query
#'   parameters, FALSE otherwise.
#' @noRd
.urls_contain_query_params <- function(data_frame, columns) {
  if (!is.data.frame(data_frame) || nrow(data_frame) == 0) {
    return(FALSE)
  }
  for (col_name in columns) {
    if (col_name %in% names(data_frame)) {
      urls <- stats::na.omit(data_frame[[col_name]])
      if (length(urls) > 0) {
        # Check for presence of '?’ or '&' characters
        # Using grepl for vectorized check.
        # useBytes for potentially non-ASCII URLs
        if (any(grepl("[?&]", urls, useBytes = TRUE))) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}
