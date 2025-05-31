#' @title Create a Memoized Version of rurl::clean_url
#' @description Creates and returns a function that is a memoized version of
#'   `rurl::clean_url`. The cache is stored in an environment associated with
#'   the returned function (closure).
#'
#' @details This is a base R implementation of memoization. Each call to
#'   `.create_memoized_cleaner()` creates a new cache.
#'
#' @return A function that takes a URL string and `...` (arguments for
#'   `rurl::clean_url`) and returns a cleaned URL string, using an internal
#'   cache to avoid re-computing for the same inputs.
#' @noRd # Marks as internal, not exported
.create_memoized_cleaner <- function() {
  cache <- new.env(hash = TRUE, parent = emptyenv())
  
  memoized_clean_url <- function(url_string, ...) {
    # Create a unique key based on the URL and other relevant args to clean_url
    # For rurl::clean_url, the ... args are significant.
    # A simple approach: paste them together. More robust: deparse(list(...))
    args_list <- list(...)
    # Sort names of args_list to ensure consistent key for same args in different order
    if (length(args_list) > 0) {
        key_args_part <- paste(names(sort(args_list)), sapply(args_list[order(names(args_list))], as.character), collapse = "|")
    } else {
        key_args_part <- ""
    }
    cache_key <- paste(as.character(url_string), key_args_part, sep = "__")
    
    if (exists(cache_key, envir = cache, inherits = FALSE)) {
      return(get(cache_key, envir = cache, inherits = FALSE))
    }
    
    # If not in cache, compute, store, and return
    # Need to ensure rurl is available or explicitly import clean_url if not done globally
    # Assuming rurl::clean_url is available in the namespace due to Imports in DESCRIPTION
    cleaned_url <- rurl::clean_url(url_string, ...)
    assign(cache_key, cleaned_url, envir = cache)
    return(cleaned_url)
  }
  
  return(memoized_clean_url)
}


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
    actual_cycle_str <- paste(c(path[path_start_index:length(path)], url), collapse = " -> ")
    
    stop(
      "Redirect cycle detected for URL '", url, "'. Path: ", actual_cycle_str,
      call. = FALSE
    )
  }
  
  current_path <- c(path, url)
  
  if (url %in% names(redirect_map)) {
    next_url <- redirect_map[[url]]
    # Recursive call to trace further
    return(.trace_redirect_path(next_url, redirect_map, current_path))
  } else {
    # No further redirect for this URL, it's a final destination in this chain
    return(url)
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
        # Using grepl for vectorized check
        if (any(grepl("[?&]", urls, useBytes = TRUE))) { # useBytes for potentially non-ASCII URLs
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
} 
