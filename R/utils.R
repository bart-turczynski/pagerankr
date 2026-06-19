
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
