
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

#' Validate an optional `weight_col` argument
#'
#' Error unless `weight_col` is NULL or a single string naming a numeric column
#' present in `edge_list_df`. Shared by the PageRank and HITS compute paths.
#' @keywords internal
#' @noRd
.validate_weight_col <- function(weight_col, edge_list_df) {
  if (is.null(weight_col)) {
    return(invisible(NULL))
  }
  if (!is.character(weight_col) || length(weight_col) != 1) {
    stop(
      "`weight_col` must be a single character string or NULL.",
      call. = FALSE
    )
  }
  if (nrow(edge_list_df) > 0 && !(weight_col %in% names(edge_list_df))) {
    stop("`weight_col` '", weight_col, "' not found in `edge_list_df`.",
      call. = FALSE
    )
  }
  if (nrow(edge_list_df) > 0 && !is.numeric(edge_list_df[[weight_col]])) {
    stop("`weight_col` '", weight_col, "' must be a numeric column.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Validate the scalar cleaning flags shared by hits() and salsa()
#'
#' Error unless each of `clean_edge_urls`, `clean_redirect_urls` and
#' `drop_isolates_flag` is a single logical and `rurl_params` is a list.
#' Error-message text is preserved verbatim.
#' @keywords internal
#' @noRd
.validate_cleaning_flags <- function(clean_edge_urls, clean_redirect_urls,
                                     rurl_params, drop_isolates_flag) {
  if (!is.logical(clean_edge_urls) || length(clean_edge_urls) != 1) {
    stop("`clean_edge_urls` must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(clean_redirect_urls) || length(clean_redirect_urls) != 1) {
    stop("`clean_redirect_urls` must be a single logical value.", call. = FALSE)
  }
  if (!is.list(rurl_params)) {
    stop("`rurl_params` must be a list.", call. = FALSE)
  }
  if (!is.logical(drop_isolates_flag) || length(drop_isolates_flag) != 1) {
    stop("`drop_isolates_flag` must be a single logical value.", call. = FALSE)
  }
  invisible(NULL)
}
