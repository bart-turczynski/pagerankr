#' @title Clean URL Columns in a Data Frame
#' @description Applies `rurl::get_clean_url` to specified columns of a data
#'   frame. URLs are cleaned under pagerankr's explicit canonicalization profile
#'   (see Details), with any arguments in `...` overriding individual knobs.
#'
#' @param data_frame A data frame containing URL columns to be cleaned.
#' @param columns A character vector specifying the names of the columns
#'   containing URLs. Defaults to `c("from", "to")`.
#' @param ... `rurl::get_clean_url` arguments that override the canonicalization
#'   profile per key. Recognized knobs: `protocol_handling`, `case_handling`,
#'   `www_handling`, `trailing_slash_handling`, `index_page_handling`,
#'   `path_normalization`, `scheme_relative_handling`,
#'   `subdomain_levels_to_keep`, `host_encoding`, `path_encoding`.
#'
#' @return A data frame with the specified URL columns cleaned.
#' @export
#' @importFrom rurl get_clean_url
#' @examples
#' df <- data.frame(
#'   from = c(
#'     "http://example.com/path",
#'     "HTTPS://Example.com/PATH#frag", NA,
#'     "http://example.com/path"
#'   ),
#'   to = c(
#'     "www.another.com?q=1", "another.com/?q=1&b=2",
#'     "http://foo.bar", NA
#'   ),
#'   other_col = 1:4,
#'   stringsAsFactors = FALSE # Added for consistency
#' )
#' cleaned_df <- clean_url_columns(df, columns = c("from", "to"))
#' print(cleaned_df)
#'
#' # Pass extra arguments to rurl::get_clean_url via ...
#' cleaned_df_custom <- clean_url_columns(
#'   df,
#'   columns = c("from", "to"),
#'   protocol_handling = "http"
#' )
#' print(cleaned_df_custom)
#'
#' @details
#' The canonicalization profile pins every `rurl` knob explicitly
#' (`protocol_handling = "keep"`, `case_handling = "lower_host"`, and the
#' remaining knobs at their faithful defaults) so node identities do not depend
#' on `rurl`'s own (version-dependent) defaults. The values mirror `rurl`'s
#' current defaults, so this is behavior-preserving; it guards against future
#' default drift and keeps the cleaning and domain-filtering paths symmetrical.
#'
#' NA values in the specified columns are preserved in the output. Downstream
#' functions in the pagerankr workflow (such as get_unique_edges and pagerank)
#' will automatically drop any edge where either from or to is NA.
clean_url_columns <- function(data_frame,
                              columns = c("from", "to"),
                              ...) {
  if (!is.data.frame(data_frame)) {
    stop("`data_frame` must be a data frame.", call. = FALSE)
  }
  cols_exist <- vapply(
    columns, function(cn) cn %in% names(data_frame), logical(1L)
  )
  if (!is.character(columns) || !all(cols_exist)) {
    # Ensure all specified columns actually exist
    missing_cols <- columns[
      !cols_exist
    ]
    if (length(missing_cols) > 0) {
      stop("Column(s) not found in `data_frame`: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
    # This case should ideally not be reached if the above check is
    # comprehensive, but as a general guard for columns argument type.
    stop(
      "`columns` must be a character vector of existing column names.",
      call. = FALSE
    )
  }

  # Apply pagerankr's explicit canonicalization profile, letting any `...`
  # arguments override individual knobs. Pinning every knob keeps node
  # identities independent of rurl's own (version-dependent) defaults.
  rurl_args <- .resolve_rurl_params(list(...))
  cleaned_data_frame <- data_frame

  for (col_name in columns) {
    # It's already confirmed col_name is in names(data_frame)
    original_column_as_char <- as.character(cleaned_data_frame[[col_name]])
    unique_urls_to_clean <- unique(stats::na.omit(original_column_as_char))

    if (length(unique_urls_to_clean) > 0) {
      # Clean each unique URL once. rurl::get_clean_url is vectorized and
      # memoizes parses internally (the cache is shared across columns and
      # calls), so no local memoization is needed here.
      cleaned_unique <- do.call(
        rurl::get_clean_url,
        c(list(unique_urls_to_clean), rurl_args)
      )
      cleaned_url_lookup <- stats::setNames(
        cleaned_unique, unique_urls_to_clean
      )

      # Apply the map back to the original column structure, preserving NAs.
      new_column_values <- character(length(original_column_as_char))
      is_na_in_original <- is.na(original_column_as_char)
      if (!all(is_na_in_original)) {
        new_column_values[!is_na_in_original] <-
          cleaned_url_lookup[original_column_as_char[!is_na_in_original]]
      }
      new_column_values[is_na_in_original] <- NA_character_

      cleaned_data_frame[[col_name]] <- new_column_values
    }
    # If length(unique_urls_to_clean) == 0, the column was all NAs or empty,
    # no cleaning needed.
  }

  cleaned_data_frame
}
