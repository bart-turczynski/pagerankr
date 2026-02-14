#' @title Clean URL Columns in a Data Frame
#' @description Applies `rurl::get_clean_url` to specified columns of a data frame,
#'   using internal memoization for efficiency.
#'
#' @param data_frame A data frame containing URL columns to be cleaned.
#' @param columns A character vector specifying the names of the columns
#'   containing URLs. Defaults to `c("from", "to")`.
#' @param ... Additional arguments passed to `rurl::get_clean_url`.
#' @param .memoized_clean_url An optional pre-existing memoized `rurl::get_clean_url` function.
#' If NULL (default), a new memoization cache is created for this function call.
#' This is mostly for internal use by the `pagerank()` wrapper to share a cache.
#'
#' @return A data frame with the specified URL columns cleaned. An attribute
#'   `url_map` containing the mapping from original to cleaned URLs might be
#'   attached invisibly (currently not implemented but noted from spec).
#' @export
#' @importFrom rurl get_clean_url
#' @examples
#' df <- data.frame(
#'   from = c("http://example.com/path", 
#'            "HTTPS://Example.com/PATH#frag", NA, 
#'            "http://example.com/path"),
#'   to = c("www.another.com?q=1", "another.com/?q=1&b=2", 
#'          "http://foo.bar", NA),
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
#' NA values in the specified columns are preserved in the output. Downstream functions in the pagerankr workflow (such as get_unique_edges and pagerank) will automatically drop any edge where either from or to is NA.
clean_url_columns <- function(data_frame, 
                              columns = c("from", "to"), 
                              ...,
                              .memoized_clean_url = NULL) {

  if (!is.data.frame(data_frame)) {
    stop("`data_frame` must be a data frame.", call. = FALSE)
  }
  if (!is.character(columns) || !all(sapply(columns, function(cn) cn %in% names(data_frame)))) {
    # Ensure all specified columns actually exist
    missing_cols <- columns[!sapply(columns, function(cn) cn %in% names(data_frame))]
    if (length(missing_cols) > 0) {
      stop("Column(s) not found in `data_frame`: ", paste(missing_cols, collapse=", "), call. = FALSE)
    }
    # This case should ideally not be reached if the above check is comprehensive,
    # but as a general guard for columns argument type.
    stop("`columns` must be a character vector of existing column names.", call. = FALSE)
  }

  if (is.null(.memoized_clean_url)) {
    # If no shared memoizer is passed, create one for the scope of this call.
    # .create_memoized_cleaner is an internal util function (e.g. from utils.R)
    active_clean_url <- .create_memoized_cleaner() 
  } else {
    active_clean_url <- .memoized_clean_url
  }

  rurl_args <- list(...)
  cleaned_data_frame <- data_frame
  # url_map_list <- list() # For the optional 'url_map' attribute, not implemented in this pass.

  for (col_name in columns) {
    # It's already confirmed col_name is in names(data_frame)
    original_column_as_char <- as.character(cleaned_data_frame[[col_name]])
    unique_urls_to_clean <- unique(stats::na.omit(original_column_as_char))
    
    if (length(unique_urls_to_clean) > 0) {
      # Create a named vector to map unique original URLs to their cleaned versions.
      cleaned_url_lookup <- stats::setNames(vector("character", length(unique_urls_to_clean)), 
                                           unique_urls_to_clean)

      for (url_to_clean in unique_urls_to_clean) {
        # Apply active_clean_url (which is memoized rurl::get_clean_url) with extra arguments.
        cleaned_version <- do.call(active_clean_url, c(list(url_to_clean), rurl_args))
        cleaned_url_lookup[url_to_clean] <- cleaned_version
      }
      
      # Apply the map back to the original column structure, preserving NAs.
      # Initialize a new vector for the cleaned column values.
      new_column_values <- character(length(original_column_as_char))
      is_na_in_original <- is.na(original_column_as_char)
      
      # Get cleaned URLs for non-NA original URLs
      # The names of cleaned_url_lookup are the non-NA original_column_as_char values
      # So, we can use original_column_as_char[!is_na_in_original] to index the lookup table.
      if(any(!is_na_in_original)){
         new_column_values[!is_na_in_original] <- cleaned_url_lookup[original_column_as_char[!is_na_in_original]]
      }
      # Set NA values in the new column where they were in the original.
      new_column_values[is_na_in_original] <- NA_character_
      
      cleaned_data_frame[[col_name]] <- new_column_values
      
      # Optionally, store this column's map for the url_map attribute
      # url_map_list[[col_name]] <- cleaned_url_lookup
    }
    # If length(unique_urls_to_clean) == 0, the column was all NAs or empty, no cleaning needed.
  }

  # As per spec: "Attaches url_map invisibly if useful." - Not implementing attachment in this pass.
  # if (length(url_map_list) > 0) {
  #   attr(cleaned_data_frame, "url_map") <- url_map_list 
  # }

  return(cleaned_data_frame)
} 