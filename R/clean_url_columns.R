#' @title Clean URL Columns in a Data Frame
#' @description Applies `rurl::clean_url` to specified columns of a data frame,
#'   using internal memoization for efficiency.
#'
#' @param data_frame A data frame containing URL columns to be cleaned.
#' @param columns A character vector specifying the names of the columns
#'   containing URLs. Defaults to `c("from", "to")`.
#' @param ... Additional arguments passed to `rurl::clean_url`.
#' @param .memoized_clean_url An optional pre-existing memoized rurl::clean_url function.
#' If NULL (default), a new memoization cache is created for this function call.
#' This is mostly for internal use by the `pagerank()` wrapper to share a cache.
#'
#' @return A data frame with the specified URL columns cleaned. An attribute
#'   `url_map` containing the mapping from original to cleaned URLs might be
#'   attached invisibly if deemed useful (currently not implemented but noted from spec).
#' @export
#' @importFrom rurl clean_url
#' @examples
#' df <- data.frame(
#'   from = c("http://example.com/path", "HTTPS://Example.com/PATH#frag"),
#'   to = c("www.another.com?q=1", "another.com/?q=1&b=2"),
#'   other_col = 1:2
#' )
#' cleaned_df <- clean_url_columns(df, columns = c("from", "to"))
#' print(cleaned_df)
#'
#' # Example with rurl::clean_url parameters
#' cleaned_df_custom <- clean_url_columns(
#'   df, 
#'   columns = c("from", "to"), 
#'   drop_fragments = FALSE
#' )
#' print(cleaned_df_custom)
clean_url_columns <- function(data_frame, 
                              columns = c("from", "to"), 
                              ...,
                              .memoized_clean_url = NULL) {

  if (!is.data.frame(data_frame)) {
    stop("`data_frame` must be a data frame.", call. = FALSE)
  }
  if (!is.character(columns) || !all(columns %in% names(data_frame))) {
    stop("All `columns` must be existing column names in `data_frame`.", call. = FALSE)
  }

  # Use the provided memoized function or create a new one for this call
  if (is.null(.memoized_clean_url)) {
    # This internal helper should be in utils.R
    # For now, defining it conceptually here or assuming it exists
    # .local_memoized_clean_url <- .create_memoized_cleaner() 
    active_clean_url <- .create_memoized_cleaner() # Placeholder for actual memoization util
  } else {
    active_clean_url <- .memoized_clean_url
  }

  # Capture additional arguments for rurl::clean_url
  rurl_args <- list(...)

  cleaned_data_frame <- data_frame
  url_map_list <- list() # To store mappings for a potential url_map attribute

  for (col_name in columns) {
    if (col_name %in% names(cleaned_data_frame)) {
      unique_urls <- unique(stats::na.omit(cleaned_data_frame[[col_name]]))
      cleaned_urls_vec <- character(length(unique_urls))
      names(cleaned_urls_vec) <- unique_urls
      
      if(length(unique_urls) > 0) {
        for (i in seq_along(unique_urls)) {
          original_url <- unique_urls[i]
          # Apply rurl::clean_url with extra arguments using do.call
          cleaned_url <- do.call(active_clean_url, c(list(original_url), rurl_args))
          cleaned_urls_vec[original_url] <- cleaned_url
        }
        
        # Create a mapping for the current column's URLs
        current_map <- cleaned_urls_vec
        # Replace NAs in original column with NAs, not "NA" string
        is_na_original <- is.na(cleaned_data_frame[[col_name]])
        cleaned_column_values <- cleaned_urls_vec[as.character(cleaned_data_frame[[col_name]])]
        cleaned_column_values[is_na_original] <- NA
        cleaned_data_frame[[col_name]] <- cleaned_column_values
        
        url_map_list[[col_name]] <- current_map
      }
    } else {
      warning(paste0("Column '", col_name, "' not found in data_frame."), call. = FALSE)
    }
  }

  # Combine individual column maps into a single url_map (optional attribute)
  # This part needs further refinement based on how `url_map` should be structured.
  # For now, just returning the cleaned data frame.
  # attr(cleaned_data_frame, "url_map") <- url_map_list # Or some processed version

  return(cleaned_data_frame)
} 