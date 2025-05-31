#' @title Resolve Redirects in an Edge List
#' @description Updates an edge list by replacing URLs with their final target
#'   URLs based on a redirect data frame. Detects cycles and ambiguities.
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or names specified by `edge_from_col` and `edge_to_col`).
#'   Input URLs are expected to be cleaned if necessary before calling this function.
#' @param redirects_df A data frame detailing redirect rules, with columns
#'   "from" and "to" (or names specified by `redirect_from_col` and `redirect_to_col`).
#'   Input URLs are expected to be cleaned if necessary.
#' @param edge_from_col Name of the source URL column in `edge_list_df`. Default "from".
#' @param edge_to_col Name of the target URL column in `edge_list_df`. Default "to".
#' @param redirect_from_col Name of the source URL column in `redirects_df`. Default "from".
#' @param redirect_to_col Name of the target URL column in `redirects_df`. Default "to".
#'
#' @return A data frame, the edge list with URLs resolved to their final
#'   destinations. Errors out if redirect cycles or ambiguities are detected.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "D", "E", "F_no_redirect", NA, "G"),
#'   to =   c("B", "C", "X", "Y", "A", "Z_no_redirect", "H", NA),
#'   stringsAsFactors = FALSE
#' )
#' redirects <- data.frame(
#'   from = c("B", "Y", "X", "A", "G"),
#'   to =   c("C", "Z", "C", "FINAL_A", "FINAL_G"),
#'   stringsAsFactors = FALSE
#' )
#' resolved_edges <- resolve_redirects(edges, redirects)
#' print(resolved_edges)
#' 
#' # Example with a cycle in redirects
#' cyclic_redirects <- data.frame(from = c("L1", "L2"), to = c("L2", "L1"))
#' try(resolve_redirects(data.frame(from="L1", to="L3"), cyclic_redirects)) # Expected to error
#' 
#' # Example with ambiguity in redirects
#' ambiguous_redirects <- data.frame(from = c("X", "X"), to = c("TARGET1", "TARGET2"))
#' try(resolve_redirects(data.frame(from="START", to="X"), ambiguous_redirects)) # Expected to error
#'
#' # Example with empty redirects_df
#' resolve_redirects(edges, data.frame(from=character(), to=character()))
resolve_redirects <- function(edge_list_df, 
                              redirects_df,
                              edge_from_col = "from",
                              edge_to_col = "to",
                              redirect_from_col = "from",
                              redirect_to_col = "to") {

  # --- Input Validation ---
  if (!is.data.frame(edge_list_df) || 
      (nrow(edge_list_df) > 0 && !all(c(edge_from_col, edge_to_col) %in% names(edge_list_df)))) {
    stop("`edge_list_df` must be a data frame with specified edge columns if not empty.", call. = FALSE)
  }
  if (!is.data.frame(redirects_df) || 
      (nrow(redirects_df) > 0 && !all(c(redirect_from_col, redirect_to_col) %in% names(redirects_df)))) {
    stop("`redirects_df` must be a data frame with specified redirect columns if not empty.", call. = FALSE)
  }
  
  # If redirects_df is empty, no redirects to process, return edge_list_df as is.
  if (nrow(redirects_df) == 0) {
    return(edge_list_df)
  }
  
  # Ensure no NA values in redirect mapping columns, as they are problematic for path finding.
  # Check only if redirects_df has rows (already handled by the above check, but for clarity).
  if (any(is.na(redirects_df[[redirect_from_col]])) || any(is.na(redirects_df[[redirect_to_col]]))) {
    stop("Redirect columns ('", redirect_from_col, "', '", redirect_to_col, "') in `redirects_df` cannot contain NA values.", call. = FALSE)
  }

  # --- Prepare Redirect Map & Check Ambiguity ---
  # Convert to character to avoid factor issues and ensure consistent keying.
  r_from <- as.character(redirects_df[[redirect_from_col]])
  r_to <- as.character(redirects_df[[redirect_to_col]])
  
  # Create a named list for easier lookup: source_url -> list of target_url(s).
  # This structure helps detect ambiguities directly.
  redirect_map_ambiguity_check <- split(r_to, r_from)

  ambiguous_sources <- names(redirect_map_ambiguity_check)[sapply(redirect_map_ambiguity_check, function(targets) length(unique(targets)) > 1)]
  if (length(ambiguous_sources) > 0) {
    # Construct a detailed error message for ambiguities
    error_message_parts <- sapply(ambiguous_sources, function(src) {
      paste0("'", src, "' -> c('", paste(unique(redirect_map_ambiguity_check[[src]]), collapse = "', '"), "')")
    })
    stop(
      "Redirect ambiguity detected. The following source URLs map to multiple distinct target URLs: \n",
      paste(error_message_parts, collapse = "\n"),
      call. = FALSE
    )
  }
  
  # Simplify redirect_map for direct lookup (now that ambiguity is checked).
  # Each from_url now maps to a single to_url (character, not list).
  # Use unique() on redirects_df before creating simple_redirect_map to handle duplicate redirect rules (e.g. A->B, A->B)
  # which are not ambiguities but would cause issues if not handled.
  unique_redirects_df <- redirects_df[!duplicated(data.frame(r_from, r_to)), , drop = FALSE]
  simple_redirect_map <- stats::setNames(as.character(unique_redirects_df[[redirect_to_col]]), 
                                         as.character(unique_redirects_df[[redirect_from_col]]))

  # --- Resolve URLs in Edge List ---
  # Convert edge list columns to character to handle factors and for consistency
  edge_from_vals <- as.character(edge_list_df[[edge_from_col]])
  edge_to_vals <- as.character(edge_list_df[[edge_to_col]])
  
  all_urls_in_edges <- unique(stats::na.omit(c(edge_from_vals, edge_to_vals)))
  
  if (length(all_urls_in_edges) == 0) {
    # Edge list contains no non-NA URLs, so nothing to resolve.
    return(edge_list_df)
  }

  # This will store final_destination_of_X = Y
  final_destinations_map <- stats::setNames(vector("character", length(all_urls_in_edges)), 
                                            all_urls_in_edges)

  for (url_to_trace in all_urls_in_edges) {
    # .trace_redirect_path is an internal util function (e.g. from utils.R)
    # It handles cycle detection and returns the final URL.
    final_destinations_map[url_to_trace] <- .trace_redirect_path(url_to_trace, simple_redirect_map)
  }

  # --- Apply Resolved URLs Back to Edge List, Preserving NAs ---
  resolved_edge_list <- edge_list_df # Start with a copy

  # Resolve 'from' column
  new_from_col <- character(length(edge_from_vals))
  na_in_from <- is.na(edge_from_vals)
  if(any(!na_in_from)){
      new_from_col[!na_in_from] <- final_destinations_map[edge_from_vals[!na_in_from]]
  }
  new_from_col[na_in_from] <- NA_character_
  resolved_edge_list[[edge_from_col]] <- new_from_col

  # Resolve 'to' column
  new_to_col <- character(length(edge_to_vals))
  na_in_to <- is.na(edge_to_vals)
  if(any(!na_in_to)){
      new_to_col[!na_in_to] <- final_destinations_map[edge_to_vals[!na_in_to]]
  }
  new_to_col[na_in_to] <- NA_character_
  resolved_edge_list[[edge_to_col]] <- new_to_col

  return(resolved_edge_list)
} 