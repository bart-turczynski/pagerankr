#' @title Resolve Redirects in an Edge List
#' @description Updates an edge list by replacing URLs with their final
#'   destinations based on a redirect data frame. Detects redirect cycles and
#'   ambiguities.
#'
#' @param edge_list_df A data frame representing the edge list.
#' @param redirects_df A data frame containing redirect rules, with 'from' and
#'   'to' columns specifying the source and target of a redirect.
#' @param edge_from_col Character, the name of the column in `edge_list_df`
#'   containing source URLs. Default "from".
#' @param edge_to_col Character, the name of the column in `edge_list_df`
#'   containing target URLs. Default "to".
#' @param redirect_from_col Character, the name of the column in `redirects_df`
#'   containing source URLs of redirects. Default "from".
#' @param redirect_to_col Character, the name of the column in `redirects_df`
#'   containing target URLs of redirects. Default "to".
#'
#' @return An updated `edge_list_df` with URLs in `edge_from_col` and
#'   `edge_to_col` replaced by their final resolved destinations.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C"),
#'   to = c("B", "C", "D"),
#'   stringsAsFactors = FALSE
#' )
#' redirects <- data.frame(
#'   from = c("B", "C", "E"),
#'   to = c("B_final", "C_final", "E_final"),
#'   stringsAsFactors = FALSE
#' )
#' resolve_redirects(edges, redirects)
#'
#' # Example with a redirect chain
#' edges_chain <- data.frame(from = "X", to = "Y", stringsAsFactors = FALSE)
#' redirects_chain <- data.frame(
#'   from = c("Y", "Z"),
#'   to = c("Z", "Z_final"),
#'   stringsAsFactors = FALSE
#' )
#' resolve_redirects(edges_chain, redirects_chain)
#'
#' # Example with different column names
#' edges_custom_names <- data.frame(source_url = "Page1", target_url = "Page2")
#' redirects_custom_names <- data.frame(original = "Page2", final = "Page2_resolved")
#' resolve_redirects(edges_custom_names, redirects_custom_names,
#'                   edge_from_col = "source_url", edge_to_col = "target_url",
#'                   redirect_from_col = "original", redirect_to_col = "final")
#'
#' # Example with NAs (NAs should be preserved)
#' edges_with_na <- data.frame(
#'  from = c("A", NA, "C"),
#'  to = c("B", "D", NA),
#'  stringsAsFactors = FALSE
#' )
#' redirects_simple <- data.frame(from = "A", to = "A_final", stringsAsFactors = FALSE)
#' resolve_redirects(edges_with_na, redirects_simple)
#'
#' # Example of error for ambiguity (uncomment to test):
#' # edges_for_amb_test <- data.frame(from = "A", to = "X", stringsAsFactors = FALSE)
#' # redirects_ambiguous <- data.frame(
#' #   from = c("A", "A"),
#' #   to = c("B", "C"),
#' #   stringsAsFactors = FALSE
#' # )
#' # try(resolve_redirects(edges_for_amb_test, redirects_ambiguous))
#'
#' # Example of error for cycle (uncomment to test):
#' # redirects_cycle <- data.frame(
#' #   from = c("L1", "L2"),
#' #   to = c("L2", "L1"),
#' #   stringsAsFactors = FALSE
#' # )
#' # edges_cycle <- data.frame(from = "Start", to = "L1", stringsAsFactors = FALSE)
#' # try(resolve_redirects(edges_cycle, redirects_cycle))

resolve_redirects <- function(edge_list_df,
                              redirects_df,
                              edge_from_col = "from",
                              edge_to_col = "to",
                              redirect_from_col = "from",
                              redirect_to_col = "to") {

  # --- Input Validation ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0 && !all(c(edge_from_col, edge_to_col) %in% names(edge_list_df))) {
    stop("`edge_list_df` must have '", edge_from_col, "' and '", edge_to_col, "' columns if not empty.", call. = FALSE)
  }

  if (!is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(redirects_df) > 0 && !all(c(redirect_from_col, redirect_to_col) %in% names(redirects_df))) {
     stop("`redirects_df` must have '", redirect_from_col, "' and '", redirect_to_col, "' columns if not empty.", call. = FALSE)
  }

  # If redirects_df is empty or has no valid rules, return edge_list_df as is.
  if (nrow(redirects_df) == 0) {
    return(edge_list_df)
  }
  
  # --- Prepare Redirect Map & Check for Ambiguities ---
  redirect_sources_raw <- redirects_df[[redirect_from_col]]
  redirect_targets_raw <- redirects_df[[redirect_to_col]]

  # Ensure redirect columns are character and handle potential factors
  redirect_sources <- as.character(redirect_sources_raw)
  redirect_targets <- as.character(redirect_targets_raw)

  valid_redirect_indices <- !is.na(redirect_sources) & !is.na(redirect_targets)
  redirect_sources <- redirect_sources[valid_redirect_indices]
  redirect_targets <- redirect_targets[valid_redirect_indices]

  if (length(redirect_sources) == 0) { # No valid redirect rules after NA removal
      return(edge_list_df)
  }
  
  # Check for ambiguities: a single 'from' URL mapping to multiple distinct 'to' URLs
  unique_src_in_rules <- unique(redirect_sources)
  for (src in unique_src_in_rules) {
    targets_for_source <- unique(redirect_targets[redirect_sources == src])
    if (length(targets_for_source) > 1) {
      stop("Ambiguous redirect: URL '", src, "' maps to multiple distinct targets: ",
           paste(targets_for_source, collapse = ", "), call. = FALSE)
    }
  }

  # Create a direct redirect map (names are sources, values are targets)
  # After the ambiguity check, we know each source maps to at most one unique target.
  # We use unique pairs of (source, target) to build the map.
  # This also handles cases where the same redirect (A->B) is listed multiple times.
  unique_redirect_pairs_df <- unique(data.frame(from = redirect_sources, to = redirect_targets, stringsAsFactors = FALSE))
  
  # The 'from' column in unique_redirect_pairs_df should now be unique due to the check above.
  redirect_map <- stats::setNames(unique_redirect_pairs_df$to, unique_redirect_pairs_df$from)

  # --- Resolve URLs in Edge List ---
  resolved_edge_list <- edge_list_df
  
  cols_to_resolve <- intersect(c(edge_from_col, edge_to_col), names(resolved_edge_list))

  # Memoization for resolved URLs within this function call
  resolved_cache <- new.env(hash = TRUE, parent = emptyenv())

  for (col_name in cols_to_resolve) {
    original_urls_in_col <- as.character(resolved_edge_list[[col_name]])
    resolved_urls_for_col <- character(length(original_urls_in_col))
    
    is_na_original <- is.na(original_urls_in_col)
    resolved_urls_for_col[is_na_original] <- NA_character_
    
    urls_to_process_in_col <- original_urls_in_col[!is_na_original]
    
    if (length(urls_to_process_in_col) > 0) {
      unique_input_urls_in_col <- unique(urls_to_process_in_col)
      
      for (url in unique_input_urls_in_col) {
          if (!exists(url, envir = resolved_cache, inherits = FALSE)) {
              final_url <- .trace_redirect_path(url = url, redirect_map = redirect_map, path = character(0))
              assign(url, final_url, envir = resolved_cache)
          }
      }
      
      # Map resolved URLs back for non-NA values
      # Get needs a vector of names if we were to use mget. Here, we lookup one by one.
      # This ensures correct assignment even with duplicates in urls_to_process_in_col
      resolved_values_for_non_na <- character(length(urls_to_process_in_col))
      for(i in seq_along(urls_to_process_in_col)){
          resolved_values_for_non_na[i] <- get(urls_to_process_in_col[i], envir = resolved_cache, inherits = FALSE)
      }
      resolved_urls_for_col[!is_na_original] <- resolved_values_for_non_na
    }
    resolved_edge_list[[col_name]] <- resolved_urls_for_col
  }

  return(resolved_edge_list)
} 