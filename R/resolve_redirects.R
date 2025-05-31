#' @title Resolve Redirects in an Edge List
#' @description Updates an edge list by replacing URLs with their final target
#'   URLs based on a redirect data frame. Detects cycles and ambiguities.
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or names specified by `edge_from_col` and `edge_to_col`).
#' @param redirects_df A data frame detailing redirect rules, with columns
#'   "from" and "to" (or names specified by `redirect_from_col` and `redirect_to_col`).
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
#'   from = c("A", "B", "C", "D", "E"),
#'   to = c("B", "C", "X", "Y", "A") 
#' )
#' redirects <- data.frame(
#'   from = c("B", "Y", "X"),
#'   to = c("C", "Z", "C")
#' )
#' resolved_edges <- resolve_redirects(edges, redirects)
#' print(resolved_edges)
#' 
#' # Example with a cycle
#' cyclic_redirects <- data.frame(from = c("L1", "L2"), to = c("L2", "L1"))
#' try(resolve_redirects(edges, cyclic_redirects)) # Expected to error
#' 
#' # Example with ambiguity
#' ambiguous_redirects <- data.frame(from = c("X", "X"), to = c("Y", "Z"))
#' try(resolve_redirects(edges, ambiguous_redirects)) # Expected to error
resolve_redirects <- function(edge_list_df, 
                              redirects_df,
                              edge_from_col = "from",
                              edge_to_col = "to",
                              redirect_from_col = "from",
                              redirect_to_col = "to") {

  # Input validation
  if (!is.data.frame(edge_list_df) || !all(c(edge_from_col, edge_to_col) %in% names(edge_list_df))) {
    stop("`edge_list_df` must be a data frame with specified edge columns.", call. = FALSE)
  }
  if (!is.data.frame(redirects_df) || !all(c(redirect_from_col, redirect_to_col) %in% names(redirects_df))) {
    stop("`redirects_df` must be a data frame with specified redirect columns.", call. = FALSE)
  }
  
  # Ensure no NA values in redirect mapping columns, as they are problematic for path finding
  if (any(is.na(redirects_df[[redirect_from_col]])) || any(is.na(redirects_df[[redirect_to_col]]))) {
    stop("Redirect columns ('", redirect_from_col, "', '", redirect_to_col, "') cannot contain NA values.", call. = FALSE)
  }

  # Internal helper function to find the final destination of a URL
  # This function will need to handle cycle detection and ambiguity.
  # It will be defined in utils.R or within this function if not too complex.
  # .find_final_destination <- function(url, redirect_map, path = c()) { ... }

  # Create a named list for easier lookup: source_url -> target_url(s)
  # This structure helps detect ambiguities directly.
  redirect_map_list <- split(redirects_df[[redirect_to_col]], redirects_df[[redirect_from_col]])

  # Check for ambiguities (one source URL maps to multiple distinct target URLs)
  ambiguous_urls <- names(redirect_map_list)[sapply(redirect_map_list, function(targets) length(unique(targets)) > 1)]
  if (length(ambiguous_urls) > 0) {
    stop(
      "Redirect ambiguity detected. The following URLs map to multiple final destinations: ",
      paste(ambiguous_urls, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Simplify redirect_map for direct lookup (now that ambiguity is checked)
  # Each from_url now maps to a single to_url (character, not list)
  simple_redirect_map <- sapply(redirect_map_list, `[`, 1)

  # Get all unique URLs from the edge list that need resolving
  urls_in_edges <- unique(c(edge_list_df[[edge_from_col]], edge_list_df[[edge_to_col]]))
  urls_in_edges <- stats::na.omit(urls_in_edges) # Remove NAs

  final_destinations <- stats::setNames(nm = urls_in_edges, object = character(length(urls_in_edges)))

  for (url in urls_in_edges) {
    final_destinations[url] <- .trace_redirect_path(url, simple_redirect_map)
  }

  # Apply the resolved URLs back to the edge list
  resolved_edge_list <- edge_list_df
  
  # Map 'from' column
  original_from <- resolved_edge_list[[edge_from_col]]
  is_na_from <- is.na(original_from)
  mapped_from <- final_destinations[as.character(original_from)]
  # If a URL from edge list was not in final_destinations (e.g. not in redirects, or was NA initially), it keeps its original value or NA.
  # The .trace_redirect_path function returns the URL itself if no redirect path.
  resolved_edge_list[[edge_from_col]] <- ifelse(is_na_from, NA, mapped_from)
  
  # Map 'to' column
  original_to <- resolved_edge_list[[edge_to_col]]
  is_na_to <- is.na(original_to)
  mapped_to <- final_destinations[as.character(original_to)]
  resolved_edge_list[[edge_to_col]] <- ifelse(is_na_to, NA, mapped_to)

  return(resolved_edge_list)
} 