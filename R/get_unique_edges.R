#' @title Get Unique Edges and Handle Self-Loops
#' @description Removes duplicate rows from an edge list data frame and provides
#'   control over how self-loops (e.g., A -> A) are handled.
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or specified by `from_col` and `to_col`).
#' @param self_loops A character string: "drop" (default) to remove self-loops,
#'   or "keep" to retain them.
#' @param from_col Name of the source node column. Default "from".
#' @param to_col Name of the target node column. Default "to".
#'
#' @return A data frame with unique edges, with self-loops handled according
#'   to the `self_loops` argument.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "A", "C", "D"),
#'   to = c("B", "C", "B", "C", "D") 
#' )
#' # Drop self-loops (D->D) and duplicates (A->B, C->C if C->C existed)
#' unique_edges_dropped <- get_unique_edges(edges, self_loops = "drop")
#' print(unique_edges_dropped)
#' 
#' # Keep self-loops (D->D)
#' unique_edges_kept <- get_unique_edges(edges, self_loops = "keep")
#' print(unique_edges_kept)
#'
#' # Example with explicit self-loop to demonstrate
#' edges_with_self_loop <- data.frame(from=c("X","X"), to=c("Y","X"))
#' get_unique_edges(edges_with_self_loop, self_loops = "drop")
#' get_unique_edges(edges_with_self_loop, self_loops = "keep")
get_unique_edges <- function(edge_list_df, 
                             self_loops = c("drop", "keep"),
                             from_col = "from",
                             to_col = "to") {
  
  self_loops <- match.arg(self_loops)

  if (!is.data.frame(edge_list_df) || !all(c(from_col, to_col) %in% names(edge_list_df))) {
    stop("`edge_list_df` must be a data frame with '", from_col, "' and '", to_col, "' columns.", call. = FALSE)
  }
  
  # Handle cases with no rows
  if (nrow(edge_list_df) == 0) {
    return(edge_list_df) 
  }

  # Remove exact duplicate rows first based on specified columns
  # Ensure 'from' and 'to' columns are used for de-duplication logic
  # Convert to character to avoid factor issues in duplicated() check if columns are factors
  deduplicated_df <- edge_list_df[!duplicated(edge_list_df[, c(from_col, to_col)]), , drop = FALSE]

  if (self_loops == "drop") {
    # Identify self-loops: rows where 'from' is the same as 'to'
    # Need to handle NAs correctly: a self-loop means from[i] == to[i] AND neither is NA.
    # If from_col or to_col can be NA, NA == NA is NA, not TRUE.
    # A more robust check for self-loops:
    is_self_loop <- (!is.na(deduplicated_df[[from_col]]) & 
                       !is.na(deduplicated_df[[to_col]]) & 
                       as.character(deduplicated_df[[from_col]]) == as.character(deduplicated_df[[to_col]]))
    
    # Ensure is_self_loop has no NAs, replace with FALSE if any arose from NA inputs
    is_self_loop[is.na(is_self_loop)] <- FALSE
    
    # Keep only rows that are NOT self-loops
    result_df <- deduplicated_df[!is_self_loop, , drop = FALSE]
  } else { # self_loops == "keep"
    result_df <- deduplicated_df
  }

  return(result_df)
} 