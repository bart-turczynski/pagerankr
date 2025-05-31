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
#'   from = c("A", "B", "A", "C", "D", "E", "E", NA, "F"),
#'   to =   c("B", "C", "B", "C", "D", "E", NA, "G", "F"),
#'   value = 1:9, # Other columns should be preserved
#'   stringsAsFactors = FALSE
#' )
#' # Drop self-loops (D->D, F->F) and duplicates (A->B, C->C if C->C existed)
#' unique_edges_dropped <- get_unique_edges(edges, self_loops = "drop")
#' print(unique_edges_dropped)
#' 
#' # Keep self-loops (D->D, F->F)
#' unique_edges_kept <- get_unique_edges(edges, self_loops = "keep")
#' print(unique_edges_kept)
#'
#' # Example with explicit self-loop to demonstrate
#' edges_with_self_loop <- data.frame(from=c("X","X"), to=c("Y","X"), val=c(1,2), stringsAsFactors = FALSE)
#' get_unique_edges(edges_with_self_loop, self_loops = "drop")
#' get_unique_edges(edges_with_self_loop, self_loops = "keep")
#' 
#' # Empty df
#' get_unique_edges(data.frame(from=character(), to=character()))
get_unique_edges <- function(edge_list_df, 
                             self_loops = c("drop", "keep"),
                             from_col = "from",
                             to_col = "to") {
  
  self_loops <- match.arg(self_loops)

  if (!is.data.frame(edge_list_df) || 
      (nrow(edge_list_df) > 0 && !all(c(from_col, to_col) %in% names(edge_list_df)))) {
    stop("`edge_list_df` must be a data frame with '", from_col, "' and '", to_col, "' columns if not empty.", call. = FALSE)
  }
  
  if (nrow(edge_list_df) == 0) {
    return(edge_list_df) 
  }

  # Create a data frame with just the from and to columns as characters for robust de-duplication and self-loop check
  # This avoids issues with factors having different levels but same character representation.
  from_vals_char <- as.character(edge_list_df[[from_col]])
  to_vals_char <- as.character(edge_list_df[[to_col]])
  
  # De-duplicate based on the character representation of from and to columns
  # The `duplicated()` function works on rows of a data frame.
  deduplication_df <- data.frame(from_check = from_vals_char, to_check = to_vals_char, stringsAsFactors = FALSE)
  is_duplicate_row <- duplicated(deduplication_df)
  
  # Keep only non-duplicate rows from the original data_frame
  unique_rows_df <- edge_list_df[!is_duplicate_row, , drop = FALSE]

  if (self_loops == "drop") {
    # Identify self-loops on the de-duplicated data frame:
    # Re-extract character values from the (potentially subsetted) unique_rows_df
    from_unique_char <- as.character(unique_rows_df[[from_col]])
    to_unique_char <- as.character(unique_rows_df[[to_col]])
    
    # A self-loop requires both from and to to be non-NA and identical.
    is_self_loop <- !is.na(from_unique_char) & 
                      !is.na(to_unique_char) & 
                      (from_unique_char == to_unique_char)
    
    # is_self_loop will be FALSE if any component of the comparison is NA, which is correct.
    # No need for `is_self_loop[is.na(is_self_loop)] <- FALSE` if defined this way.
    
    result_df <- unique_rows_df[!is_self_loop, , drop = FALSE]
  } else { # self_loops == "keep"
    result_df <- unique_rows_df
  }

  return(result_df)
} 