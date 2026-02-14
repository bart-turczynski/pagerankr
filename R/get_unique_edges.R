#' @title Get Unique Edges from an Edge List
#' @description Removes duplicate edge rows from an edge list data frame and
#'   provides control over how self-loops (e.g., a -> a) are handled.
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or as specified by `from_col`, `to_col`).
#' @param self_loops A character string specifying how to handle self-loops.
#'   Must be one of "drop" (default) or "keep".
#' @param from_col Name of the source node column in `edge_list_df`. Default "from".
#' @param to_col Name of the target node column in `edge_list_df`. Default "to".
#'
#' @return A data frame with unique edges, with self-loops handled according to
#'   the `self_loops` argument. The from/to columns are coerced to character; all
#'   other columns in the input are preserved (first occurrence kept on dedup).
#'   If input columns are factors, they are converted to characters in the output.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "A", "C", "D"),
#'   to = c("B", "C", "B", "C", "D"),
#'   stringsAsFactors = FALSE
#' )
#' get_unique_edges(edges, self_loops = "drop")
#' get_unique_edges(edges, self_loops = "keep")
#' 
#' # With custom column names
#' edges_custom <- data.frame(
#'   source = c("X", "Y", "X"),
#'   target = c("Y", "Y", "Y"),
#'   stringsAsFactors = FALSE
#' )
#' get_unique_edges(edges_custom, from_col="source", to_col="target")
#'
#' # With NAs (NAs are preserved as they are, duplicates involving NAs are also removed)
#' edges_na <- data.frame(
#'   from = c("A", NA, "A", "B", NA),
#'   to = c("B", "C", "B", "D", "C"),
#'   stringsAsFactors = FALSE
#' )
#' get_unique_edges(edges_na, self_loops = "keep")
#' get_unique_edges(edges_na, self_loops = "drop") # No self-loops with NA to drop
#' @details
#' Any edge where either from or to is NA is automatically dropped before deduplication and self-loop handling.

get_unique_edges <- function(edge_list_df,
                             self_loops = c("drop", "keep"),
                             from_col = "from",
                             to_col = "to") {

  # --- Argument Matching and Basic Validation ---
  self_loops <- match.arg(self_loops)

  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0 && !all(c(from_col, to_col) %in% names(edge_list_df))) {
    stop("`edge_list_df` must have specified 'from' and 'to' columns if not empty.", call. = FALSE)
  }
  
  # If edge_list_df is empty, return it as is.
  if (nrow(edge_list_df) == 0) {
    # Ensure it has the correct column names even if empty
    # This is particularly important if from_col or to_col are not the defaults "from", "to"
    # and edge_list_df was truly empty (0 rows, 0 cols initially).
    # However, the check above ensures columns exist if nrow > 0.
    # If nrow == 0, we assume it has the right structure or we return an empty df with correct names.
    if(ncol(edge_list_df) == 0 && (from_col != "from" || to_col != "to")) {
        # This case is tricky: if edge_list_df is an empty data.frame() and from_col/to_col are non-default
        # it's better to return an empty df with the right structure.
        empty_df <- data.frame(matrix(ncol = 2, nrow = 0, 
                                      dimnames = list(NULL, c(from_col, to_col))))
        empty_df[[from_col]] <- character(0)
        empty_df[[to_col]] <- character(0)
        return(empty_df)
    } else if (!all(c(from_col, to_col) %in% names(edge_list_df)) && ncol(edge_list_df) > 0){
        # Edge list has columns, but not the ones we need.
        # This should have been caught by the validation above if nrow > 0.
        # If nrow == 0, but edge_list_df has other columns, we should still make sure return
        # has the right ones.
        # For safety, if columns are missing and it's empty, reconstruct.
        # However, the above validation (nrow > 0 && !all(...)) makes this less likely.
        # If it's an empty df with wrong columns, this is a problem for subsetting later.
        # Let's assume if nrow(edge_list_df) == 0, it either has correct columns or is df().
    }
    return(edge_list_df) # Return as is if empty and columns are assumed OK or it's truly empty.
  }

  # Drop any edge where from or to is NA
  edge_list_df <- edge_list_df[!is.na(edge_list_df[[from_col]]) & !is.na(edge_list_df[[to_col]]), , drop = FALSE]

  # --- Coerce from/to to character (handles factors) ---
  edge_list_df[[from_col]] <- as.character(edge_list_df[[from_col]])
  edge_list_df[[to_col]] <- as.character(edge_list_df[[to_col]])

  # --- Handle Self-loops ---
  if (self_loops == "drop") {
    is_self_loop <- edge_list_df[[from_col]] == edge_list_df[[to_col]]
    edge_list_df <- edge_list_df[!is_self_loop, , drop = FALSE]
  }

  # If all rows were dropped, return an empty data frame preserving column structure.
  if (nrow(edge_list_df) == 0) {
    empty_result_df <- edge_list_df[FALSE, , drop = FALSE]
    row.names(empty_result_df) <- NULL
    return(empty_result_df)
  }

  # --- Remove Duplicate Edges ---
  # Deduplicate by from/to columns only, preserving all other columns
  # (first occurrence of each from/to pair is kept).
  key_cols <- c(from_col, to_col)
  unique_edges_df <- edge_list_df[!duplicated(edge_list_df[, key_cols, drop = FALSE]), , drop = FALSE]

  # Reset row names for cleaner output
  row.names(unique_edges_df) <- NULL

  return(unique_edges_df)
} 