#' @title Get Unique Edges from an Edge List
#' @description Removes duplicate edge rows from an edge list data frame and
#'   provides control over how self-loops (e.g., a -> a) are handled.
#'
#' @inheritParams compute_pagerank
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or as specified by `from_col`, `to_col`).
#' @param self_loops A character string specifying how to handle self-loops.
#'   Must be one of "drop" (default) or "keep".
#'
#' @return A data frame with unique edges, with self-loops handled according to
#'   the `self_loops` argument. The from/to columns are coerced to character;
#'   all other columns in the input are preserved (first occurrence kept on
#'   dedup). If input columns are factors, they are converted to characters in
#'   the output.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "A", "C", "D"),
#'   to = c("B", "C", "B", "C", "D")
#' )
#' get_unique_edges(edges, self_loops = "drop")
#' get_unique_edges(edges, self_loops = "keep")
#'
#' # With custom column names
#' edges_custom <- data.frame(
#'   source = c("X", "Y", "X"),
#'   target = c("Y", "Y", "Y")
#' )
#' get_unique_edges(edges_custom, from_col = "source", to_col = "target")
#'
#' # With NAs (NAs are preserved as they are,
#' # duplicates involving NAs are also removed)
#' edges_na <- data.frame(
#'   from = c("A", NA, "A", "B", NA),
#'   to = c("B", "C", "B", "D", "C")
#' )
#' get_unique_edges(edges_na, self_loops = "keep")
#' # No self-loops with NA to drop
#' get_unique_edges(edges_na, self_loops = "drop")
#' @details
#' Any edge where either from or to is NA is automatically dropped before
#' deduplication and self-loop handling.

get_unique_edges <- function(edge_list_df,
                             self_loops = c("drop", "keep"),
                             from_col = "from",
                             to_col = "to") {
  # --- Argument Matching and Basic Validation ---
  self_loops <- match.arg(self_loops)
  .gue_validate_input(edge_list_df, from_col, to_col)

  # If edge_list_df is empty, return it (reconstructing structure if needed).
  if (nrow(edge_list_df) == 0) {
    return(.gue_empty_input_result(edge_list_df, from_col, to_col))
  }

  # Drop NA edges and coerce from/to to character (handles factors).
  edge_list_df <- .gue_normalize_edges(edge_list_df, from_col, to_col)

  # --- Handle Self-loops ---
  if (self_loops == "drop") {
    edge_list_df <- .gue_drop_self_loops(edge_list_df, from_col, to_col)
  }

  # If all rows were dropped, return an empty data frame preserving column
  # structure.
  if (nrow(edge_list_df) == 0) {
    empty_result_df <- edge_list_df[FALSE, , drop = FALSE]
    row.names(empty_result_df) <- NULL
    return(empty_result_df)
  }

  # --- Remove Duplicate Edges ---
  .gue_dedup_edges(edge_list_df, from_col, to_col)
}

#' Validate the edge list input and its required columns
#' @noRd
.gue_validate_input <- function(edge_list_df, from_col, to_col) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) == 0) {
    return(invisible(NULL))
  }
  if (!all(c(from_col, to_col) %in% names(edge_list_df))) {
    stop(
      "`edge_list_df` must have specified 'from' and 'to' ",
      "columns if not empty.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Return an empty edge list, reconstructing column structure when needed
#' @noRd
.gue_empty_input_result <- function(edge_list_df, from_col, to_col) {
  # If edge_list_df is an empty data.frame() with no columns and non-default
  # column names, return an empty df with the right structure.
  needs_reconstruct <- from_col != "from" || to_col != "to"
  if (ncol(edge_list_df) == 0 && needs_reconstruct) {
    empty_df <- data.frame(matrix(
      ncol = 2, nrow = 0,
      dimnames = list(NULL, c(from_col, to_col))
    ))
    empty_df[[from_col]] <- character(0)
    empty_df[[to_col]] <- character(0)
    return(empty_df)
  }
  # Otherwise return as is (columns assumed OK or truly empty).
  edge_list_df
}

#' Drop NA edges and coerce from/to columns to character
#' @noRd
.gue_normalize_edges <- function(edge_list_df, from_col, to_col) {
  edge_list_df <- edge_list_df[
    !is.na(edge_list_df[[from_col]]) & !is.na(edge_list_df[[to_col]]), ,
    drop = FALSE
  ]
  edge_list_df[[from_col]] <- as.character(edge_list_df[[from_col]])
  edge_list_df[[to_col]] <- as.character(edge_list_df[[to_col]])
  edge_list_df
}

#' Drop self-loop rows (from == to)
#' @noRd
.gue_drop_self_loops <- function(edge_list_df, from_col, to_col) {
  is_self_loop <- edge_list_df[[from_col]] == edge_list_df[[to_col]]
  edge_list_df[!is_self_loop, , drop = FALSE]
}

#' Deduplicate by from/to pair, keeping first occurrence and resetting row names
#' @noRd
.gue_dedup_edges <- function(edge_list_df, from_col, to_col) {
  key_cols <- c(from_col, to_col)
  unique_edges_df <- edge_list_df[
    !duplicated(edge_list_df[, key_cols, drop = FALSE]), ,
    drop = FALSE
  ]
  row.names(unique_edges_df) <- NULL
  unique_edges_df
}
