#' @title Identify and Optionally Drop Isolated Nodes from an Edge List
#' @description From an edge list, identifies nodes with a total degree of zero
#'   (i.e., they are not a source or target of any edge). It can return a list
#'   of nodes to keep (degree > 0) or all unique nodes from the edge list.
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or as specified by `from_col`, `to_col`).
#'   This should typically be a processed edge list (e.g., after unique edges
#'   and redirect resolution).
#' @param drop Logical. If `TRUE`, returns a single-column data frame containing
#'   only node names with a total degree greater than 0. If `FALSE` (default),
#'   returns a single-column data frame of all unique node names present in the
#'   `edge_list_df` (after NA removal from source/target columns).
#' @param from_col Name of the source node column in `edge_list_df`. Default "from".
#' @param to_col Name of the target node column in `edge_list_df`. Default "to".
#' @param node_col_name Name for the output column containing node names.
#'   Default "node_name".
#'
#' @return A single-column data frame named according to `node_col_name`.
#'   If `drop = TRUE`, contains unique node names with degree > 0.
#'   If `drop = FALSE`, contains all unique node names from the input edge list.
#'   Returns an empty data frame with the correct column name if no nodes meet
#'   the criteria or if the input edge list is empty/all NAs.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", NA, "D"),
#'   to = c("B", "C", "A", "E", NA),
#'   stringsAsFactors = FALSE
#' )
#' # Nodes involved in edges are A, B, C, D, E. Assume F, G are isolates if not in edges.
#' 
#' # Get nodes with degree > 0 (A, B, C, D, E should appear)
#' active_nodes <- drop_isolates(edges, drop = TRUE)
#' print(active_nodes)
#' 
#' # Get all unique nodes mentioned in edges (A, B, C, D, E)
#' all_nodes <- drop_isolates(edges, drop = FALSE)
#' print(all_nodes)
#' 
#' # Edge list leading to some nodes being isolates
#' edges_with_isolates <- data.frame(
#'   from = c("X", "Y"), 
#'   to = c("Y", "X"), 
#'   stringsAsFactors = FALSE
#' )
#' # If we consider nodes X, Y, Z, then Z would be an isolate here.
#' # drop_isolates only knows about nodes in the edge_list_df.
#' drop_isolates(edges_with_isolates, drop = TRUE) # X, Y
#' drop_isolates(edges_with_isolates, drop = FALSE) # X, Y
#' 
#' # Empty edge list
#' empty_edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
#' drop_isolates(empty_edges, drop = TRUE)
#' drop_isolates(empty_edges, drop = FALSE)
#'
#' # Edge list with only NAs
#' na_edges <- data.frame(from = NA_character_, to = NA_character_, stringsAsFactors = FALSE)
#' drop_isolates(na_edges, drop = TRUE)
#' drop_isolates(na_edges, drop = FALSE)
#'
#' # Custom column names
#' custom_edges <- data.frame(source = c("S1"), target = c("T1"), stringsAsFactors = FALSE)
#' drop_isolates(custom_edges, from_col = "source", to_col = "target", node_col_name = "vertex")

drop_isolates <- function(edge_list_df,
                          drop = FALSE,
                          from_col = "from",
                          to_col = "to",
                          node_col_name = "node_name") {

  # --- Input Validation ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0 && !all(c(from_col, to_col) %in% names(edge_list_df))) {
    stop("`edge_list_df` must have specified 'from' and 'to' columns if not empty.", call. = FALSE)
  }
  if (!is.logical(drop) || length(drop) != 1) {
    stop("`drop` must be a single logical value.", call. = FALSE)
  }
  if (!is.character(node_col_name) || length(node_col_name) != 1 || nchar(node_col_name) == 0) {
    stop("`node_col_name` must be a non-empty single character string.", call. = FALSE)
  }

  # --- Prepare Empty Result Frame ---
  # This structure is returned if no nodes are found or input is empty.
  empty_result_df <- stats::setNames(data.frame(character(0), stringsAsFactors = FALSE),
                                     node_col_name)

  if (nrow(edge_list_df) == 0) {
    return(empty_result_df)
  }

  # --- Extract and Combine All Node Mentions ---
  # Ensure columns are character to handle factors correctly and remove NAs.
  from_nodes <- as.character(edge_list_df[[from_col]])
  to_nodes <- as.character(edge_list_df[[to_col]])

  all_mentioned_nodes <- unique(stats::na.omit(c(from_nodes, to_nodes)))

  if (length(all_mentioned_nodes) == 0) {
    # This happens if edge_list_df contained only NAs in from/to columns.
    return(empty_result_df)
  }

  # --- Determine Nodes to Return ---
  nodes_to_return <- character(0)

  if (drop) {
    # If drop = TRUE, we return nodes with degree > 0.
    # In this context, any node mentioned in a valid (non-NA in both parts) edge
    # has degree > 0 with respect to the provided edge_list_df.
    # So, all_mentioned_nodes are effectively the nodes with degree > 0.
    nodes_to_return <- all_mentioned_nodes
  } else {
    # If drop = FALSE, we return all unique nodes mentioned in the edge list.
    nodes_to_return <- all_mentioned_nodes
  }

  if (length(nodes_to_return) == 0) {
      return(empty_result_df)
  }
  
  # --- Format Output Data Frame ---
  result_df <- stats::setNames(data.frame(nodes_to_return, stringsAsFactors = FALSE),
                               node_col_name)
  
  # Sort for consistency, though not strictly required by spec (can be helpful for tests)
  if(nrow(result_df) > 0) {
      result_df <- result_df[order(result_df[[node_col_name]]), , drop = FALSE]
      row.names(result_df) <- NULL
  }
  
  return(result_df)
} 