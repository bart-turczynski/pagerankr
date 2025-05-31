#' @title Identify or Drop Isolate Nodes from an Edge List
#' @description From an edge list, identifies all unique nodes. If `drop = TRUE`,
#'   it returns only nodes with a total degree greater than zero (i.e., non-isolates).
#'   If `drop = FALSE`, it returns all unique node names present in the edge list
#'   (excluding NAs).
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or specified by `from_col` and `to_col`).
#' @param drop Logical. If `TRUE`, return only nodes with degree > 0. 
#'   If `FALSE` (default), return all unique non-NA nodes from the edge list.
#' @param from_col Name of the source node column. Default "from".
#' @param to_col Name of the target node column. Default "to".
#' @param node_col_name The name of the column in the output data frame that will
#'   contain the node names. Defaults to "node_name".
#'
#' @return A single-column data frame containing node names. If `drop = TRUE`,
#'   these are non-NA nodes with degree > 0. If `drop = FALSE`, these are all unique non-NA nodes.
#'   The column name is determined by `node_col_name`.
#' @export
#' @examples
#' edges1 <- data.frame(
#'   from = c("A", "B", "C", "Isolated", NA, "E"),
#'   to =   c("B", "C", "A", NA, "Orphan", "F"),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Get all unique non-NA nodes 
#' drop_isolates(edges1, drop = FALSE) # Should include A, B, C, E, F, Isolated, Orphan
#' 
#' # Get only nodes with degree > 0 (non-isolates)
#' drop_isolates(edges1, drop = TRUE)  # Should include A, B, C, E, F (Isolated, Orphan dropped if only with NA)
#' 
#' # Verify: 'Isolated' only appears with NA in 'to', 'Orphan' only appears with NA in 'from'.
#' # Depending on interpretation, stats::na.omit(c(from,to)) might remove them from degree consideration.
#' # Let's test more explicitly.
#' edges2 <- data.frame(from=c("X","Y"), to=c("Y",NA), stringsAsFactors = FALSE)
#' drop_isolates(edges2, drop=TRUE) # X, Y
#' drop_isolates(edges2, drop=FALSE) # X, Y
#' 
#' edges3 <- data.frame(from=c("Z", NA), to=c(NA, "W"), stringsAsFactors = FALSE)
#' drop_isolates(edges3, drop=TRUE) # Z, W
#' drop_isolates(edges3, drop=FALSE) # Z, W
#'
#' # Edge case: empty edge list
#' empty_edges <- data.frame(from=character(), to=character(), stringsAsFactors = FALSE)
#' drop_isolates(empty_edges, drop = TRUE)
#' drop_isolates(empty_edges, drop = FALSE)
#'
#' # Edge case: edge list with only NAs
#' na_edges <- data.frame(from=NA_character_, to=NA_character_, stringsAsFactors = FALSE)
#' drop_isolates(na_edges, drop = TRUE)
#' drop_isolates(na_edges, drop = FALSE)
#'
#' # Edge case: one node connected to NA, another is truly isolated by not appearing
#' edges4 <- data.frame(from=c("K", "L"), to=c(NA, "M"), stringsAsFactors = FALSE)
#' # If only L-M is a valid edge, K is an isolate for drop=TRUE.
#' # For drop=FALSE, K, L, M are all unique non-NA nodes mentioned.
#' drop_isolates(edges4, drop=TRUE) # Expect L, M
#' drop_isolates(edges4, drop=FALSE) # Expect K, L, M
drop_isolates <- function(edge_list_df, 
                          drop = FALSE, 
                          from_col = "from", 
                          to_col = "to",
                          node_col_name = "node_name") {

  if (!is.data.frame(edge_list_df)) {
      stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  # Check for column existence only if data frame is not empty
  if (nrow(edge_list_df) > 0) {
    if (!is.character(from_col) || length(from_col) != 1 || !(from_col %in% names(edge_list_df))){
        stop("`from_col` must be a single string and an existing column name in non-empty `edge_list_df`.", call. = FALSE)
    }
    if (!is.character(to_col) || length(to_col) != 1 || !(to_col %in% names(edge_list_df))){
        stop("`to_col` must be a single string and an existing column name in non-empty `edge_list_df`.", call. = FALSE)
    }
  }
  if (!is.logical(drop) || length(drop) != 1) {
    stop("`drop` must be a single logical value.", call. = FALSE)
  }
  if (!is.character(node_col_name) || length(node_col_name) != 1 || nchar(node_col_name) == 0) {
    stop("`node_col_name` must be a single non-empty character string.", call. = FALSE)
  }

  # Prepare empty result data frame template
  nodes_to_keep <- character(0)

  if (nrow(edge_list_df) > 0) {
    all_nodes_from <- as.character(edge_list_df[[from_col]])
    all_nodes_to <- as.character(edge_list_df[[to_col]])
    
    # Get all unique, non-NA nodes mentioned in either column.
    # These are candidates for being included.
    unique_nodes_mentioned <- unique(stats::na.omit(c(all_nodes_from, all_nodes_to)))

    if (drop) {
      # If drop is TRUE, we only want nodes that form part of a valid edge (degree > 0).
      # A valid edge has non-NA nodes in both its from and to positions.
      # So, collect all nodes that appear in such valid edges.
      valid_edges_from <- all_nodes_from[!is.na(all_nodes_from) & !is.na(all_nodes_to)]
      valid_edges_to <- all_nodes_to[!is.na(all_nodes_from) & !is.na(all_nodes_to)]
      nodes_with_degree_gt_zero <- unique(c(valid_edges_from, valid_edges_to))
      nodes_to_keep <- nodes_with_degree_gt_zero
    } else {
      # If drop is FALSE, return all unique non-NA nodes mentioned anywhere.
      nodes_to_keep <- unique_nodes_mentioned
    }
  }
  # If edge_list_df is empty, nodes_to_keep remains character(0)
  
  # Construct the result data frame
  if (length(nodes_to_keep) > 0) {
    result_df <- data.frame(nodes_to_keep, stringsAsFactors = FALSE)
  } else {
    # Create an empty data frame with the specified column name
    result_df <- data.frame(matrix(ncol = 1, nrow = 0, 
                                 dimnames = list(NULL, node_col_name)))
    # Ensure the type of the empty column is character if possible, though data.frame() might make it logical
    # Forcing it here if it became logical (common for 0-row, 1-col matrix from data.frame())
    if (ncol(result_df) == 1 && is.logical(result_df[[1]])) {
        result_df[[node_col_name]] <- character(0)
    }
  }
  # Set names again in case the above construction didn't persist it for 0-row matrix case
  names(result_df) <- node_col_name 
  
  return(result_df)
} 