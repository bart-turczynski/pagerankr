#' @title Identify or Drop Isolate Nodes from an Edge List
#' @description From an edge list, identifies all unique nodes. If `drop = TRUE`,
#'   it returns only nodes with a total degree greater than zero (i.e., non-isolates).
#'   If `drop = FALSE`, it returns all unique node names present in the edge list.
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or specified by `from_col` and `to_col`).
#' @param drop Logical. If `TRUE` (default is `FALSE` as per spec table, but `TRUE` in `pagerank` wrapper context), return only nodes with degree > 0.
#'   If `FALSE`, return all unique nodes from the edge list.
#' @param from_col Name of the source node column. Default "from".
#' @param to_col Name of the target node column. Default "to".
#' @param node_col_name The name of the column in the output data frame that will
#'   contain the node names. Defaults to "node_name".
#'
#' @return A single-column data frame containing node names. If `drop = TRUE`,
#'   these are nodes with degree > 0. If `drop = FALSE`, these are all unique nodes.
#'   The column name is determined by `node_col_name`.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "Isolated", NA, "E"),
#'   to =   c("B", "C", "A", NA, "Orphan", "F")
#' )
#' 
#' # Get all unique nodes (including those that might be isolates or NAs if present)
#' all_nodes <- drop_isolates(edges, drop = FALSE)
#' print(all_nodes)
#' 
#' # Get only nodes with degree > 0 (non-isolates)
#' # Assuming "Isolated" has no edges and "Orphan" appears only once.
#' # "E" and "F" form an edge.
#' non_isolates <- drop_isolates(edges, drop = TRUE)
#' print(non_isolates)
#'
#' # Edge case: empty edge list
#' empty_edges <- data.frame(from=character(), to=character())
#' drop_isolates(empty_edges, drop=TRUE)
#' drop_isolates(empty_edges, drop=FALSE)
#'
#' # Edge case: edge list with only NAs
#' na_edges <- data.frame(from=NA_character_, to=NA_character_)
#' drop_isolates(na_edges, drop=TRUE)
#' drop_isolates(na_edges, drop=FALSE)
drop_isolates <- function(edge_list_df, 
                          drop = FALSE, 
                          from_col = "from", 
                          to_col = "to",
                          node_col_name = "node_name") {

  if (!is.data.frame(edge_list_df) || 
      (! (from_col %in% names(edge_list_df)) && nrow(edge_list_df) > 0) || 
      (! (to_col %in% names(edge_list_df)) && nrow(edge_list_df) > 0) ) {
    # Allow empty dataframes with no columns to pass, but if rows exist, columns must exist.
    if (nrow(edge_list_df) > 0) {
      stop("`edge_list_df` must be a data frame with specified '", from_col, "' and '", to_col, "' columns if not empty.", call. = FALSE)
    }
     # If it's an empty df (0 rows, potentially 0 cols), proceed to return empty df.
  }
  if (!is.logical(drop) || length(drop) != 1) {
    stop("`drop` must be a single logical value.", call. = FALSE)
  }
  if (!is.character(node_col_name) || length(node_col_name) != 1) {
    stop("`node_col_name` must be a single character string.", call. = FALSE)
  }

  # Handle empty input data frame
  if (nrow(edge_list_df) == 0) {
    result_df <- data.frame(matrix(ncol = 1, nrow = 0))
    names(result_df) <- node_col_name
    return(result_df)
  }

  all_nodes_from <- as.character(edge_list_df[[from_col]])
  all_nodes_to <- as.character(edge_list_df[[to_col]])
  
  # Get all unique, non-NA nodes involved in any edge
  unique_nodes_vector <- unique(stats::na.omit(c(all_nodes_from, all_nodes_to)))

  if (drop) {
    # If drop is TRUE, we only want nodes with degree > 0.
    # This means any node that appears in the from_col or to_col of a valid edge.
    # Our unique_nodes_vector already contains only non-NA nodes from edges.
    # So, these are effectively the nodes with degree > 0.
    
    # If unique_nodes_vector is empty (e.g. edge_list_df had only NAs or was empty after na.omit)
    if (length(unique_nodes_vector) == 0) {
      result_df <- data.frame(matrix(ncol = 1, nrow = 0))
      names(result_df) <- node_col_name
      return(result_df)
    }
    
    nodes_to_keep <- unique_nodes_vector
    
  } else {
    # If drop is FALSE, return all unique nodes from the edge list (including those that might be isolates)
    # The spec says: "returns a data.frame of all unique node names from edge_list_df"
    # This implies considering NAs as potential node names if they are not omitted earlier.
    # However, na.omit was used above. For consistency with igraph behavior (which typically ignores NAs or treats them as errors),
    # it is safer to return only non-NA unique nodes.
    # If NAs were to be treated as actual nodes, the spec would need to be clearer.
    # The current unique_nodes_vector (after na.omit) is appropriate here too.
    nodes_to_keep <- unique_nodes_vector
  }
  
  # Construct the result data frame
  if (length(nodes_to_keep) > 0) {
    result_df <- data.frame(nodes_to_keep, stringsAsFactors = FALSE)
  } else {
    result_df <- data.frame(matrix(ncol = 1, nrow = 0))
  }
  names(result_df) <- node_col_name
  
  return(result_df)
} 