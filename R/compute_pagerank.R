#' @title Compute PageRank using igraph
#' @description Builds a graph from a processed edge list and computes PageRank
#'   scores using `igraph::page_rank()`.
#'
#' @param edge_list_df A data frame representing the processed edge list,
#'   typically with columns "from" and "to" (or as specified by `from_col`, `to_col`).
#'   It should contain only edges to be included in the graph.
#' @param vertices_df An optional single-column data frame of node names to define
#'   the set of vertices for the graph. If `NULL` (default), all unique nodes
#'   present in `edge_list_df` are used. The column name is specified by `vertex_col_name`.
#' @param damping The damping factor for PageRank. Default is 0.85.
#' @param from_col Name of the source node column in `edge_list_df`. Default "from".
#' @param to_col Name of the target node column in `edge_list_df`. Default "to".
#' @param vertex_col_name Name of the column in `vertices_df` containing node names.
#'   Default "node_name". (This matches the output of `drop_isolates`).
#' @param pr_node_col Name for the node column in the output PageRank data frame. Default "node_name".
#' @param pr_value_col Name for the PageRank value column in the output data frame. Default "pagerank".
#' @param ... Additional arguments passed to `igraph::page_rank()`.
#'
#' @return A data frame with two columns: one for node names (named by `pr_node_col`)
#'   and one for their PageRank scores (named by `pr_value_col`), which sum to 1.
#'   Returns an empty data frame with correct columns if the graph is empty or
#'   has no nodes after processing.
#' @export
#' @import igraph
#' @examples
#' edges <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
#' pr_results <- compute_pagerank(edges)
#' print(pr_results)
#' sum(pr_results$pagerank)
#' 
#' # With specified vertices (e.g., from drop_isolates)
#' vertices <- data.frame(node_name = c("A", "B", "C", "D")) # D is an isolate
#' pr_results_isolates_kept <- compute_pagerank(edges, vertices_df = vertices)
#' print(pr_results_isolates_kept)
#' sum(pr_results_isolates_kept$pagerank)
#'
#' # Single node graph
#' single_node_edges <- data.frame(from="A", to="A") # if self-loops kept
#' compute_pagerank(single_node_edges)
#' 
#' # Single node graph, no self loop (effectively an isolate if self-loops dropped prior)
#' single_node_no_loop <- data.frame(from=character(0), to=character(0))
#' # compute_pagerank will use vertices_df if provided
#' compute_pagerank(single_node_no_loop, vertices_df = data.frame(node_name="A"))
#'
#' # Empty graph
#' empty_edges <- data.frame(from = character(), to = character())
#' compute_pagerank(empty_edges)
compute_pagerank <- function(edge_list_df, 
                             vertices_df = NULL, 
                             damping = 0.85, 
                             from_col = "from", 
                             to_col = "to",
                             vertex_col_name = "node_name",
                             pr_node_col = "node_name",
                             pr_value_col = "pagerank",
                             ...) {

  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  # Validate edge_list_df columns only if it's not empty, to allow empty graphs
  if (nrow(edge_list_df) > 0 && !all(c(from_col, to_col) %in% names(edge_list_df))) {
     stop("`edge_list_df` must have '", from_col, "' and '", to_col, "' columns if not empty.", call. = FALSE)
  }
  if (!is.null(vertices_df) && !is.data.frame(vertices_df)) {
    stop("`vertices_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.null(vertices_df) && !(vertex_col_name %in% names(vertices_df))) {
    stop("`vertices_df` must have a column named '", vertex_col_name, "'.", call. = FALSE)
  }
  if (!is.numeric(damping) || length(damping) != 1 || damping < 0 || damping > 1) {
    stop("`damping` must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  # Prepare an empty result data frame template for early exits
  empty_pr_result <- stats::setNames(data.frame(matrix(ncol = 2, nrow = 0)), c(pr_node_col, pr_value_col))
  
  # Determine the set of vertices for the graph
  graph_nodes <- NULL
  if (!is.null(vertices_df)) {
    if (nrow(vertices_df) > 0 && ncol(vertices_df) > 0) {
      graph_nodes <- as.character(vertices_df[[vertex_col_name]])
      graph_nodes <- unique(stats::na.omit(graph_nodes)) # ensure unique, non-NA
    } else {
      # vertices_df is provided but is empty or has no valid column data
      # This implies a graph with no pre-defined nodes. If edge_list_df is also empty,
      # it results in an empty graph.
      graph_nodes <- character(0)
    }
  } 
  # If vertices_df is NULL, igraph::graph_from_data_frame will infer nodes from edge_list_df.
  # If edge_list_df is also empty, it correctly creates an empty graph.

  # Handle edge case: no edges and no explicitly defined vertices through vertices_df
  # (or vertices_df was empty and edge_list_df is empty).
  if (nrow(edge_list_df) == 0 && (is.null(graph_nodes) || length(graph_nodes) == 0) ) {
    return(empty_pr_result)
  }
  
  # Create the graph
  # Ensure edge list columns are character to avoid factor issues with igraph
  # Only select from_col and to_col for graph_from_data_frame
  # Handle empty edge_list_df explicitly for graph_from_data_frame
  if (nrow(edge_list_df) > 0) {
      graph_edges_df <- edge_list_df[, c(from_col, to_col), drop = FALSE]
      graph_edges_df[[from_col]] <- as.character(graph_edges_df[[from_col]])
      graph_edges_df[[to_col]] <- as.character(graph_edges_df[[to_col]])
      
      # Filter out edges with NA in from or to, as igraph cannot handle them
      graph_edges_df <- stats::na.omit(graph_edges_df)
      
      # If after na.omit, there are no edges, but graph_nodes are defined, we build a graph with these nodes and no edges.
      if (nrow(graph_edges_df) == 0 && !is.null(graph_nodes) && length(graph_nodes) > 0) {
         current_graph <- igraph::make_empty_graph(n = length(graph_nodes), directed = TRUE)
         igraph::V(current_graph)$name <- graph_nodes
      } else if (nrow(graph_edges_df) == 0 && (is.null(graph_nodes) || length(graph_nodes) == 0) ){
         # No edges and no nodes, return empty result
         return(empty_pr_result)
      } else {
        # We have edges
        current_graph <- igraph::graph_from_data_frame(d = graph_edges_df, 
                                                       directed = TRUE, 
                                                       vertices = graph_nodes) # graph_nodes can be NULL
      }
  } else { # nrow(edge_list_df) == 0 but graph_nodes might be defined
      if (!is.null(graph_nodes) && length(graph_nodes) > 0) {
          current_graph <- igraph::make_empty_graph(n = length(graph_nodes), directed = TRUE)
          igraph::V(current_graph)$name <- graph_nodes
      } else {
          # No edges, no nodes defined. This case should be caught above, but as a safeguard:
          return(empty_pr_result)
      }
  }
  
  # If graph is empty (no vertices), igraph::page_rank might error or return trivial results.
  if (igraph::vcount(current_graph) == 0) {
    return(empty_pr_result)
  }

  # Compute PageRank
  pr_result <- tryCatch({
    igraph::page_rank(graph = current_graph, damping = damping, ...)
  }, error = function(e) {
    warning("igraph::page_rank computation failed: ", e$message, call. = FALSE)
    # Return a structure that can be processed into the expected output format, even if empty
    list(vector = stats::setNames(numeric(0), character(0)))
  })

  # Format results into a data frame
  if (length(pr_result$vector) > 0) {
    pagerank_df <- data.frame(
      node_name = names(pr_result$vector),
      pagerank = pr_result$vector,
      row.names = NULL, # Important for consistent data.frame structure
      stringsAsFactors = FALSE
    )
    # Ensure correct column names as per parameters
    names(pagerank_df) <- c(pr_node_col, pr_value_col)
    
    # Ensure PageRank sums to 1 (within tolerance) for non-empty results
    # This is more of a check/assertion for development, actual igraph output should be correct.
    # if (abs(sum(pagerank_df[[pr_value_col]]) - 1.0) > 1e-6 && sum(pagerank_df[[pr_value_col]]) != 0) { 
    #   warning("PageRank scores do not sum to 1.")
    # }
  } else {
    # Handles cases like completely disconnected graph if igraph returns empty vector, or error case
    pagerank_df <- empty_pr_result
  }

  return(pagerank_df)
} 