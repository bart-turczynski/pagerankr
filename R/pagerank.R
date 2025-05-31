#' @title Master PageRank Calculation Wrapper
#' @description Orchestrates the complete PageRank calculation workflow,
#' including URL cleaning, redirect resolution, edge deduplication,
#' isolate handling, and PageRank computation.
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns like "from" and "to".
#' @param redirects_df An optional data frame for redirect rules, typically
#'   with "from" and "to" columns. Defaults to NULL.
#' @param clean_edge_urls Logical, whether to clean URLs in the edge list.
#'   Defaults to TRUE.
#' @param clean_redirect_urls Logical, whether to clean URLs in the redirect list.
#'   Defaults to TRUE.
#' @param rurl_params A list of parameters to pass to `rurl::clean_url`.
#'   Defaults to an empty list.
#' @param self_loops A character string specifying how to handle self-loops.
#'   Either "drop" (default) or "keep".
#' @param drop_isolates_flag Logical, whether to drop isolated nodes before
#'   PageRank computation. Defaults to TRUE. (Note: The spec table uses drop_isolates_flag, while acceptance criteria 6 uses this name for the pagerank() wrapper, and the table uses 'drop' for the drop_isolates() function. I'll use drop_isolates_flag for the wrapper argument for clarity).
#' @param ... Additional arguments passed to `compute_pagerank` and subsequently
#'   to `igraph::page_rank`.
#'
#' @return A data frame with node names and their PageRank scores, summing to 1.
#' @export
#' @examples
#' # Coming soon: End-to-end example
pagerank <- function(edge_list_df,
                     redirects_df = NULL,
                     clean_edge_urls = TRUE,
                     clean_redirect_urls = TRUE,
                     rurl_params = list(),
                     self_loops = c("drop", "keep"),
                     drop_isolates_flag = TRUE,
                     ...) {

  # Match arguments
  self_loops <- match.arg(self_loops)

  # 0. Argument validation (basic)
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.")
  }
  if (!is.null(redirects_df) && !is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame or NULL.")
  }
  if (!is.logical(clean_edge_urls) || length(clean_edge_urls) != 1) {
    stop("`clean_edge_urls` must be a single logical value.")
  }
  if (!is.logical(clean_redirect_urls) || length(clean_redirect_urls) != 1) {
    stop("`clean_redirect_urls` must be a single logical value.")
  }
  if (!is.list(rurl_params)) {
    stop("`rurl_params` must be a list.")
  }
  if (!is.logical(drop_isolates_flag) || length(drop_isolates_flag) != 1) {
    stop("`drop_isolates_flag` must be a single logical value.")
  }

  # Create a shared memoized cleaner instance
  # This will be refined later, for now, a placeholder or direct calls
  # .memoized_clean_url <- .create_memoized_cleaner() # Assuming .create_memoized_cleaner is in utils.R

  # 1. URL Cleaning
  # Shared cleaning if both flags are true and redirects_df is present
  # Otherwise, clean individually or not at all.
  # This logic will be more complex to handle the shared memoization correctly.

  original_edge_list_df <- edge_list_df
  original_redirects_df <- redirects_df

  # Warning for query parameters if cleaning is off for edges
  if (!clean_edge_urls) {
    # Check for '?' or '&' in 'from' or 'to' columns of edge_list_df
    # This requires a helper function, for now, conceptual
    # if (.urls_contain_query_params(edge_list_df, c("from", "to"))) {
    #   warning("URLs in `edge_list_df` may contain query parameters. Consider setting `clean_edge_urls = TRUE`.")
    # }
  }

  # Placeholder for the actual cleaning logic that uses shared memoization
  # For now, assume clean_url_columns handles its own memoization or is called directly.
  if (clean_edge_urls) {
    edge_list_df <- clean_url_columns(
      data_frame = edge_list_df,
      columns = intersect(c("from", "to"), names(edge_list_df)),
      # .memoized_clean_url = .memoized_clean_url, # Pass shared cleaner
      !!!rurl_params
    )
  }

  if (!is.null(redirects_df) && clean_redirect_urls) {
    redirects_df <- clean_url_columns(
      data_frame = redirects_df,
      columns = intersect(c("from", "to"), names(redirects_df)),
      # .memoized_clean_url = .memoized_clean_url, # Pass shared cleaner
      !!!rurl_params
    )
  }


  # 2. Redirect Resolution
  processed_edge_list <- edge_list_df
  if (!is.null(redirects_df)) {
    processed_edge_list <- resolve_redirects(
      edge_list_df = processed_edge_list,
      redirects_df = redirects_df
    )
  }

  # 3. Get Unique Edges (handles self-loops)
  processed_edge_list <- get_unique_edges(
    edge_list_df = processed_edge_list,
    self_loops = self_loops
  )

  # 4. Handle Isolates
  vertices_for_graph <- NULL # by default, igraph uses all unique nodes in edges
  if (drop_isolates_flag) {
    # drop_isolates returns a df with a single column of node names to keep
    nodes_to_keep_df <- drop_isolates(
      edge_list_df = processed_edge_list,
      drop = TRUE
    )
    # Ensure it's a character vector for igraph if not NULL
    if (nrow(nodes_to_keep_df) > 0) {
       # Assuming the column name is known or standardized by drop_isolates, e.g., "node_name"
       # For now, let's assume the first column contains the node names.
       vertices_for_graph <- nodes_to_keep_df[[1]]
    } else {
      # If all nodes are isolates and dropped, result in an empty graph / no pagerank
      # Or handle as an error/warning depending on desired behavior for empty edge lists
      # compute_pagerank should handle an empty edge list gracefully
    }
  } else {
      # If not dropping isolates, we might still want to define all unique vertices
      # explicitly for igraph, especially if some nodes appear only as sources or targets
      # and we want to ensure they are part of the graph.
      # drop_isolates with drop=FALSE returns all unique nodes.
      all_nodes_df <- drop_isolates(
        edge_list_df = processed_edge_list,
        drop = FALSE
      )
      if (nrow(all_nodes_df) > 0) {
        vertices_for_graph <- all_nodes_df[[1]] # Assuming first column
      }
  }


  # 5. Compute PageRank
  pagerank_results <- compute_pagerank(
    edge_list_df = processed_edge_list,
    vertices_df = if (!is.null(vertices_for_graph)) data.frame(node_name = vertices_for_graph) else NULL,
    ... # Pass through damping and other igraph::page_rank params
  )

  return(pagerank_results)
} 