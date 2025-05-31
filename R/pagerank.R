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
#'   Defaults to TRUE. Only effective if `redirects_df` is provided.
#' @param rurl_params A list of parameters to pass to `rurl::clean_url`.
#'   Defaults to an empty list.
#' @param self_loops A character string specifying how to handle self-loops.
#'   Either "drop" (default) or "keep".
#' @param drop_isolates_flag Logical, whether to drop isolated nodes before
#'   PageRank computation. Defaults to TRUE.
#' @param edge_from_col,edge_to_col Names of from/to columns in `edge_list_df`.
#' @param redirect_from_col,redirect_to_col Names of from/to columns in `redirects_df`.
#' @param ... Additional arguments passed to `compute_pagerank` and subsequently
#'   to `igraph::page_rank` (e.g., `damping`).
#'
#' @return A data frame with node names and their PageRank scores, summing to 1
#'   for non-empty graphs.
#' @export
#' @examples
#' # Basic example
#' edges <- data.frame(
#'   from = c("http://A.com/", "B", "C?q=1", "D"), 
#'   to = c("B", "http://A.com", "D#frag", "D"),
#'   stringsAsFactors = FALSE
#' )
#' redirects <- data.frame(
#'   from = c("C?q=1", "B"), 
#'   to = c("http://C_resolved.com", "A"), # B redirects to A, C to C_resolved
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Run full pipeline
#' pr_full <- pagerank(edges, redirects_df = redirects, self_loops="drop", drop_isolates_flag=TRUE)
#' print(pr_full)
#' 
#' # Run without URL cleaning for edges (warning expected if query params present)
#' pr_no_edge_clean <- pagerank(edges, redirects_df = redirects, clean_edge_urls = FALSE)
#' print(pr_no_edge_clean)
#' 
#' # Keep isolates
#' edges_isol <- rbind(edges, data.frame(from="ISO", to="LAND"))
#' pr_keep_isolates <- pagerank(edges_isol, drop_isolates_flag = FALSE)
#' print(pr_keep_isolates)

# Declare package-internal functions as global for linter satisfaction
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".create_memoized_cleaner", ".urls_contain_query_params", # from utils.R
    "clean_url_columns", 
    "resolve_redirects", 
    "get_unique_edges", 
    "drop_isolates", 
    "compute_pagerank"
  ))
}

pagerank <- function(edge_list_df,
                     redirects_df = NULL,
                     clean_edge_urls = TRUE,
                     clean_redirect_urls = TRUE,
                     rurl_params = list(),
                     self_loops = c("drop", "keep"),
                     drop_isolates_flag = TRUE,
                     edge_from_col = "from",
                     edge_to_col = "to",
                     redirect_from_col = "from",
                     redirect_to_col = "to",
                     ...) {

  # --- Argument Matching and Basic Validation ---
  self_loops <- match.arg(self_loops)

  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  # Further column checks within functions called.

  if (!is.null(redirects_df) && !is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.logical(clean_edge_urls) || length(clean_edge_urls) != 1) {
    stop("`clean_edge_urls` must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(clean_redirect_urls) || length(clean_redirect_urls) != 1) {
    stop("`clean_redirect_urls` must be a single logical value.", call. = FALSE)
  }
  if (!is.list(rurl_params)) {
    stop("`rurl_params` must be a list.", call. = FALSE)
  }
  if (!is.logical(drop_isolates_flag) || length(drop_isolates_flag) != 1) {
    stop("`drop_isolates_flag` must be a single logical value.", call. = FALSE)
  }
  # Dots for igraph params are handled by compute_pagerank directly.

  # --- Initialize working copies of data frames ---
  current_edge_list <- edge_list_df
  current_redirects_list <- redirects_df

  # --- 1. URL Cleaning (Potentially Shared Memoization) ---
  # As per Spec: "ensures that all unique URLs from both the edge list and 
  # redirect list are canonicalized *once* per unique string using a shared 
  # memoized `rurl::clean_url` instance"
  
  # Determine edge and redirect columns for cleaning
  edge_url_cols <- intersect(c(edge_from_col, edge_to_col), names(current_edge_list))
  redirect_url_cols <- if (!is.null(current_redirects_list)) intersect(c(redirect_from_col, redirect_to_col), names(current_redirects_list)) else character(0)

  shared_cleaner <- NULL
  # Condition for shared cleaning: both flags TRUE, redirects present, and columns exist for cleaning
  use_shared_cleaning <- clean_edge_urls && clean_redirect_urls && 
                         !is.null(current_redirects_list) && nrow(current_redirects_list) > 0 &&
                         length(edge_url_cols) > 0 && length(redirect_url_cols) > 0

  if (use_shared_cleaning) {
    shared_cleaner <- .create_memoized_cleaner()
    
    if (length(edge_url_cols) > 0) {
        current_edge_list <- do.call(clean_url_columns, 
                                     c(list(data_frame = current_edge_list, 
                                            columns = edge_url_cols, 
                                            .memoized_clean_url = shared_cleaner), 
                                       rurl_params))
    }
    if (length(redirect_url_cols) > 0) {
        current_redirects_list <- do.call(clean_url_columns, 
                                          c(list(data_frame = current_redirects_list, 
                                                 columns = redirect_url_cols, 
                                                 .memoized_clean_url = shared_cleaner), rurl_params))
    }
  } else {
    # No shared cleaning, apply individually if flags are set
    if (clean_edge_urls && length(edge_url_cols) > 0) {
      current_edge_list <- do.call(clean_url_columns, 
                                   c(list(data_frame = current_edge_list, 
                                          columns = edge_url_cols), rurl_params)) # Uses its own memoizer
    }
    if (clean_redirect_urls && !is.null(current_redirects_list) && nrow(current_redirects_list) > 0 && length(redirect_url_cols) > 0) {
      current_redirects_list <- do.call(clean_url_columns, 
                                        c(list(data_frame = current_redirects_list, 
                                               columns = redirect_url_cols), rurl_params)) # Uses its own memoizer
    }
  }

  # --- Warning for Uncleaned Edge URLs with Query Parameters ---
  # Spec: "If clean_edge_urls = FALSE and URLs in the edge_list_df contain query parameters (`?` or `&`), warn users"
  if (!clean_edge_urls && length(edge_url_cols) > 0) {
    if (.urls_contain_query_params(current_edge_list, columns = edge_url_cols)) {
      warning("URLs in `edge_list_df` may contain query parameters (e.g. '?’ or '&'). ",
              "Consider setting `clean_edge_urls = TRUE` for consistent PageRank calculation, using `rurl_params` to control `rurl::clean_url` behavior if needed.", 
              call. = FALSE)
    }
  }

  # --- 2. Redirect Resolution ---
  if (!is.null(current_redirects_list) && nrow(current_redirects_list) > 0) {
    current_edge_list <- resolve_redirects(
      edge_list_df = current_edge_list, 
      redirects_df = current_redirects_list,
      edge_from_col = edge_from_col, edge_to_col = edge_to_col,
      redirect_from_col = redirect_from_col, redirect_to_col = redirect_to_col
    )
  }

  # --- 3. Get Unique Edges (handles self-loops) ---
  current_edge_list <- get_unique_edges(
    edge_list_df = current_edge_list,
    self_loops = self_loops,
    from_col = edge_from_col,
    to_col = edge_to_col
  )

  # --- 4. Handle Isolates ---
  vertices_for_pagerank_df <- NULL # Input for compute_pagerank's vertices_df argument
  
  # Determine the node column name for the output of drop_isolates and input to compute_pagerank
  # This should be consistent. Let's use a fixed internal temporary name or ensure it matches.
  # compute_pagerank expects a column named "node_name" by default in its vertices_df if provided.
  temp_node_col_name_for_isolates <- "node_name" # Standardized name for this intermediate step

  if (drop_isolates_flag) {
    nodes_to_keep_df <- drop_isolates(
      edge_list_df = current_edge_list,
      drop = TRUE,
      from_col = edge_from_col,
      to_col = edge_to_col,
      node_col_name = temp_node_col_name_for_isolates
    )
    if (nrow(nodes_to_keep_df) > 0) {
      vertices_for_pagerank_df <- nodes_to_keep_df
    } else {
      # All nodes are isolates and dropped, or current_edge_list was empty.
      # Create an empty data frame with the correct column name for compute_pagerank
      vertices_for_pagerank_df <- data.frame(matrix(ncol=1, nrow=0, dimnames=list(NULL, temp_node_col_name_for_isolates)))
      vertices_for_pagerank_df[[temp_node_col_name_for_isolates]] <- character(0)
    }
  } else {
    # If not dropping isolates, get all unique nodes to define the graph vertices.
    all_nodes_df <- drop_isolates(
      edge_list_df = current_edge_list,
      drop = FALSE,
      from_col = edge_from_col,
      to_col = edge_to_col,
      node_col_name = temp_node_col_name_for_isolates
    )
    if (nrow(all_nodes_df) > 0) {
      vertices_for_pagerank_df <- all_nodes_df
    } else {
      # current_edge_list was empty or contained only NAs.
      vertices_for_pagerank_df <- data.frame(matrix(ncol=1, nrow=0, dimnames=list(NULL, temp_node_col_name_for_isolates)))
      vertices_for_pagerank_df[[temp_node_col_name_for_isolates]] <- character(0)
    }
  }
  # If vertices_for_pagerank_df ends up with 0 rows, compute_pagerank should handle it.
  # It will be NULL if it has 0 rows and passed as data.frame(node_name=character(0)) to compute_pagerank.
  # Let's ensure it's NULL if empty, or correctly structured if not.
  if(is.data.frame(vertices_for_pagerank_df) && nrow(vertices_for_pagerank_df) == 0){
      vertices_for_pagerank_df <- NULL # compute_pagerank handles NULL vertices_df to infer from edges or make empty graph.
  }


  # --- 5. Compute PageRank ---
  # `...` will pass through arguments like `damping` if not explicitly set above, or other igraph args.
  # compute_pagerank uses "node_name" as default for vertex_col_name, matching temp_node_col_name_for_isolates.
  pagerank_results <- compute_pagerank(
    edge_list_df = current_edge_list,
    vertices_df = vertices_for_pagerank_df, 
    from_col = edge_from_col,
    to_col = edge_to_col,
    vertex_col_name = temp_node_col_name_for_isolates, # Explicitly pass the col name used by drop_isolates
    # pr_node_col and pr_value_col for output naming are handled by compute_pagerank defaults or can be passed in ...
    ...
  )

  return(pagerank_results)
} 