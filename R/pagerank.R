#' @title Master PageRank Calculation Wrapper
#' @description Orchestrates the complete PageRank calculation workflow,
#' including URL cleaning, redirect resolution, edge deduplication,
#' indexability handling, nofollow handling, isolate handling, and
#' PageRank computation.
#' @name pagerank
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
#' @param weight_col Optional name of a numeric column in `edge_list_df`
#'   containing edge weights. Higher weights make edges more likely to be
#'   followed. If `NULL` (default), all edges have equal weight.
#' @param nofollow_col Optional name of a logical or 0/1 column in
#'   `edge_list_df` indicating nofollow edges. If `NULL` (default),
#'   no nofollow handling is performed.
#' @param nofollow_action How to handle nofollow edges when `nofollow_col` is
#'   provided. One of:
#'   \describe{
#'     \item{`"evaporate"`}{(default, Google-like) Nofollow links consume their
#'       share of the source node's outgoing PR budget but pass nothing to the
#'       target. Implemented via a sink node that absorbs the wasted PR.}
#'     \item{`"drop"`}{Remove nofollow edges entirely. Follow edges share the
#'       full PR budget among themselves.}
#'     \item{`"keep"`}{Treat nofollow edges identically to follow edges.}
#'   }
#' @param indexability_df Optional data frame mapping URLs to their indexability
#'   status (e.g., from an SEO crawl export). See Details.
#' @param indexability_url_col Name of the URL column in `indexability_df`.
#'   Default `"url"`.
#' @param indexability_status_col Name of the status column in
#'   `indexability_df`. Default `"indexability_status"`. Values are
#'   comma-separated strings; recognized statuses are `"Blocked by robots.txt"`
#'   and `"noindex"` (case-insensitive for noindex).
#' @param robots_blocked_action How to present robots.txt-blocked pages in
#'   results. One of:
#'   \describe{
#'     \item{`"trap"`}{(default) Blocked pages appear in results showing their
#'       accumulated (trapped) PageRank, useful for seeing wasted PR.}
#'     \item{`"vanish"`}{Blocked pages are removed from results. Their PR
#'       disappears.}
#'   }
#' @param edge_from_col,edge_to_col Names of from/to columns in `edge_list_df`.
#' @param redirect_from_col,redirect_to_col Names of from/to columns in
#'   `redirects_df`.
#' @param duplicate_from_policy How to handle conflicting redirects in
#'   `redirects_df`. Passed through to [resolve_redirects()]. Default
#'   `"strict"` (error on conflicts). See [resolve_redirects()] for all
#'   available policies.
#' @param loop_handling How to handle redirect cycles. Passed through to
#'   [resolve_redirects()]. Default `"error"`. See [resolve_redirects()] for
#'   all available policies.
#' @param keep_domains Optional character vector of domains to keep. When
#'   provided, edges are filtered via [filter_links_by_domain()] before
#'   PageRank calculation so that only links where both endpoints belong
#'   to one of the specified domains are included. Useful for restricting
#'   to internal links. Default `NULL` (no domain filtering).
#' @param exclude_domains Optional character vector of domains to exclude.
#'   Edges where either endpoint belongs to one of these domains are removed.
#'   Default `NULL` (no exclusion).
#' @param ... Additional arguments passed to `compute_pagerank` and subsequently
#'   to `igraph::page_rank` (e.g., `damping`).
#'
#' @details
#' ## Indexability handling
#'
#' When `indexability_df` is provided, two types of pages receive special
#' treatment:
#'
#' **noindex pages:** All outgoing links from a noindex page are treated as
#' nofollow (the page is not in Google's index, so it cannot pass PageRank).
#' These edges are then processed by the nofollow mechanism according to
#' `nofollow_action`.
#'
#' **robots.txt-blocked pages:** Google cannot access the page content, so
#' there are no visible outgoing links. All outgoing edges are removed and a
#' self-loop is added to trap inbound PageRank. The `robots_blocked_action`
#' parameter controls whether these pages appear in results (`"trap"`) or
#' are removed (`"vanish"`).
#'
#' **Priority rule:** robots.txt always takes precedence over noindex. If a
#' page is both robots-blocked and noindex, it is treated as robots-blocked.
#'
#' @return A data frame with node names and their PageRank scores. When
#'   nofollow evaporation, indexability handling, or `robots_blocked_action =
#'   "vanish"` is active, scores may sum to less than 1 (the difference is
#'   the wasted/evaporated share).
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
#'
#' # With nofollow edges (evaporate mode)
#' edges_nf <- data.frame(
#'   from = c("A", "A", "B"), to = c("B", "C", "A"),
#'   nofollow = c(FALSE, TRUE, FALSE), stringsAsFactors = FALSE
#' )
#' pr_nf <- pagerank(edges_nf, nofollow_col = "nofollow",
#'                   nofollow_action = "evaporate", clean_edge_urls = FALSE)
#' print(pr_nf)

pagerank <- function(edge_list_df,
                     redirects_df = NULL,
                     clean_edge_urls = TRUE,
                     clean_redirect_urls = TRUE,
                     rurl_params = list(),
                     self_loops = c("drop", "keep"),
                     drop_isolates_flag = TRUE,
                     weight_col = NULL,
                     nofollow_col = NULL,
                     nofollow_action = c("evaporate", "drop", "keep"),
                     indexability_df = NULL,
                     indexability_url_col = "url",
                     indexability_status_col = "indexability_status",
                     robots_blocked_action = c("trap", "vanish"),
                     edge_from_col = "from",
                     edge_to_col = "to",
                     redirect_from_col = "from",
                     redirect_to_col = "to",
                     duplicate_from_policy = c("strict",
                                               "first_wins",
                                               "last_wins",
                                               "most_frequent",
                                               "prune_source",
                                               "resolve_if_consistent"),
                     loop_handling = c("error",
                                       "prune_loop",
                                       "break_arrow"),
                     keep_domains = NULL,
                     exclude_domains = NULL,
                     ...) {

  # --- Argument Matching and Basic Validation ---
  self_loops <- match.arg(self_loops)
  nofollow_action <- match.arg(nofollow_action)
  robots_blocked_action <- match.arg(robots_blocked_action)
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)

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

  # Validate weight_col
  if (!is.null(weight_col)) {
    if (!is.character(weight_col) || length(weight_col) != 1) {
      stop("`weight_col` must be a single character string or NULL.", call. = FALSE)
    }
    if (nrow(edge_list_df) > 0 && !(weight_col %in% names(edge_list_df))) {
      stop("`weight_col` '", weight_col, "' not found in `edge_list_df`.", call. = FALSE)
    }
  }

  # Validate nofollow_col
  if (!is.null(nofollow_col)) {
    if (!is.character(nofollow_col) || length(nofollow_col) != 1) {
      stop("`nofollow_col` must be a single character string or NULL.", call. = FALSE)
    }
    if (nrow(edge_list_df) > 0 && !(nofollow_col %in% names(edge_list_df))) {
      stop("`nofollow_col` '", nofollow_col, "' not found in `edge_list_df`.", call. = FALSE)
    }
  }

  # Validate indexability_df
  if (!is.null(indexability_df)) {
    if (!is.data.frame(indexability_df)) {
      stop("`indexability_df` must be a data frame or NULL.", call. = FALSE)
    }
    if (nrow(indexability_df) > 0) {
      if (!(indexability_url_col %in% names(indexability_df))) {
        stop("`indexability_url_col` '", indexability_url_col,
             "' not found in `indexability_df`.", call. = FALSE)
      }
      if (!(indexability_status_col %in% names(indexability_df))) {
        stop("`indexability_status_col` '", indexability_status_col,
             "' not found in `indexability_df`.", call. = FALSE)
      }
    }
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

  # Default rurl_params for internal consistency if not overridden by user for protocol handling
  effective_rurl_params <- rurl_params
  if (is.null(effective_rurl_params$protocol_handling)) {
    effective_rurl_params$protocol_handling <- "http" # Ensure schemes for consistency, valid for rurl
  }
  # rurl::get_clean_url will apply its own defaults for other params like case_handling, www_handling, etc., if not in rurl_params.

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
                                       effective_rurl_params))
    }
    if (length(redirect_url_cols) > 0) {
        current_redirects_list <- do.call(clean_url_columns, 
                                          c(list(data_frame = current_redirects_list, 
                                                 columns = redirect_url_cols, 
                                                 .memoized_clean_url = shared_cleaner), effective_rurl_params))
    }
  } else {
    # No shared cleaning, apply individually if flags are set
    if (clean_edge_urls && length(edge_url_cols) > 0) {
      current_edge_list <- do.call(clean_url_columns, 
                                   c(list(data_frame = current_edge_list, 
                                          columns = edge_url_cols), effective_rurl_params))
    }
    if (clean_redirect_urls && !is.null(current_redirects_list) && nrow(current_redirects_list) > 0 && length(redirect_url_cols) > 0) {
      current_redirects_list <- do.call(clean_url_columns, 
                                        c(list(data_frame = current_redirects_list, 
                                               columns = redirect_url_cols), effective_rurl_params))
    }
  }

  # --- Warning for Uncleaned Edge URLs with Query Parameters ---
  # Spec: "If clean_edge_urls = FALSE and URLs in the edge_list_df contain query parameters (`?` or `&`), warn users"
  if (!clean_edge_urls && length(edge_url_cols) > 0) {
    if (.urls_contain_query_params(current_edge_list, columns = edge_url_cols)) {
      warning("URLs in `edge_list_df` may contain query parameters (e.g. '?' or '&'). ",
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
      redirect_from_col = redirect_from_col, redirect_to_col = redirect_to_col,
      duplicate_from_policy = duplicate_from_policy,
      loop_handling = loop_handling
    )
  }

  # --- 2.7. Domain filtering ---
  if (!is.null(keep_domains) || !is.null(exclude_domains)) {
    current_edge_list <- filter_links_by_domain(
      edge_list_df = current_edge_list,
      from_col = edge_from_col,
      to_col = edge_to_col,
      keep_domains = keep_domains,
      ignore_domains = exclude_domains
    )
  }

  # --- 2.5. Extract full vertex universe (before NA rows are stripped) ---
  # This must happen before get_unique_edges() which drops rows with NAs.
  # When drop_isolates_flag = FALSE, nodes from partial rows (one NA column)
  # represent known URLs that should be included in the graph as isolates.
  temp_node_col_name <- "node_name" # Standardized name for intermediate vertex data
  all_vertex_universe <- unique(stats::na.omit(c(
    as.character(current_edge_list[[edge_from_col]]),
    as.character(current_edge_list[[edge_to_col]])
  )))

  # --- 3. Get Unique Edges (handles self-loops) ---
  current_edge_list <- get_unique_edges(
    edge_list_df = current_edge_list,
    self_loops = self_loops,
    from_col = edge_from_col,
    to_col = edge_to_col
  )

  # --- 3.5. Indexability handling ---
  # Must come after dedup (step 3) but before nofollow (step 3.6) so that

  # noindex-derived nofollow edges are picked up by the nofollow mechanism.
  robots_blocked_urls <- character(0)
  if (!is.null(indexability_df) && nrow(indexability_df) > 0 && nrow(current_edge_list) > 0) {
    statuses <- as.character(indexability_df[[indexability_status_col]])
    urls <- as.character(indexability_df[[indexability_url_col]])

    # Parse statuses: robots.txt takes priority over noindex
    is_robots_blocked <- grepl("Blocked by robots.txt", statuses, fixed = TRUE)
    is_noindex <- grepl("noindex", statuses, ignore.case = TRUE) & !is_robots_blocked

    noindex_urls <- urls[is_noindex]
    robots_blocked_urls <- urls[is_robots_blocked]

    # --- noindex pages: mark all outgoing edges as nofollow ---
    if (length(noindex_urls) > 0) {
      from_is_noindex <- current_edge_list[[edge_from_col]] %in% noindex_urls
      if (any(from_is_noindex)) {
        # Create nofollow column if it doesn't exist
        if (is.null(nofollow_col)) {
          nofollow_col <- "__pr_nofollow__"
          current_edge_list[[nofollow_col]] <- FALSE
        } else if (!(nofollow_col %in% names(current_edge_list))) {
          current_edge_list[[nofollow_col]] <- FALSE
        }
        current_edge_list[[nofollow_col]][from_is_noindex] <- TRUE
      }
    }

    # --- robots-blocked pages: remove outgoing edges, add self-loop ---
    if (length(robots_blocked_urls) > 0) {
      from_is_blocked <- current_edge_list[[edge_from_col]] %in% robots_blocked_urls
      if (any(from_is_blocked)) {
        # Remove all outgoing edges from blocked pages
        current_edge_list <- current_edge_list[!from_is_blocked, , drop = FALSE]

        # Add self-loops for each blocked URL that exists as a source
        blocked_with_edges <- unique(robots_blocked_urls[robots_blocked_urls %in%
          current_edge_list[[edge_from_col]] | robots_blocked_urls %in%
          current_edge_list[[edge_to_col]] | robots_blocked_urls %in%
          all_vertex_universe])
        if (length(blocked_with_edges) > 0) {
          # Build self-loop rows matching the edge list structure
          self_loop_df <- stats::setNames(
            data.frame(blocked_with_edges, blocked_with_edges,
                       stringsAsFactors = FALSE),
            c(edge_from_col, edge_to_col)
          )
          # Fill extra columns with appropriate defaults
          extra_cols <- setdiff(names(current_edge_list), c(edge_from_col, edge_to_col))
          for (col in extra_cols) {
            if (is.logical(current_edge_list[[col]])) {
              self_loop_df[[col]] <- FALSE
            } else if (is.numeric(current_edge_list[[col]])) {
              self_loop_df[[col]] <- 1
            } else {
              self_loop_df[[col]] <- NA
            }
          }
          current_edge_list <- rbind(current_edge_list, self_loop_df)
        }
      }
    }
  }

  # --- 3.6. Nofollow handling ---
  nofollow_sink_name <- "__pr_nofollow_sink__"
  used_nofollow_sink <- FALSE

  if (!is.null(nofollow_col) && nofollow_col %in% names(current_edge_list) &&
      nrow(current_edge_list) > 0) {

    # Coerce nofollow column to logical
    nf_vals <- current_edge_list[[nofollow_col]]
    if (is.numeric(nf_vals)) nf_vals <- as.logical(nf_vals)
    nf_mask <- !is.na(nf_vals) & nf_vals

    if (any(nf_mask)) {
      if (nofollow_action == "drop") {
        # Simply remove nofollow edges
        current_edge_list <- current_edge_list[!nf_mask, , drop = FALSE]

      } else if (nofollow_action == "evaporate") {
        # Redirect nofollow edge targets to the sink node
        current_edge_list[[edge_to_col]][nf_mask] <- nofollow_sink_name

        # Add a self-loop on the sink so it isn't a dangling node
        sink_row <- stats::setNames(
          data.frame(nofollow_sink_name, nofollow_sink_name,
                     stringsAsFactors = FALSE),
          c(edge_from_col, edge_to_col)
        )
        # Fill extra columns
        extra_cols <- setdiff(names(current_edge_list), c(edge_from_col, edge_to_col))
        for (col in extra_cols) {
          if (is.logical(current_edge_list[[col]])) {
            sink_row[[col]] <- FALSE
          } else if (is.numeric(current_edge_list[[col]])) {
            sink_row[[col]] <- 1
          } else {
            sink_row[[col]] <- NA
          }
        }
        current_edge_list <- rbind(current_edge_list, sink_row)
        used_nofollow_sink <- TRUE

      }
      # nofollow_action == "keep": do nothing
    }
  }

  # --- 4. Handle Isolates ---
  # After nofollow/indexability steps, the edge list may contain new nodes

  # (e.g., __pr_nofollow_sink__, robots-blocked self-loops). Include them.
  current_edge_nodes <- unique(c(
    as.character(current_edge_list[[edge_from_col]]),
    as.character(current_edge_list[[edge_to_col]])
  ))
  current_edge_nodes <- current_edge_nodes[!is.na(current_edge_nodes)]

  vertices_for_pagerank_df <- NULL

  if (drop_isolates_flag) {
    # Only keep nodes that participate in at least one complete edge.
    if (length(current_edge_nodes) > 0) {
      vertices_for_pagerank_df <- stats::setNames(
        data.frame(sort(current_edge_nodes), stringsAsFactors = FALSE),
        temp_node_col_name
      )
    }
  } else {
    # Keep all known nodes: original vertex universe PLUS any nodes
    # introduced by nofollow/indexability steps (sink node, etc.)
    full_universe <- unique(c(all_vertex_universe, current_edge_nodes))
    if (length(full_universe) > 0) {
      vertices_for_pagerank_df <- stats::setNames(
        data.frame(sort(full_universe), stringsAsFactors = FALSE),
        temp_node_col_name
      )
    }
  }

  # --- 5. Compute PageRank ---
  pagerank_results <- compute_pagerank(
    edge_list_df = current_edge_list,
    vertices_df = vertices_for_pagerank_df,
    from_col = edge_from_col,
    to_col = edge_to_col,
    vertex_col_name = temp_node_col_name,
    weight_col = weight_col,
    ...
  )

  # --- 6. Post-processing: remove internal nodes from results ---
  if (nrow(pagerank_results) > 0) {
    pr_node_col <- names(pagerank_results)[1]

    # Remove nofollow sink node
    if (used_nofollow_sink) {
      pagerank_results <- pagerank_results[
        pagerank_results[[pr_node_col]] != nofollow_sink_name, , drop = FALSE
      ]
    }

    # Remove robots-blocked nodes if vanish action
    if (robots_blocked_action == "vanish" && length(robots_blocked_urls) > 0) {
      pagerank_results <- pagerank_results[
        !(pagerank_results[[pr_node_col]] %in% robots_blocked_urls), , drop = FALSE
      ]
    }

    row.names(pagerank_results) <- NULL
  }

  return(pagerank_results)
} 