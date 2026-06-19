#' @title Compute HITS hub and authority scores using igraph
#' @description Builds a directed graph from a processed edge list and computes
#'   Kleinberg's HITS hub and authority scores using `igraph::hits_scores()`
#'   (the non-deprecated successor of `igraph::hub_score()` /
#'   `igraph::authority_score()`). This is the low-level computational core; the
#'   high-level [hits()] wrapper runs the URL-cleaning, redirect/canonical
#'   folding, domain filtering, deduplication, and isolate handling identity
#'   pipeline first.
#'
#' @param edge_list_df A data frame representing the processed edge list, with
#'   source/target columns (see `from_col`, `to_col`). NAs in those columns are
#'   omitted before graph construction.
#' @param vertices_df An optional single-column data frame of node names
#'   defining the vertex set (e.g. to retain isolates). If `NULL` (default), the
#'   vertices are inferred from `edge_list_df`. The column name is given by
#'   `vertex_col_name`.
#' @param from_col,to_col Names of the source/target columns in `edge_list_df`.
#'   Defaults `"from"` / `"to"`.
#' @param vertex_col_name Name of the node column in `vertices_df`. Default
#'   `"node_name"`.
#' @param weight_col Optional name of a numeric edge-weight column. Higher
#'   weights give an edge more influence in the hub/authority mutual
#'   reinforcement. If `NULL` (default), the graph is unweighted.
#' @param weight_validation How invalid edge weights are handled when
#'   `weight_col` is supplied: `"error"` (default), `"warning"`, or `"none"`.
#'   See [validate_edge_weights()].
#' @param scale Logical, passed to `igraph::hits_scores()`. When `TRUE`
#'   (default) each score vector is scaled so its maximum entry is `1`, the
#'   conventional HITS reporting convention. When `FALSE` the raw principal
#'   eigenvectors (unit Euclidean norm) are returned.
#' @param pr_node_col Name for the node column in the output. Default
#'   `"node_name"` (kept consistent with [compute_pagerank()]).
#' @param hub_col,authority_col Names for the hub and authority score columns in
#'   the output. Defaults `"hub"` / `"authority"`.
#' @param ... Additional arguments passed to `igraph::hits_scores()` (e.g.
#'   `options`).
#'
#' @details
#' ## Matrix formulation
#'
#' Let \eqn{A} be the adjacency matrix of the directed graph (\eqn{A_{ij} = 1}
#' when page \eqn{i} links to page \eqn{j}, or the edge weight when weighted).
#' HITS computes two mutually reinforcing scores as the dominant eigenvectors:
#'
#' \itemize{
#'   \item **authority** is the dominant eigenvector of \eqn{A^\top A}: a page
#'     is a good authority when it is pointed to by good hubs.
#'   \item **hub** is the dominant eigenvector of \eqn{A A^\top}: a page is a
#'     good hub when it points to good authorities.
#' }
#'
#' `igraph::hits_scores()` solves these eigenproblems directly, so no separate
#' direction flip is needed: authority is the inflow-oriented score and hub is
#' the outflow-oriented score, both returned from a single call.
#'
#' @return A data frame with three columns: the node name (named by
#'   `pr_node_col`) and the hub and authority scores (named by `hub_col` /
#'   `authority_col`). Returns an empty (zero-row) data frame with those columns
#'   when the graph has no vertices.
#' @export
#' @import igraph
#' @seealso [hits()] for the full identity pipeline; [compute_pagerank()] for
#'   the PageRank analogue.
#' @examples
#' edges <- data.frame(
#'   from = c("A", "A", "B"), to = c("B", "C", "C"),
#'   stringsAsFactors = FALSE
#' )
#' compute_hits(edges)
#'
#' # Retain an isolate via vertices_df (scores 0 for both hub and authority)
#' verts <- data.frame(node_name = c("A", "B", "C", "D"))
#' compute_hits(edges, vertices_df = verts)
compute_hits <- function(edge_list_df,
                         vertices_df = NULL,
                         from_col = "from",
                         to_col = "to",
                         vertex_col_name = "node_name",
                         weight_col = NULL,
                         weight_validation = c("error", "warning", "none"),
                         scale = TRUE,
                         pr_node_col = "node_name",
                         hub_col = "hub",
                         authority_col = "authority",
                         ...) {
  weight_validation <- match.arg(weight_validation)

  # --- Input validation (mirrors compute_pagerank) ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0 &&
        !all(c(from_col, to_col) %in% names(edge_list_df))) {
    stop("`edge_list_df` must have '", from_col, "' and '", to_col,
      "' columns if not empty.",
      call. = FALSE
    )
  }
  if (!is.null(vertices_df)) {
    if (!is.data.frame(vertices_df)) {
      stop("`vertices_df` must be a data frame or NULL.", call. = FALSE)
    }
    if (nrow(vertices_df) > 0 && !(vertex_col_name %in% names(vertices_df))) {
      stop("`vertices_df` must have a column named '", vertex_col_name,
        "' if not empty.",
        call. = FALSE
      )
    }
  }
  if (!is.logical(scale) || length(scale) != 1 || is.na(scale)) {
    stop("`scale` must be a single logical value.", call. = FALSE)
  }
  node_cols <- c(pr_node_col, hub_col, authority_col)
  if (!all(vapply(node_cols, function(x) {
    is.character(x) && length(x) == 1 && nchar(x) > 0
  }, logical(1)))) {
    stop(
      "`pr_node_col`, `hub_col`, and `authority_col` must be ",
      "non-empty character strings.",
      call. = FALSE
    )
  }
  if (anyDuplicated(node_cols)) {
    stop(
      "`pr_node_col`, `hub_col`, and `authority_col` must be distinct.",
      call. = FALSE
    )
  }
  if (!is.null(weight_col)) {
    if (!is.character(weight_col) || length(weight_col) != 1) {
      stop(
        "`weight_col` must be a single character string or NULL.",
        call. = FALSE
      )
    }
    if (nrow(edge_list_df) > 0 && !(weight_col %in% names(edge_list_df))) {
      stop("`weight_col` '", weight_col, "' not found in `edge_list_df`.",
        call. = FALSE
      )
    }
    if (nrow(edge_list_df) > 0 && !is.numeric(edge_list_df[[weight_col]])) {
      stop("`weight_col` '", weight_col, "' must be a numeric column.",
        call. = FALSE
      )
    }
  }

  # --- Empty result template ---
  empty_result <- stats::setNames(
    data.frame(
      character(0), numeric(0), numeric(0),
      stringsAsFactors = FALSE
    ),
    node_cols
  )

  defined_nodes <- NULL
  if (!is.null(vertices_df) && nrow(vertices_df) > 0 &&
        vertex_col_name %in% names(vertices_df)) {
    defined_nodes <- unique(
      stats::na.omit(as.character(vertices_df[[vertex_col_name]]))
    )
    if (length(defined_nodes) == 0) defined_nodes <- NULL
  }

  # --- Prepare edges (remove NAs, extract aligned weights) ---
  valid_edges_df <- NULL
  weight_vector <- NULL
  if (nrow(edge_list_df) > 0) {
    cols_to_keep <- c(from_col, to_col)
    if (!is.null(weight_col)) cols_to_keep <- c(cols_to_keep, weight_col)
    edges_for_graph <- edge_list_df[, cols_to_keep, drop = FALSE]
    edges_for_graph[[from_col]] <- as.character(edges_for_graph[[from_col]])
    edges_for_graph[[to_col]] <- as.character(edges_for_graph[[to_col]])

    na_in_edges <- is.na(edges_for_graph[[from_col]]) |
      is.na(edges_for_graph[[to_col]])
    valid_edges_df <- edges_for_graph[!na_in_edges, , drop = FALSE]

    if (!is.null(weight_col) && nrow(valid_edges_df) > 0) {
      validate_edge_weights(
        edge_list_df = valid_edges_df,
        weight_col = weight_col,
        from_col = from_col,
        action = weight_validation
      )
      weight_vector <- valid_edges_df[[weight_col]]
      valid_edges_df <- valid_edges_df[, c(from_col, to_col), drop = FALSE]
    }
  } else {
    valid_edges_df <- data.frame(
      matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c(from_col, to_col)))
    )
    valid_edges_df[[from_col]] <- character(0)
    valid_edges_df[[to_col]] <- character(0)
  }

  # --- Build graph ---
  if (nrow(valid_edges_df) == 0 && is.null(defined_nodes)) {
    return(empty_result)
  }

  current_graph <- NULL
  if (nrow(valid_edges_df) > 0) {
    current_graph <- igraph::graph_from_data_frame(
      d = valid_edges_df,
      directed = TRUE,
      vertices = defined_nodes
    )
    if (!is.null(weight_vector)) {
      igraph::E(current_graph)$weight <- weight_vector
    }
  } else {
    current_graph <- igraph::make_empty_graph(
      n = length(defined_nodes), directed = TRUE
    )
    igraph::V(current_graph)$name <- defined_nodes
  }

  if (igraph::vcount(current_graph) == 0) {
    return(empty_result)
  }

  # --- Compute HITS ---
  # hits_scores() returns hub and authority in one pass; pass weights
  # explicitly (it does not auto-detect the edge weight attribute the way
  # page_rank() does).
  hits_out <- tryCatch(
    igraph::hits_scores(
      graph = current_graph,
      scale = scale,
      weights = weight_vector,
      ...
    ),
    error = function(e) {
      warning("igraph::hits_scores computation failed: ", e$message,
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(hits_out) || is.null(hits_out$hub) ||
        length(hits_out$hub) == 0) {
    return(empty_result)
  }

  result <- data.frame(
    node = names(hits_out$hub),
    hub = unname(hits_out$hub),
    authority = unname(hits_out$authority),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  names(result) <- node_cols
  result
}

#' @title Master HITS hub/authority calculation wrapper
#' @description Computes Kleinberg's HITS hub and authority scores over the same
#'   cleaned, redirect/canonical-folded, domain-filtered, deduplicated link
#'   graph that [pagerank()] builds, so node identities line up across the two
#'   centrality measures. Wraps `igraph::hits_scores()` (the non-deprecated
#'   successor of `igraph::hub_score()` / `igraph::authority_score()`).
#' @name hits
#'
#' @inheritParams pagerank
#' @param scale Logical, passed to `igraph::hits_scores()` via [compute_hits()].
#'   `TRUE` (default) scales each score so its maximum is `1`; `FALSE` returns
#'   the unit-norm eigenvectors. See [compute_hits()].
#' @param ... Additional arguments forwarded to [compute_hits()] and then to
#'   `igraph::hits_scores()`.
#'
#' @details
#' ## Relationship to the PageRank pipeline
#'
#' `hits()` reuses the exact identity-forming steps of [pagerank()] — URL
#' canonicalization (the same resolved `rurl` profile), the same composed
#' redirect + canonical fold map, the same domain/host filtering, the same
#' `duplicate_edge_policy` deduplication, and the same self-loop / isolate
#' handling. The resulting vertex set therefore matches `pagerank()` run with
#' the same arguments, so hub, authority, and PageRank can be joined on
#' `node_name` without re-canonicalizing.
#'
#' The PageRank-specific, *forward-flow* modeling devices have **no HITS
#' analogue and are intentionally not exposed**: nofollow evaporation, the
#' indexability (noindex / robots.txt) transforms, the TIPR teleport prior, and
#' the `reverse` flag. HITS already computes both directions of authority flow
#' (hub is the outflow-oriented score, authority the inflow-oriented one), so a
#' separate reversal is unnecessary.
#'
#' ## Matrix formulation and the whole-graph caveat
#'
#' With adjacency matrix \eqn{A}, **authority** is the dominant eigenvector of
#' \eqn{A^\top A} ("pages pointed to by pages that point to many things") and
#' **hub** is the dominant eigenvector of \eqn{A A^\top} ("pages that point to
#' pages pointed to by many things"). See [compute_hits()].
#'
#' Kleinberg's original HITS (1999) was run on a small, **query-focused base
#' set** of pages, where the hub/authority distinction is sharply
#' interpretable. `hits()` instead runs on the **full (or user-filtered) site
#' graph** that `pagerankr` assembles. The eigenvector computation is identical
#' and correct, but the interpretation shifts: scores describe hub/authority
#' structure across the whole crawled graph rather than relevance to a specific
#' query. Treat them as site-wide structural centralities, not query-relevance
#' scores.
#'
#' @return A data frame with one row per node and columns `node_name`, `hub`,
#'   and `authority` (column names configurable via `...`). Hub and authority
#'   are scaled to a maximum of `1` by default (`scale = TRUE`).
#' @export
#' @seealso [compute_hits()] for the computational core, [pagerank()] for the
#'   PageRank analogue sharing this identity pipeline.
#' @examples
#' edges <- data.frame(
#'   from = c("http://A.com/", "http://A.com/", "B.com"),
#'   to = c("B.com", "C.com", "C.com"),
#'   stringsAsFactors = FALSE
#' )
#' hits(edges)
#'
#' # Hub vs authority: a pure outflow page tops hub, a pure inflow page tops
#' # authority.
#' h <- hits(edges)
#' h[which.max(h$hub), ]
#' h[which.max(h$authority), ]
hits <- function(edge_list_df,
                 redirects_df = NULL,
                 clean_edge_urls = TRUE,
                 clean_redirect_urls = TRUE,
                 rurl_params = list(),
                 self_loops = c("drop", "keep"),
                 drop_isolates_flag = TRUE,
                 weight_col = NULL,
                 duplicate_edge_policy = c(
                   "collapse", "aggregate", "count_instances"
                 ),
                 edge_from_col = "from",
                 edge_to_col = "to",
                 redirect_from_col = "from",
                 redirect_to_col = "to",
                 duplicate_from_policy = c(
                   "strict",
                   "first_wins",
                   "last_wins",
                   "most_frequent",
                   "prune_source",
                   "resolve_if_consistent"
                 ),
                 loop_handling = c(
                   "error",
                   "prune_loop",
                   "break_arrow"
                 ),
                 canonicals_df = NULL,
                 canonical_from_col = "from",
                 canonical_to_col = "to",
                 clean_canonical_urls = TRUE,
                 canonical_duplicate_from_policy = c(
                   "strict",
                   "first_wins",
                   "last_wins",
                   "most_frequent",
                   "prune_source",
                   "resolve_if_consistent"
                 ),
                 canonical_loop_handling = c(
                   "error",
                   "prune_loop",
                   "break_arrow"
                 ),
                 canonical_conflict_policy = c(
                   "redirect_wins",
                   "error",
                   "canonical_wins"
                 ),
                 keep_domains = NULL,
                 exclude_domains = NULL,
                 keep_hosts = NULL,
                 exclude_hosts = NULL,
                 scale = TRUE,
                 ...) {
  # --- Argument matching and validation ---
  self_loops <- match.arg(self_loops)
  duplicate_edge_policy <- match.arg(duplicate_edge_policy)
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)
  canonical_duplicate_from_policy <- match.arg(canonical_duplicate_from_policy)
  canonical_loop_handling <- match.arg(canonical_loop_handling)
  canonical_conflict_policy <- match.arg(canonical_conflict_policy)

  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!is.null(redirects_df) && !is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.null(canonicals_df) && !is.data.frame(canonicals_df)) {
    stop("`canonicals_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.logical(clean_canonical_urls) || length(clean_canonical_urls) != 1) {
    stop(
      "`clean_canonical_urls` must be a single logical value.",
      call. = FALSE
    )
  }
  canonical_cols <- c(canonical_from_col, canonical_to_col)
  if (!is.null(canonicals_df) && nrow(canonicals_df) > 0 &&
        !all(canonical_cols %in% names(canonicals_df))) {
    stop("`canonicals_df` must have '", canonical_from_col, "' and '",
      canonical_to_col, "' columns.",
      call. = FALSE
    )
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
  if (!is.null(weight_col)) {
    if (!is.character(weight_col) || length(weight_col) != 1) {
      stop(
        "`weight_col` must be a single character string or NULL.",
        call. = FALSE
      )
    }
    if (nrow(edge_list_df) > 0 && !(weight_col %in% names(edge_list_df))) {
      stop(
        "`weight_col` '", weight_col, "' not found in `edge_list_df`.",
        call. = FALSE
      )
    }
  }

  # --- Working copies ---
  current_edge_list <- edge_list_df
  current_redirects_list <- redirects_df
  current_canonicals_list <- canonicals_df

  # --- 1. URL cleaning (shared resolved rurl profile) ---
  edge_url_cols <- intersect(
    c(edge_from_col, edge_to_col),
    names(current_edge_list)
  )
  redirect_url_cols <- if (!is.null(current_redirects_list)) {
    intersect(
      c(redirect_from_col, redirect_to_col),
      names(current_redirects_list)
    )
  } else {
    character(0)
  }
  canonical_url_cols <- if (!is.null(current_canonicals_list)) {
    intersect(
      c(canonical_from_col, canonical_to_col),
      names(current_canonicals_list)
    )
  } else {
    character(0)
  }

  effective_rurl_params <- .resolve_rurl_params(rurl_params)

  if (clean_edge_urls && length(edge_url_cols) > 0) {
    current_edge_list <- do.call(
      clean_url_columns,
      c(list(
        data_frame = current_edge_list,
        columns = edge_url_cols
      ), effective_rurl_params)
    )
  }
  if (clean_redirect_urls && !is.null(current_redirects_list) &&
        nrow(current_redirects_list) > 0 && length(redirect_url_cols) > 0) {
    current_redirects_list <- do.call(
      clean_url_columns,
      c(list(
        data_frame = current_redirects_list,
        columns = redirect_url_cols
      ), effective_rurl_params)
    )
  }
  if (clean_canonical_urls && !is.null(current_canonicals_list) &&
        nrow(current_canonicals_list) > 0 && length(canonical_url_cols) > 0) {
    current_canonicals_list <- do.call(
      clean_url_columns,
      c(list(
        data_frame = current_canonicals_list,
        columns = canonical_url_cols
      ), effective_rurl_params)
    )
  }

  # --- 2. Redirect + canonical resolution (one composed fold map) ---
  has_redirects <- !is.null(current_redirects_list) &&
    nrow(current_redirects_list) > 0
  has_canonicals <- !is.null(current_canonicals_list) &&
    nrow(current_canonicals_list) > 0

  fold_map <- character(0)
  if (has_redirects || has_canonicals) {
    fold <- .compose_fold_map(
      redirects_df = if (has_redirects) current_redirects_list else NULL,
      canonicals_df = if (has_canonicals) current_canonicals_list else NULL,
      redirect_from_col = redirect_from_col,
      redirect_to_col = redirect_to_col,
      canonical_from_col = canonical_from_col,
      canonical_to_col = canonical_to_col,
      duplicate_from_policy = duplicate_from_policy,
      loop_handling = loop_handling,
      canonical_duplicate_from_policy = canonical_duplicate_from_policy,
      canonical_loop_handling = canonical_loop_handling,
      canonical_conflict_policy = canonical_conflict_policy
    )
    fold_map <- fold$map

    if (length(fold_map) > 0) {
      for (col_name in c(edge_from_col, edge_to_col)) {
        if (col_name %in% names(current_edge_list)) {
          current_edge_list[[col_name]] <- .apply_fold_map(
            current_edge_list[[col_name]], fold_map
          )
        }
      }
    }
  }

  # --- 2.7. Domain / host filtering ---
  if (!is.null(keep_domains) || !is.null(exclude_domains) ||
        !is.null(keep_hosts) || !is.null(exclude_hosts)) {
    current_edge_list <- filter_links_by_domain(
      edge_list_df = current_edge_list,
      from_col = edge_from_col,
      to_col = edge_to_col,
      keep_domains = keep_domains,
      ignore_domains = exclude_domains,
      keep_hosts = keep_hosts,
      ignore_hosts = exclude_hosts,
      rurl_params = effective_rurl_params
    )
  }

  # --- 2.5. Full vertex universe (before NA rows are stripped) ---
  temp_node_col_name <- "node_name"
  all_vertex_universe <- unique(stats::na.omit(c(
    as.character(current_edge_list[[edge_from_col]]),
    as.character(current_edge_list[[edge_to_col]])
  )))

  # --- 3. Duplicate-edge policy (handles self-loops) ---
  effective_weight_col <- weight_col
  current_edge_list <- .apply_duplicate_edge_policy(
    edge_list_df = current_edge_list,
    policy = duplicate_edge_policy,
    self_loops = self_loops,
    from_col = edge_from_col,
    to_col = edge_to_col
  )
  if (duplicate_edge_policy == "count_instances" && is.null(weight_col)) {
    effective_weight_col <- "__pr_instance_count__"
  }

  # --- 4. Handle isolates / assemble vertex set ---
  current_edge_nodes <- unique(c(
    as.character(current_edge_list[[edge_from_col]]),
    as.character(current_edge_list[[edge_to_col]])
  ))
  current_edge_nodes <- current_edge_nodes[!is.na(current_edge_nodes)]

  vertices_for_hits_df <- NULL
  if (drop_isolates_flag) {
    if (length(current_edge_nodes) > 0) {
      vertices_for_hits_df <- stats::setNames(
        data.frame(sort(current_edge_nodes), stringsAsFactors = FALSE),
        temp_node_col_name
      )
    }
  } else {
    full_universe <- unique(c(all_vertex_universe, current_edge_nodes))
    if (length(full_universe) > 0) {
      vertices_for_hits_df <- stats::setNames(
        data.frame(sort(full_universe), stringsAsFactors = FALSE),
        temp_node_col_name
      )
    }
  }

  # --- 5. Compute HITS ---
  compute_hits(
    edge_list_df = current_edge_list,
    vertices_df = vertices_for_hits_df,
    from_col = edge_from_col,
    to_col = edge_to_col,
    vertex_col_name = temp_node_col_name,
    weight_col = effective_weight_col,
    scale = scale,
    ...
  )
}
