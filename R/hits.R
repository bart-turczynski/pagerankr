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
#' @seealso [hits()] for the full identity pipeline; [compute_pagerank()] for
#'   the PageRank analogue.
#' @examples
#' edges <- data.frame(
#'   from = c("A", "A", "B"), to = c("B", "C", "C")
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
  .validate_compute_hits_frames(
    edge_list_df, from_col, to_col, vertices_df, vertex_col_name
  )
  node_cols <- c(pr_node_col, hub_col, authority_col)
  .validate_compute_hits_scalars(scale, node_cols)
  .validate_weight_col(weight_col, edge_list_df)

  # --- Empty result template ---
  empty_result <- .empty_hits_result(node_cols)

  defined_nodes <- .resolve_defined_nodes(vertices_df, vertex_col_name)

  # --- Prepare edges (remove NAs, extract aligned weights) ---
  prepared <- .prepare_hits_edges(
    edge_list_df, from_col, to_col, weight_col, weight_validation
  )
  valid_edges_df <- prepared$edges
  weight_vector <- prepared$weights

  # --- Build graph ---
  if (nrow(valid_edges_df) == 0 && is.null(defined_nodes)) {
    return(empty_result)
  }

  current_graph <- .build_hits_graph(
    valid_edges_df, defined_nodes, weight_vector
  )

  if (igraph::vcount(current_graph) == 0) {
    return(empty_result)
  }

  # --- Compute HITS ---
  .run_hits_scores(
    current_graph, scale, weight_vector, node_cols, empty_result, ...
  )
}

# --- Internal helpers for compute_hits() -------------------------------------

#' Validate the data-frame inputs to compute_hits()
#' @noRd
.validate_compute_hits_frames <- function(edge_list_df, from_col, to_col,
                                          vertices_df, vertex_col_name) {
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
  invisible(NULL)
}

#' Validate the `scale` flag and the output column names for compute_hits()
#' @noRd
.validate_compute_hits_scalars <- function(scale, node_cols) {
  if (!is.logical(scale) || length(scale) != 1 || is.na(scale)) {
    stop("`scale` must be a single logical value.", call. = FALSE)
  }
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
  invisible(NULL)
}

#' Build the empty (zero-row) HITS result template
#' @noRd
.empty_hits_result <- function(node_cols) {
  stats::setNames(
    data.frame(
      character(0), numeric(0), numeric(0)
    ),
    node_cols
  )
}

#' Resolve the user-supplied vertex set (or NULL) for compute_hits()
#' @noRd
.resolve_defined_nodes <- function(vertices_df, vertex_col_name) {
  defined_nodes <- NULL
  if (!is.null(vertices_df) && nrow(vertices_df) > 0 &&
        vertex_col_name %in% names(vertices_df)) {
    defined_nodes <- unique(
      stats::na.omit(as.character(vertices_df[[vertex_col_name]]))
    )
    if (length(defined_nodes) == 0) defined_nodes <- NULL
  }
  defined_nodes
}

#' Prepare the edge data frame (drop NA endpoints, extract aligned weights)
#' @noRd
.prepare_hits_edges <- function(edge_list_df, from_col, to_col,
                                weight_col, weight_validation) {
  if (nrow(edge_list_df) == 0) {
    valid_edges_df <- data.frame(
      matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c(from_col, to_col)))
    )
    valid_edges_df[[from_col]] <- character(0)
    valid_edges_df[[to_col]] <- character(0)
    return(list(edges = valid_edges_df, weights = NULL))
  }

  cols_to_keep <- c(from_col, to_col)
  if (!is.null(weight_col)) cols_to_keep <- c(cols_to_keep, weight_col)
  edges_for_graph <- edge_list_df[, cols_to_keep, drop = FALSE]
  edges_for_graph[[from_col]] <- as.character(edges_for_graph[[from_col]])
  edges_for_graph[[to_col]] <- as.character(edges_for_graph[[to_col]])

  na_in_edges <- is.na(edges_for_graph[[from_col]]) |
    is.na(edges_for_graph[[to_col]])
  valid_edges_df <- edges_for_graph[!na_in_edges, , drop = FALSE]

  weight_vector <- NULL
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

  list(edges = valid_edges_df, weights = weight_vector)
}

#' Build the directed igraph graph from prepared edges / vertices
#' @noRd
.build_hits_graph <- function(valid_edges_df, defined_nodes, weight_vector) {
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
  current_graph
}

#' Run igraph::hits_scores() and assemble the output data frame
#' @noRd
.run_hits_scores <- function(current_graph, scale, weight_vector,
                             node_cols, empty_result, ...) {
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
    row.names = NULL
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
#'   to = c("B.com", "C.com", "C.com")
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

  .validate_hits_inputs(
    edge_list_df, redirects_df, canonicals_df,
    clean_canonical_urls, canonical_from_col, canonical_to_col,
    clean_edge_urls, clean_redirect_urls, rurl_params,
    drop_isolates_flag, weight_col
  )

  # --- 1-4. Shared link-graph prep: clean, fold, filter, dedup, vertices ---
  # See .prepare_link_graph(); salsa() drives the same spine.
  prep <- .prepare_link_graph(
    edge_list_df = edge_list_df,
    redirects_df = redirects_df,
    canonicals_df = canonicals_df,
    rurl_params = rurl_params,
    clean_edge_urls = clean_edge_urls,
    clean_redirect_urls = clean_redirect_urls,
    clean_canonical_urls = clean_canonical_urls,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    redirect_from_col = redirect_from_col,
    redirect_to_col = redirect_to_col,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    duplicate_from_policy = duplicate_from_policy,
    loop_handling = loop_handling,
    canonical_duplicate_from_policy = canonical_duplicate_from_policy,
    canonical_loop_handling = canonical_loop_handling,
    canonical_conflict_policy = canonical_conflict_policy,
    keep_domains = keep_domains,
    exclude_domains = exclude_domains,
    keep_hosts = keep_hosts,
    exclude_hosts = exclude_hosts,
    duplicate_edge_policy = duplicate_edge_policy,
    self_loops = self_loops,
    drop_isolates_flag = drop_isolates_flag,
    weight_col = weight_col
  )

  # --- 5. Compute HITS ---
  compute_hits(
    edge_list_df = prep$edge_list, vertices_df = prep$vertices_df,
    from_col = edge_from_col, to_col = edge_to_col,
    vertex_col_name = prep$node_col, weight_col = prep$weight_col,
    scale = scale, ...
  )
}

#' Run all top-level hits() input validation
#' @noRd
.validate_hits_inputs <- function(edge_list_df, redirects_df, canonicals_df,
                                  clean_canonical_urls, canonical_from_col,
                                  canonical_to_col, clean_edge_urls,
                                  clean_redirect_urls, rurl_params,
                                  drop_isolates_flag, weight_col) {
  .validate_hits_frames(edge_list_df, redirects_df, canonicals_df)
  .validate_hits_canonicals(
    canonicals_df, clean_canonical_urls, canonical_from_col, canonical_to_col
  )
  .validate_cleaning_flags(
    clean_edge_urls, clean_redirect_urls, rurl_params, drop_isolates_flag
  )
  .validate_hits_weight(weight_col, edge_list_df)
  invisible(NULL)
}

# --- Internal helpers for hits() ---------------------------------------------

#' Validate the top-level data-frame inputs to hits()
#' @noRd
.validate_hits_frames <- function(edge_list_df, redirects_df, canonicals_df) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!is.null(redirects_df) && !is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.null(canonicals_df) && !is.data.frame(canonicals_df)) {
    stop("`canonicals_df` must be a data frame or NULL.", call. = FALSE)
  }
  invisible(NULL)
}

#' Validate the canonical-cleaning flag and canonical column presence for hits()
#' @noRd
.validate_hits_canonicals <- function(canonicals_df, clean_canonical_urls,
                                      canonical_from_col, canonical_to_col) {
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
  invisible(NULL)
}

#' Validate the optional `weight_col` for hits()
#' @noRd
.validate_hits_weight <- function(weight_col, edge_list_df) {
  if (is.null(weight_col)) {
    return(invisible(NULL))
  }
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
  invisible(NULL)
}

