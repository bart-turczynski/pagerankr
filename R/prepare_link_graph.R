# Shared link-graph preparation spine for the undirected hub/authority scorers
# (hits(), salsa()). It sequences the genuinely-identical steps both scorers
# ran in parallel private orchestrators: clean the URL columns -> compose and
# apply the redirect/canonical fold map -> domain/host filter -> snapshot the
# vertex universe -> deduplicate -> assemble the vertex set.
#
# pagerank() deliberately does NOT route through here: its forward-flow devices
# (out-of-scope-fold policy, leak sink, fold-collision detection, indexability,
# the TIPR prior, reverse) enrich the same spine with scorer-specific steps, so
# it keeps its own richer orchestrators and only shares the atomic cleaning
# helper (`.clean_pipeline_urls`, with the uncleaned-edge advisory suppressed
# here). See the "Relationship to the PageRank pipeline" sections in ?hits and
# ?salsa.

#' Prepare the shared hub/authority link graph (hits() / salsa()).
#'
#' Runs the identity-forming pipeline shared by [hits()] and [salsa()]: URL
#' cleaning (one resolved `rurl` profile), composed redirect + canonical
#' folding, domain/host filtering, vertex-universe capture, deduplication per
#' `duplicate_edge_policy`, and isolate-aware vertex-set assembly. Returns the
#' prepared edge list, the vertex data frame (or `NULL`), the effective weight
#' column, and the node-column name the caller passes to its `compute_*` core.
#'
#' The advisory that `pagerank()` emits when edge cleaning is disabled but query
#' parameters remain is intentionally suppressed here, matching the historical
#' behavior of both wrappers. `weight_col` is honored by the deduplication (via
#' [aggregate_edges()]); `salsa()` passes `NULL` and ignores the returned
#' `weight_col` because SALSA v1 is unweighted.
#'
#' @return A list with `edge_list` (prepared edges), `vertices_df` (a
#'   single-column vertex frame or `NULL`), `weight_col` (the effective weight
#'   column, possibly the synthetic instance-count column), and `node_col` (the
#'   vertex-column name).
#' @keywords internal
#' @noRd
.prepare_link_graph <- function(edge_list_df,
                                redirects_df,
                                canonicals_df,
                                rurl_params,
                                clean_edge_urls,
                                clean_redirect_urls,
                                clean_canonical_urls,
                                edge_from_col,
                                edge_to_col,
                                redirect_from_col,
                                redirect_to_col,
                                canonical_from_col,
                                canonical_to_col,
                                duplicate_from_policy,
                                loop_handling,
                                canonical_duplicate_from_policy,
                                canonical_loop_handling,
                                canonical_conflict_policy,
                                keep_domains,
                                exclude_domains,
                                keep_hosts,
                                exclude_hosts,
                                duplicate_edge_policy,
                                self_loops,
                                drop_isolates_flag,
                                weight_col = NULL) {
  node_col <- "node_name"

  # --- 1. URL cleaning (shared resolved rurl profile) ---
  effective_rurl_params <- .resolve_rurl_params(rurl_params)
  cleaned <- .clean_pipeline_urls(
    edge_list_df = edge_list_df,
    redirects_df = redirects_df,
    canonicals_df = canonicals_df,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    redirect_from_col = redirect_from_col,
    redirect_to_col = redirect_to_col,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    clean_edge_urls = clean_edge_urls,
    clean_redirect_urls = clean_redirect_urls,
    clean_canonical_urls = clean_canonical_urls,
    effective_rurl_params = effective_rurl_params,
    warn_uncleaned_edges = FALSE
  )

  # --- 2. Redirect + canonical resolution (one composed fold map) ---
  current_edge_list <- .compose_and_apply_fold_map(
    edge_list_df = cleaned$edge_list_df,
    redirects_df = cleaned$redirects_df,
    canonicals_df = cleaned$canonicals_df,
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
    canonical_conflict_policy = canonical_conflict_policy
  )

  # --- 2.7. Domain / host filtering ---
  current_edge_list <- .filter_graph_domains(
    edge_list_df = current_edge_list,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    keep_domains = keep_domains,
    exclude_domains = exclude_domains,
    keep_hosts = keep_hosts,
    exclude_hosts = exclude_hosts,
    effective_rurl_params = effective_rurl_params
  )

  # --- 2.5. Full vertex universe (before NA rows are stripped by dedup) ---
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
  vertices_df <- .assemble_vertices_df(
    edge_list_df = current_edge_list,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    drop_isolates_flag = drop_isolates_flag,
    all_vertex_universe = all_vertex_universe,
    node_col = node_col
  )

  list(
    edge_list = current_edge_list,
    vertices_df = vertices_df,
    weight_col = effective_weight_col,
    node_col = node_col
  )
}

#' Compose the redirect + canonical fold map and apply it to the edge endpoints.
#'
#' Shared by [hits()] and [salsa()] (via [.prepare_link_graph()]). No-op when
#' neither redirects nor canonicals are supplied, or when the composed map is
#' empty. This is the plain fold applier; [pagerank()] uses its own richer
#' [.resolve_fold_and_apply()] that additionally classifies out-of-scope folds,
#' routes leak sources, and detects fold-target collisions.
#' @return The (possibly relabeled) edge list.
#' @keywords internal
#' @noRd
.compose_and_apply_fold_map <- function(edge_list_df,
                                        redirects_df,
                                        canonicals_df,
                                        edge_from_col,
                                        edge_to_col,
                                        redirect_from_col,
                                        redirect_to_col,
                                        canonical_from_col,
                                        canonical_to_col,
                                        duplicate_from_policy,
                                        loop_handling,
                                        canonical_duplicate_from_policy,
                                        canonical_loop_handling,
                                        canonical_conflict_policy) {
  has_redirects <- .df_has_rows(redirects_df)
  has_canonicals <- .df_has_rows(canonicals_df)

  if (!has_redirects && !has_canonicals) {
    return(edge_list_df)
  }

  fold <- .compose_fold_map(
    redirects_df = if (has_redirects) redirects_df else NULL,
    canonicals_df = if (has_canonicals) canonicals_df else NULL,
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

  if (length(fold_map) == 0) {
    return(edge_list_df)
  }

  for (col_name in c(edge_from_col, edge_to_col)) {
    if (col_name %in% names(edge_list_df)) {
      edge_list_df[[col_name]] <- .apply_fold_map(
        edge_list_df[[col_name]], fold_map
      )
    }
  }
  edge_list_df
}

#' Apply optional domain / host filtering to the shared edge list.
#'
#' No-op unless at least one keep/exclude domain or host value is supplied.
#' Shared by [hits()] and [salsa()]; [pagerank()] uses its own
#' [.apply_domain_host_filter()], which additionally warns when an out-of-scope
#' fold rewrote a crawled filter value away.
#' @return The (possibly filtered) edge list.
#' @keywords internal
#' @noRd
.filter_graph_domains <- function(edge_list_df,
                                  edge_from_col,
                                  edge_to_col,
                                  keep_domains,
                                  exclude_domains,
                                  keep_hosts,
                                  exclude_hosts,
                                  effective_rurl_params) {
  if (is.null(keep_domains) && is.null(exclude_domains) &&
        is.null(keep_hosts) && is.null(exclude_hosts)) {
    return(edge_list_df)
  }
  filter_links_by_domain(
    edge_list_df = edge_list_df,
    from_col = edge_from_col,
    to_col = edge_to_col,
    keep_domains = keep_domains,
    ignore_domains = exclude_domains,
    keep_hosts = keep_hosts,
    ignore_hosts = exclude_hosts,
    rurl_params = effective_rurl_params
  )
}

#' Assemble the vertex data frame, honoring the isolate-handling flag.
#'
#' With `drop_isolates_flag = TRUE` only nodes on a surviving (deduplicated)
#' edge are kept; otherwise the full pre-dedup vertex universe is retained so
#' partial-row / isolate nodes appear in the result. Returns `NULL` when the
#' resulting node set is empty. Shared by [hits()] and [salsa()].
#' @return A single-column, sorted vertex frame named `node_col`, or `NULL`.
#' @keywords internal
#' @noRd
.assemble_vertices_df <- function(edge_list_df,
                                  edge_from_col,
                                  edge_to_col,
                                  drop_isolates_flag,
                                  all_vertex_universe,
                                  node_col) {
  current_edge_nodes <- unique(c(
    as.character(edge_list_df[[edge_from_col]]),
    as.character(edge_list_df[[edge_to_col]])
  ))
  current_edge_nodes <- current_edge_nodes[!is.na(current_edge_nodes)]

  if (drop_isolates_flag) {
    node_set <- current_edge_nodes
  } else {
    node_set <- unique(c(all_vertex_universe, current_edge_nodes))
  }

  if (length(node_set) == 0) {
    return(NULL)
  }
  stats::setNames(data.frame(sort(node_set)), node_col)
}
