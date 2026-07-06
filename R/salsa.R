#' @title Compute SALSA hub and authority scores
#' @description Computes the Stochastic Approach for Link-Structure Analysis
#'   (SALSA; Lempel & Moran 2001) hub and authority scores from a processed edge
#'   list. SALSA combines HITS-style mutual reinforcement with PageRank-style
#'   stochastic random walks on the bipartite hub/authority graph. This is the
#'   low-level computational core; the high-level [salsa()] wrapper runs the
#'   URL-cleaning, redirect/canonical folding, domain filtering, deduplication,
#'   and isolate-handling identity pipeline first.
#'
#' @inheritParams compute_hits
#'
#' @details
#' ## The two SALSA Markov chains
#'
#' SALSA builds an undirected bipartite graph \eqn{\hat{G}}: each crawl-graph
#' edge \eqn{u \rightarrow v} contributes a hub-node \eqn{u_h} and an
#' authority-node \eqn{v_a} joined by an edge. Two coupled random walks run on
#' it. The **authority** chain alternates authority \eqn{\rightarrow} hub
#' \eqn{\rightarrow} authority (one step = two traversals); the **hub** chain
#' alternates the other way. Unlike HITS — whose scores are the dominant
#' eigenvectors of \eqn{A^\top A} and \eqn{A A^\top} — each SALSA chain is
#' *stochastic*, so its stationary distribution is the score vector.
#'
#' ## Closed form (no iteration)
#'
#' Lempel & Moran (2001, Proposition 6) show the stationary distributions have a
#' degree-based closed form, so **no eigenvector iteration is needed**. On a
#' single connected component the authority score of a node is
#' \eqn{d_{in}(i) / W} and the hub score is \eqn{d_{out}(i) / W}, where \eqn{W}
#' is the edge count. When the support graph splits into several weakly
#' connected components, each component's scores are renormalized within the
#' component and then reweighted by the component's share of the relevant side
#' (Proposition 6):
#'
#' \deqn{\tilde{\pi}_j = \frac{|A_{c(j)}|}{|A|} \times
#'   \frac{d_{in}(j)}{W_{c(j)}}}
#'
#' for authorities (and symmetrically for hubs with \eqn{d_{out}} and
#' \eqn{|H_c|}), where \eqn{A} is the set of all authorities (in-degree
#' \eqn{> 0}), \eqn{A_{c(j)}} the authorities in \eqn{j}'s component, and
#' \eqn{W_{c(j)}} the edges in that component. **This component reweighting is
#' required for correctness:** without it, cross-component score comparisons are
#' invalid — a common failure mode on site crawls with orphan page clusters.
#' Each side's scores sum to `1`.
#'
#' ## Coverage and one-sided vertices
#'
#' The hub side contains only nodes with out-degree \eqn{> 0}; the authority
#' side only nodes with in-degree \eqn{> 0}. A node's `hub` is `NA` when its
#' out-degree is `0`, and its `authority` is `NA` when its in-degree is `0`
#' (a pure sink has `NA` hub; a pure source has `NA` authority; an isolate has
#' both `NA`). SALSA coverage therefore differs from PageRank coverage on the
#' same graph — this is expected, not a bug.
#'
#' ## Weighting
#'
#' v1 is **unweighted**: the closed form assumes uniform edge weights, so scores
#' are driven by in-/out-degree on the deduplicated simple graph. A weighted
#' extension is deferred.
#'
#' @return A data frame with three columns: the node name (named by
#'   `pr_node_col`) and the hub and authority scores (named by `hub_col` /
#'   `authority_col`). Hub and authority each sum to `1` over their non-`NA`
#'   entries. Returns an empty (zero-row) data frame with those columns when the
#'   graph has no vertices.
#' @references Lempel, R. & Moran, S. (2001). SALSA: The Stochastic Approach for
#'   Link-Structure Analysis. *ACM Transactions on Information Systems*,
#'   19(2), 131-160.
#' @export
#' @seealso [salsa()] for the full identity pipeline; [compute_hits()] for the
#'   HITS analogue; [compute_pagerank()] for the PageRank analogue.
#' @examples
#' edges <- data.frame(
#'   from = c("A", "A", "B"), to = c("B", "C", "C")
#' )
#' compute_salsa(edges)
#'
#' # Retain an isolate via vertices_df (NA hub and NA authority)
#' verts <- data.frame(node_name = c("A", "B", "C", "D"))
#' compute_salsa(edges, vertices_df = verts)
compute_salsa <- function(edge_list_df,
                          vertices_df = NULL,
                          from_col = "from",
                          to_col = "to",
                          vertex_col_name = "node_name",
                          pr_node_col = "node_name",
                          hub_col = "hub",
                          authority_col = "authority") {
  node_cols <- c(pr_node_col, hub_col, authority_col)
  .compute_salsa_validate_inputs(
    edge_list_df, vertices_df, from_col, to_col, vertex_col_name, node_cols
  )

  # --- Empty result template ---
  empty_result <- stats::setNames(
    data.frame(
      character(0), numeric(0), numeric(0)
    ),
    node_cols
  )

  defined_nodes <- .compute_salsa_defined_nodes(vertices_df, vertex_col_name)
  valid_edges_df <- .compute_salsa_prepare_edges(edge_list_df, from_col, to_col)

  # --- Build graph ---
  if (nrow(valid_edges_df) == 0 && is.null(defined_nodes)) {
    return(empty_result)
  }

  current_graph <- .compute_salsa_build_graph(valid_edges_df, defined_nodes)

  if (igraph::vcount(current_graph) == 0) {
    return(empty_result)
  }

  .compute_salsa_closed_form(current_graph, node_cols)
}

#' Validate compute_salsa() inputs.
#'
#' Mirrors compute_hits validation. Preserves error-message text and the
#' short-circuit order exactly.
#' @keywords internal
#' @noRd
.compute_salsa_validate_inputs <- function(edge_list_df,
                                           vertices_df,
                                           from_col,
                                           to_col,
                                           vertex_col_name,
                                           node_cols) {
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
  if (!all(vapply(node_cols, .compute_salsa_is_valid_col, logical(1)))) {
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

#' Is `x` a single non-empty character string?
#' @keywords internal
#' @noRd
.compute_salsa_is_valid_col <- function(x) {
  is.character(x) && length(x) == 1 && nchar(x) > 0
}

#' Resolve the defined-node universe from a vertices data frame.
#'
#' Returns `NULL` when no usable vertices are supplied.
#' @keywords internal
#' @noRd
.compute_salsa_defined_nodes <- function(vertices_df, vertex_col_name) {
  if (is.null(vertices_df) || nrow(vertices_df) == 0 ||
        !(vertex_col_name %in% names(vertices_df))) {
    return(NULL)
  }
  defined_nodes <- unique(
    stats::na.omit(as.character(vertices_df[[vertex_col_name]]))
  )
  if (length(defined_nodes) == 0) {
    return(NULL)
  }
  defined_nodes
}

#' Prepare the edge frame for graph construction (drop NA endpoints).
#' @keywords internal
#' @noRd
.compute_salsa_prepare_edges <- function(edge_list_df, from_col, to_col) {
  if (nrow(edge_list_df) == 0) {
    valid_edges_df <- data.frame(
      matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c(from_col, to_col)))
    )
    valid_edges_df[[from_col]] <- character(0)
    valid_edges_df[[to_col]] <- character(0)
    return(valid_edges_df)
  }
  edges_for_graph <- edge_list_df[, c(from_col, to_col), drop = FALSE]
  edges_for_graph[[from_col]] <- as.character(edges_for_graph[[from_col]])
  edges_for_graph[[to_col]] <- as.character(edges_for_graph[[to_col]])
  na_in_edges <- is.na(edges_for_graph[[from_col]]) |
    is.na(edges_for_graph[[to_col]])
  edges_for_graph[!na_in_edges, , drop = FALSE]
}

#' Build the directed igraph object from prepared edges / defined nodes.
#' @keywords internal
#' @noRd
.compute_salsa_build_graph <- function(valid_edges_df, defined_nodes) {
  if (nrow(valid_edges_df) > 0) {
    return(igraph::graph_from_data_frame(
      d = valid_edges_df,
      directed = TRUE,
      vertices = defined_nodes
    ))
  }
  current_graph <- igraph::make_empty_graph(
    n = length(defined_nodes), directed = TRUE
  )
  igraph::V(current_graph)$name <- defined_nodes
  current_graph
}

#' SALSA closed form (Lempel & Moran 2001, Proposition 6).
#'
#' Computes component-reweighted hub/authority scores from graph degrees.
#' @keywords internal
#' @noRd
.compute_salsa_closed_form <- function(current_graph, node_cols) {
  node_names <- igraph::V(current_graph)$name
  d_in <- igraph::degree(current_graph, mode = "in")
  d_out <- igraph::degree(current_graph, mode = "out")
  # Weak components capture the bipartite hub/authority chain irreducible
  # classes: hubs and authorities of the same weakly connected component share
  # one component in the SALSA bipartite graph.
  membership <- igraph::components(current_graph, mode = "weak")$membership

  is_auth <- d_in > 0
  is_hub <- d_out > 0
  n_auth <- sum(is_auth)
  n_hub <- sum(is_hub)

  # Edges per component: sum of in-degree (== sum of out-degree) within a
  # component equals that component's edge count W_c.
  edges_per_comp <- tapply(d_in, membership, sum)
  auth_per_comp <- tapply(is_auth, membership, sum)
  hub_per_comp <- tapply(is_hub, membership, sum)
  comp_key <- as.character(membership)

  hub <- rep(NA_real_, length(node_names))
  authority <- rep(NA_real_, length(node_names))

  if (n_auth > 0) {
    wc_a <- as.numeric(edges_per_comp[comp_key[is_auth]])
    comp_share_a <- as.numeric(auth_per_comp[comp_key[is_auth]]) / n_auth
    authority[is_auth] <- comp_share_a * d_in[is_auth] / wc_a
  }
  if (n_hub > 0) {
    wc_h <- as.numeric(edges_per_comp[comp_key[is_hub]])
    comp_share_h <- as.numeric(hub_per_comp[comp_key[is_hub]]) / n_hub
    hub[is_hub] <- comp_share_h * d_out[is_hub] / wc_h
  }

  result <- data.frame(
    node = node_names,
    hub = hub,
    authority = authority,
    row.names = NULL
  )
  names(result) <- node_cols
  result
}

#' @title Master SALSA hub/authority calculation wrapper
#' @description Computes Lempel & Moran's SALSA hub and authority scores over
#'   the same cleaned, redirect/canonical-folded, domain-filtered, deduplicated
#'   link graph that [pagerank()] builds, so node identities line up across the
#'   centrality measures. SALSA is a stochastic variant of HITS: it runs
#'   HITS-style mutual reinforcement as PageRank-style random walks on the
#'   bipartite hub/authority graph, yielding stationary-distribution scores
#'   instead of dominant eigenvectors.
#' @name salsa
#'
#' @inheritParams pagerank
#' @param ... Additional arguments forwarded to [compute_salsa()].
#'
#' @details
#' ## Relationship to the PageRank pipeline
#'
#' `salsa()` reuses the exact identity-forming steps of [pagerank()] — URL
#' canonicalization (the same resolved `rurl` profile), the same composed
#' redirect + canonical fold map, the same domain/host filtering, the same
#' `duplicate_edge_policy` deduplication, and the same self-loop / isolate
#' handling. The resulting vertex set therefore matches `pagerank()` run with
#' the same arguments, so hub, authority, and PageRank can be joined on
#' `node_name` without re-canonicalizing.
#'
#' The PageRank-specific, *forward-flow* modeling devices have **no SALSA
#' analogue and are intentionally not exposed**: nofollow evaporation, the
#' indexability (noindex / robots.txt) transforms, the TIPR teleport prior, and
#' the `reverse` flag. SALSA already computes both directions of authority flow
#' (hub is the outflow-oriented score, authority the inflow-oriented one).
#'
#' ## SALSA versus HITS, and the pagerankr adaptation
#'
#' Where [hits()] takes the dominant eigenvectors of \eqn{A^\top A} and
#' \eqn{A A^\top}, SALSA replaces the mutual-reinforcement iteration with two
#' *stochastic* Markov chains on the bipartite hub/authority graph; their
#' stationary distributions are the scores, which on a connected graph reduce to
#' a simple in-/out-degree closed form (see [compute_salsa()]). Because the
#' chains are stochastic, SALSA is far less prone than HITS to the
#' "tightly-knit community" effect, where a dense cluster of mutually linking
#' pages dominates the top scores.
#'
#' Lempel & Moran's original SALSA ran on a query-focused base set. `salsa()`
#' instead runs on the **full (or user-filtered) site graph** that `pagerankr`
#' assembles — a documented site-graph adaptation of the focused-subgraph
#' algorithm. Treat the scores as site-wide structural centralities, not
#' query-relevance scores. Coverage differs from PageRank: a node's `hub` is
#' `NA` when it has no outlinks and its `authority` is `NA` when it has no
#' inlinks (see [compute_salsa()]).
#'
#' @return A data frame with one row per node and columns `node_name`, `hub`,
#'   and `authority` (column names configurable via `...`). Hub and authority
#'   each sum to `1` over their non-`NA` entries.
#' @references Lempel, R. & Moran, S. (2001). SALSA: The Stochastic Approach for
#'   Link-Structure Analysis. *ACM Transactions on Information Systems*,
#'   19(2), 131-160.
#' @export
#' @seealso [compute_salsa()] for the computational core, [hits()] for the HITS
#'   analogue, and [pagerank()] for the PageRank analogue sharing this identity
#'   pipeline.
#' @examples
#' edges <- data.frame(
#'   from = c("http://A.com/", "http://A.com/", "B.com"),
#'   to = c("B.com", "C.com", "C.com")
#' )
#' salsa(edges)
#'
#' # Hub vs authority: a pure outflow page tops hub, a pure inflow page tops
#' # authority.
#' s <- salsa(edges)
#' s[which.max(s$hub), ]
#' s[which.max(s$authority), ]
salsa <- function(edge_list_df,
                  redirects_df = NULL,
                  clean_edge_urls = TRUE,
                  clean_redirect_urls = TRUE,
                  rurl_params = list(),
                  self_loops = c("drop", "keep"),
                  drop_isolates_flag = TRUE,
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
                  ...) {
  # --- Argument matching and validation ---
  self_loops <- match.arg(self_loops)
  duplicate_edge_policy <- match.arg(duplicate_edge_policy)
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)
  canonical_duplicate_from_policy <- match.arg(canonical_duplicate_from_policy)
  canonical_loop_handling <- match.arg(canonical_loop_handling)
  canonical_conflict_policy <- match.arg(canonical_conflict_policy)

  .salsa_validate_frames(
    edge_list_df, redirects_df, canonicals_df, clean_canonical_urls,
    canonical_from_col, canonical_to_col
  )
  .salsa_validate_flags(
    clean_edge_urls, clean_redirect_urls, rurl_params, drop_isolates_flag
  )

  # --- Working copies ---
  current_edge_list <- edge_list_df
  current_redirects_list <- redirects_df
  current_canonicals_list <- canonicals_df

  # --- 1. URL cleaning (shared resolved rurl profile) ---
  effective_rurl_params <- .resolve_rurl_params(rurl_params)

  current_edge_list <- .salsa_clean_url_df(
    current_edge_list, c(edge_from_col, edge_to_col),
    clean_edge_urls, effective_rurl_params, require_rows = FALSE
  )
  current_redirects_list <- .salsa_clean_url_df(
    current_redirects_list, c(redirect_from_col, redirect_to_col),
    clean_redirect_urls, effective_rurl_params, require_rows = TRUE
  )
  current_canonicals_list <- .salsa_clean_url_df(
    current_canonicals_list, c(canonical_from_col, canonical_to_col),
    clean_canonical_urls, effective_rurl_params, require_rows = TRUE
  )

  # --- 2. Redirect + canonical resolution (one composed fold map) ---
  current_edge_list <- .salsa_apply_folds(
    current_edge_list, current_redirects_list, current_canonicals_list,
    edge_from_col, edge_to_col,
    redirect_from_col, redirect_to_col,
    canonical_from_col, canonical_to_col,
    duplicate_from_policy, loop_handling,
    canonical_duplicate_from_policy, canonical_loop_handling,
    canonical_conflict_policy
  )

  # --- 2.5 / 2.7 / 3 / 4. Domain filter, vertex universe, dedup, isolates ---
  assembled <- .salsa_assemble_vertices(
    current_edge_list, edge_from_col, edge_to_col,
    keep_domains, exclude_domains, keep_hosts, exclude_hosts,
    effective_rurl_params, duplicate_edge_policy, self_loops,
    drop_isolates_flag
  )

  # --- 5. Compute SALSA ---
  compute_salsa(
    edge_list_df = assembled$edge_list,
    vertices_df = assembled$vertices_df,
    from_col = edge_from_col,
    to_col = edge_to_col,
    vertex_col_name = assembled$node_col,
    ...
  )
}

#' Validate salsa() data-frame arguments and the canonical cleaning flag.
#'
#' Runs the first block of salsa() validation in the original order (edge,
#' redirects, canonicals data frames; `clean_canonical_urls`; canonical
#' columns). Error-message text is preserved verbatim.
#' @keywords internal
#' @noRd
.salsa_validate_frames <- function(edge_list_df,
                                   redirects_df,
                                   canonicals_df,
                                   clean_canonical_urls,
                                   canonical_from_col,
                                   canonical_to_col) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!is.null(redirects_df)) {
    if (!is.data.frame(redirects_df)) {
      stop("`redirects_df` must be a data frame or NULL.", call. = FALSE)
    }
  }
  if (!is.null(canonicals_df)) {
    if (!is.data.frame(canonicals_df)) {
      stop("`canonicals_df` must be a data frame or NULL.", call. = FALSE)
    }
  }
  if (!is.logical(clean_canonical_urls)) {
    stop(
      "`clean_canonical_urls` must be a single logical value.",
      call. = FALSE
    )
  }
  if (length(clean_canonical_urls) != 1) {
    stop(
      "`clean_canonical_urls` must be a single logical value.",
      call. = FALSE
    )
  }
  .salsa_validate_canonical_cols(
    canonicals_df, canonical_from_col, canonical_to_col
  )
  invisible(NULL)
}

#' Validate that `canonicals_df` carries the required from/to columns.
#' @keywords internal
#' @noRd
.salsa_validate_canonical_cols <- function(canonicals_df,
                                           canonical_from_col,
                                           canonical_to_col) {
  if (is.null(canonicals_df)) {
    return(invisible(NULL))
  }
  if (nrow(canonicals_df) == 0) {
    return(invisible(NULL))
  }
  canonical_cols <- c(canonical_from_col, canonical_to_col)
  if (!all(canonical_cols %in% names(canonicals_df))) {
    stop("`canonicals_df` must have '", canonical_from_col, "' and '",
      canonical_to_col, "' columns.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Validate salsa() scalar-flag / list arguments.
#'
#' Runs the second block of salsa() validation in the original order
#' (`clean_edge_urls`, `clean_redirect_urls`, `rurl_params`,
#' `drop_isolates_flag`). Error-message text is preserved verbatim.
#' @keywords internal
#' @noRd
.salsa_validate_flags <- function(clean_edge_urls,
                                  clean_redirect_urls,
                                  rurl_params,
                                  drop_isolates_flag) {
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
  invisible(NULL)
}

#' Clean the URL columns of one salsa() data frame.
#'
#' Applies the shared resolved rurl profile to the intersection of the wanted
#' columns present in `df`. `require_rows = TRUE` skips empty frames (matching
#' the original redirect/canonical guards); edges use `require_rows = FALSE`.
#' @keywords internal
#' @noRd
.salsa_clean_url_df <- function(df,
                                cols_wanted,
                                do_clean,
                                rurl_params,
                                require_rows) {
  if (!do_clean) {
    return(df)
  }
  if (is.null(df)) {
    return(df)
  }
  if (require_rows && nrow(df) == 0) {
    return(df)
  }
  cols <- intersect(cols_wanted, names(df))
  if (length(cols) == 0) {
    return(df)
  }
  do.call(
    clean_url_columns,
    c(list(data_frame = df, columns = cols), rurl_params)
  )
}

#' Compose the redirect + canonical fold map and apply it to the edge list.
#'
#' Returns the (possibly relabeled) edge list. No-op when neither redirects nor
#' canonicals are present, or when the composed map is empty.
#' @keywords internal
#' @noRd
.salsa_apply_folds <- function(current_edge_list,
                               current_redirects_list,
                               current_canonicals_list,
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
  has_redirects <- !is.null(current_redirects_list) &&
    nrow(current_redirects_list) > 0
  has_canonicals <- !is.null(current_canonicals_list) &&
    nrow(current_canonicals_list) > 0

  if (!has_redirects && !has_canonicals) {
    return(current_edge_list)
  }

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

  if (length(fold_map) == 0) {
    return(current_edge_list)
  }

  for (col_name in c(edge_from_col, edge_to_col)) {
    if (col_name %in% names(current_edge_list)) {
      current_edge_list[[col_name]] <- .apply_fold_map(
        current_edge_list[[col_name]], fold_map
      )
    }
  }
  current_edge_list
}

#' Domain-filter, capture the vertex universe, deduplicate, and handle isolates.
#'
#' Returns a list with the deduplicated `edge_list`, the assembled
#' `vertices_df` (or `NULL`), and the temporary `node_col` name. Preserves the
#' original ordering: filter, capture universe (pre-dedup), dedup, isolates.
#' @keywords internal
#' @noRd
.salsa_assemble_vertices <- function(current_edge_list,
                                     edge_from_col,
                                     edge_to_col,
                                     keep_domains,
                                     exclude_domains,
                                     keep_hosts,
                                     exclude_hosts,
                                     effective_rurl_params,
                                     duplicate_edge_policy,
                                     self_loops,
                                     drop_isolates_flag) {
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
  # SALSA v1 is unweighted, so every policy yields the same simple deduplicated
  # graph; the policy is exposed only to keep the vertex set identical to
  # pagerank()/hits() run with the same arguments.
  current_edge_list <- .salsa_apply_duplicate_edge_policy(
    edge_list_df = current_edge_list,
    policy = duplicate_edge_policy,
    self_loops = self_loops,
    from_col = edge_from_col,
    to_col = edge_to_col
  )

  # --- 4. Handle isolates / assemble vertex set ---
  current_edge_nodes <- unique(c(
    as.character(current_edge_list[[edge_from_col]]),
    as.character(current_edge_list[[edge_to_col]])
  ))
  current_edge_nodes <- current_edge_nodes[!is.na(current_edge_nodes)]

  vertices_for_salsa_df <- .salsa_build_vertices_df(
    current_edge_nodes, all_vertex_universe, drop_isolates_flag,
    temp_node_col_name
  )

  list(
    edge_list = current_edge_list,
    vertices_df = vertices_for_salsa_df,
    node_col = temp_node_col_name
  )
}

#' Build the salsa() vertices data frame (or `NULL`) from node sets.
#' @keywords internal
#' @noRd
.salsa_build_vertices_df <- function(current_edge_nodes,
                                     all_vertex_universe,
                                     drop_isolates_flag,
                                     temp_node_col_name) {
  if (drop_isolates_flag) {
    if (length(current_edge_nodes) == 0) {
      return(NULL)
    }
    return(stats::setNames(
      data.frame(sort(current_edge_nodes)),
      temp_node_col_name
    ))
  }
  full_universe <- unique(c(all_vertex_universe, current_edge_nodes))
  if (length(full_universe) == 0) {
    return(NULL)
  }
  stats::setNames(
    data.frame(sort(full_universe)),
    temp_node_col_name
  )
}

#' Apply salsa() duplicate-edge policy.
#'
#' Mirrors the deduplication dispatch [pagerank()] / [hits()] use, so `salsa()`
#' collapses repeated `from -> to` rows identically and therefore shares node
#' identities. SALSA v1 is unweighted, so no per-instance count is stamped:
#' `"collapse"` keeps the binary convention via [get_unique_edges()] and
#' `"aggregate"` / `"count_instances"` route through [aggregate_edges()]; all
#' three yield the same simple edge set for degree counting.
#' @keywords internal
#' @noRd
.salsa_apply_duplicate_edge_policy <- function(edge_list_df,
                                               policy,
                                               self_loops,
                                               from_col,
                                               to_col) {
  if (policy == "collapse") {
    return(get_unique_edges(
      edge_list_df = edge_list_df,
      self_loops = self_loops,
      from_col = from_col,
      to_col = to_col
    ))
  }

  aggregate_edges(
    edge_list_df = edge_list_df,
    self_loops = self_loops,
    from_col = from_col,
    to_col = to_col
  )
}
