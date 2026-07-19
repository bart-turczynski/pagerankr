#' @title Compute PageRank using igraph
#' @description Builds a graph from a processed edge list and computes PageRank
#'   scores using `igraph::page_rank()`.
#'
#' @param edge_list_df A data frame representing the processed edge list,
#'   typically with columns "from" and "to" (or as specified by `from_col`,
#'   `to_col`).
#'   It should contain only edges to be included in the graph. NAs in these
#'   columns will be omitted before graph construction.
#' @param vertices_df An optional single-column data frame of node names to
#'   define
#'   the set of vertices for the graph. If `NULL` (default), all unique non-NA
#'   nodes
#'   present in `edge_list_df` (after NA removal from edges) are used.
#'   The column name is specified by `vertex_col_name`.
#' @param damping The damping factor for PageRank. Default is 0.85.
#' @param algo Solver back-end passed to `igraph::page_rank()`. Either
#'   `"prpack"` (default; a fast, exact direct solver with no tunable
#'   convergence controls) or `"arpack"` (an iterative eigensolver that honors
#'   `eps` / `niter` and reports its iteration count). Supplying `eps` or
#'   `niter` while leaving `algo` at its default transparently switches to
#'   `"arpack"`, since PRPACK ignores those controls. See [pagerank_convergence]
#'   for the trade-offs.
#' @param eps Optional convergence tolerance (L1, the ARPACK `options$tol`).
#'   When supplied, the solver switches to `"arpack"` and iterates until the
#'   residual is at or below `eps`. `NULL` (default) uses the solver's own
#'   default.
#' @param niter Optional maximum iteration count (the ARPACK `options$maxiter`).
#'   When supplied, the solver switches to `"arpack"`. `NULL` (default) uses the
#'   solver's own default. As a rule of thumb, power-iteration PageRank needs
#'   about `log10(eps) / log10(damping)` iterations, so raise `niter` when you
#'   raise `damping` toward 1. `NULL` (default) uses the solver's own default.
#' @param from_col Name of the source node column in `edge_list_df`. Default
#'   "from".
#' @param to_col Name of the target node column in `edge_list_df`. Default "to".
#' @param vertex_col_name Name of the column in `vertices_df` containing node
#'   names.
#'   Default "node_name".
#' @param reverse Logical. If `TRUE`, edge orientation is flipped before the
#'   graph is built, so PageRank is computed on the transposed graph. This is
#'   the reverse / inverse PageRank (a.k.a. CheiRank): instead of inflow
#'   importance ("who points to me"), it measures outflow centrality ("does this
#'   page funnel authority outward"). Vertices, weights, and the teleport prior
#'   are unaffected by the flip; only edge direction is reversed. Default
#'   `FALSE`. See [pagerank()] for the higher-level wrapper and the caveats on
#'   combining `reverse = TRUE` with direction-sensitive features.
#' @param weight_col Optional name of a numeric column in `edge_list_df`
#'   containing
#'   edge weights. Higher weights make edges more likely to be followed in the
#'   random surfer model. If `NULL` (default), all edges have equal weight
#'   (unweighted PageRank).
#' @param weight_validation How invalid edge weights are handled when
#'   `weight_col` is supplied: `"error"` (default), `"warning"`, or `"none"`.
#'   Validation covers negative and non-finite values plus sources whose
#'   outgoing weights are all zero. See [validate_edge_weights()].
#' @param weight_expected_total Optional expected per-source weight total.
#'   Leave `NULL` (default) for ordinary raw edge weights. Set to `1` when
#'   `weight_col` contains pre-normalized transition probabilities.
#' @param weight_tolerance Non-negative tolerance used with
#'   `weight_expected_total`.
#' @param pr_node_col Name for the node column in the output PageRank data
#'   frame. Default "node_name".
#' @param pr_value_col Name for the PageRank value column in the output data
#'   frame. Default "pagerank".
#' @param prior_df Optional per-URL external-authority prior (TIPR). When
#'   supplied, a personalization/teleport vector is built via
#'   [align_prior_to_vertices()] from the final vertex set and passed to
#'   `igraph::page_rank(personalized = )`. The prior URLs must already share the
#'   vertex namespace (canonicalized + redirect-folded); [pagerank()] handles
#'   that. Default `NULL` (uniform teleport).
#' @param prior_url_col,prior_weight_col Column names in `prior_df`. Defaults
#'   `"url"` / `"weight"`.
#' @param prior_transform,prior_alpha,prior_exclude_nodes,prior_verbose Passed
#'   to [align_prior_to_vertices()] as `transform`, `alpha`, `exclude_nodes`,
#'   `verbose`. See that function for semantics.
#' @param ... Additional arguments passed to `igraph::page_rank()`.
#'
#' @return A data frame with two columns: one for node names (named by
#'   `pr_node_col`)
#'   and one for their PageRank scores (named by `pr_value_col`), which sum to 1
#'   for non-empty graphs. Returns an empty data frame with correct column names
#'   if the graph is empty or has no nodes after processing.
#'
#'   For non-empty graphs the result carries a `"convergence"` attribute (a
#'   [pagerank_convergence] object) recording the solver used, iterations (when
#'   the solver exposes them), and the post-hoc L1 residual of the returned
#'   vector. Retrieve it with `attr(result, "convergence")`.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C"), to = c("B", "C", "A")
#' )
#' pr_results <- compute_pagerank(edges)
#' print(pr_results)
#' if (nrow(pr_results) > 0) sum(pr_results$pagerank)
#'
#' # With specified vertices (e.g., from drop_isolates)
#' vertices <- data.frame(
#'   node_name = c("A", "B", "C", "D")
#' ) # D is an isolate
#' pr_results_isolates_kept <- compute_pagerank(edges, vertices_df = vertices)
#' print(pr_results_isolates_kept)
#' if (nrow(pr_results_isolates_kept) > 0) {
#'   sum(pr_results_isolates_kept$pagerank)
#' }
#'
#' # Single node graph with self-loop
#' single_node_edges <- data.frame(
#'   from = "A", to = "A"
#' )
#' compute_pagerank(single_node_edges)
#'
#' # Single node, no edges, defined by vertices_df
#' single_node_no_loop <- data.frame(
#'   from = character(0), to = character(0)
#' )
#' compute_pagerank(
#'   single_node_no_loop,
#'   vertices_df = data.frame(node_name = "A")
#' )
#'
#' # Empty graph (no edges, no vertices defined)
#' empty_edges <- data.frame(
#'   from = character(), to = character()
#' )
#' compute_pagerank(empty_edges)
#'
#' # Edges with NAs (these edges will be dropped)
#' edges_with_na <- data.frame(
#'   from = c("A", NA, "C"), to = c("B", "D", NA)
#' )
#' compute_pagerank(edges_with_na) # Should only process A->B
#' compute_pagerank(
#'   edges_with_na,
#'   vertices_df = data.frame(node_name = c("A", "B", "C", "D"))
#' )
compute_pagerank <- function(edge_list_df,
                             vertices_df = NULL,
                             damping = 0.85,
                             algo = c("prpack", "arpack"),
                             eps = NULL,
                             niter = NULL,
                             from_col = "from",
                             to_col = "to",
                             vertex_col_name = "node_name",
                             reverse = FALSE,
                             weight_col = NULL,
                             weight_validation = c("error", "warning", "none"),
                             weight_expected_total = NULL,
                             weight_tolerance = sqrt(.Machine$double.eps),
                             pr_node_col = "node_name",
                             pr_value_col = "pagerank",
                             prior_df = NULL,
                             prior_url_col = "url",
                             prior_weight_col = "weight",
                             prior_transform = "none",
                             prior_alpha = 0,
                             prior_exclude_nodes = character(0),
                             prior_verbose = TRUE,
                             ...) {
  weight_validation <- match.arg(weight_validation)
  algo <- match.arg(algo)

  # --- Convergence-control validation and solver selection ---
  # `eps` / `niter` are friendly aliases for the ARPACK options$tol /
  # options$maxiter; supplying either switches to ARPACK. See
  # .validate_and_select_solver.
  .cc <- .validate_and_select_solver(eps, niter, algo)
  eps <- .cc$eps
  niter <- .cc$niter
  algo <- .cc$algo

  # --- Input validation (see .validate_compute_pagerank_args) ---
  .validate_compute_pagerank_args(
    edge_list_df, vertices_df, damping, reverse, pr_node_col,
    pr_value_col, weight_col, from_col, to_col, vertex_col_name
  )

  empty_pr_result <- .empty_pagerank_result(pr_node_col, pr_value_col)

  # --- Build the graph to score (NULL => empty result) ---
  current_graph <- .build_pagerank_input(
    edge_list_df, vertices_df, vertex_col_name, from_col, to_col,
    weight_col, weight_expected_total, weight_tolerance,
    weight_validation, reverse
  )
  if (is.null(current_graph)) {
    return(empty_pr_result)
  }

  # --- Personalization, PageRank, formatting, convergence attribute ---
  .finalize_pagerank(
    current_graph, algo, damping, eps, niter,
    pr_node_col, pr_value_col, empty_pr_result,
    prior_df, prior_url_col, prior_weight_col, prior_transform,
    prior_alpha, prior_exclude_nodes, prior_verbose, ...
  )
}

#' Build the empty (zero-row) PageRank result data frame.
#'
#' @keywords internal
#' @noRd
.empty_pagerank_result <- function(pr_node_col, pr_value_col) {
  empty_pr_result <- stats::setNames(
    data.frame(matrix(ncol = 2, nrow = 0)),
    c(pr_node_col, pr_value_col)
  )
  # Ensure columns of empty result are character and numeric respectively
  empty_pr_result[[pr_node_col]] <- character(0)
  empty_pr_result[[pr_value_col]] <- numeric(0)
  empty_pr_result
}

#' Resolve vertices + edges and build the igraph object to score.
#'
#' Returns the graph, or `NULL` when the result should be the empty data frame
#' (no valid edges and no defined nodes, or an empty / vertex-less graph).
#' @keywords internal
#' @noRd
.build_pagerank_input <- function(edge_list_df, vertices_df, vertex_col_name,
                                  from_col, to_col, weight_col,
                                  weight_expected_total, weight_tolerance,
                                  weight_validation, reverse) {
  defined_nodes <- .compute_defined_nodes(vertices_df, vertex_col_name)

  # --- Prepare edges (remove NAs, extract aligned weight vector) ---
  .pe <- .prepare_graph_edges(
    edge_list_df = edge_list_df,
    from_col = from_col,
    to_col = to_col,
    weight_col = weight_col,
    weight_expected_total = weight_expected_total,
    weight_tolerance = weight_tolerance,
    weight_validation = weight_validation
  )
  valid_edges_df <- .pe$valid_edges_df
  weight_vector <- .pe$weight_vector

  # If no valid edges and no defined nodes, the result is an empty graph.
  if (nrow(valid_edges_df) == 0 && is.null(defined_nodes)) {
    return(NULL)
  }
  current_graph <- .build_pagerank_graph(
    valid_edges_df = valid_edges_df,
    defined_nodes = defined_nodes,
    weight_vector = weight_vector,
    reverse = reverse,
    from_col = from_col,
    to_col = to_col
  )
  # NULL (unreachable safeguard) or a vertex-less graph => empty result.
  if (is.null(current_graph) || igraph::vcount(current_graph) == 0) {
    return(NULL)
  }
  current_graph
}

#' Run PageRank on a built graph and assemble the formatted result.
#'
#' Builds the teleport/personalization vector, runs the solver, and (unless the
#' output is empty, in which case `empty_pr_result` is returned) formats the
#' scores and attaches the `"convergence"` attribute.
#' @keywords internal
#' @noRd
.finalize_pagerank <- function(graph, algo, damping, eps, niter,
                               pr_node_col, pr_value_col, empty_pr_result,
                               prior_df, prior_url_col, prior_weight_col,
                               prior_transform, prior_alpha,
                               prior_exclude_nodes, prior_verbose, ...) {
  personalized_vec <- .compute_personalization(
    graph, prior_df, prior_url_col, prior_weight_col,
    prior_transform, prior_alpha, prior_exclude_nodes, prior_verbose
  )

  pr_igraph_output <- .run_page_rank(
    graph, algo, damping, personalized_vec, eps, niter, ...
  )

  # A NULL output (page_rank errored) or an empty score vector cannot be
  # formatted; return the empty result.
  if (.is_empty_pagerank_output(pr_igraph_output)) {
    return(empty_pr_result)
  }

  pagerank_df <- .format_pagerank_output(
    pr_igraph_output, pr_node_col, pr_value_col, personalized_vec, graph
  )

  attr(pagerank_df, "convergence") <- .build_convergence_attr(
    graph, pr_igraph_output$vector, algo, damping, personalized_vec,
    eps, niter, pr_igraph_output$options
  )

  pagerank_df
}

#' L1 residual of a PageRank vector
#'
#' Computes \eqn{\|G x - x\|_1}, the L1 norm of one application of the Google
#' operator implied by `graph`, `damping`, and the teleport vector. Used as a
#' solver-independent convergence check (Kamvar, Haveliwala & Golub 2004): a
#' converged stationary vector sits near machine precision.
#'
#' @param graph The igraph object actually scored (already transposed when
#'   `reverse = TRUE`, so out-edges here are the walk direction).
#' @param x The returned PageRank vector, in `V(graph)` order.
#' @param damping The damping factor used.
#' @param teleport The personalization/teleport vector in `V(graph)` order, or
#'   `NULL` for the uniform `1/n` teleport.
#' @return The L1 residual (numeric scalar), or `NA_real_` for an empty graph.
#' @keywords internal
#' @noRd
.pagerank_l1_residual <- function(graph, x, damping, teleport = NULL) {
  n <- igraph::vcount(graph)
  if (n == 0 || length(x) != n) {
    return(NA_real_)
  }
  w <- igraph::E(graph)$weight
  if (is.null(w)) w <- rep(1, igraph::ecount(graph))
  out_strength <- igraph::strength(graph, mode = "out", weights = w)
  teleport_vec <- if (is.null(teleport)) {
    rep(1 / n, n)
  } else {
    teleport / sum(teleport)
  }
  dangling <- out_strength == 0
  dangling_mass <- sum(x[dangling])
  # Per-source outgoing share; dangling sources contribute via teleport only.
  share <- ifelse(dangling, 0, x / out_strength)
  el <- igraph::as_edgelist(graph, names = FALSE)
  inflow <- numeric(n)
  if (nrow(el) > 0) {
    agg <- rowsum(share[el[, 1]] * w, el[, 2])
    inflow[as.integer(rownames(agg))] <- agg[, 1]
  }
  x_new <- damping * inflow +
    (damping * dangling_mass + (1 - damping)) * teleport_vec
  sum(abs(x_new - x))
}

#' Validate the convergence controls and pick the solver back-end.
#'
#' `eps` / `niter` are friendly aliases for the ARPACK `options$tol` /
#' `options$maxiter`; PRPACK ignores them, so supplying either transparently
#' switches `algo` to `"arpack"` (with an informational message). `niter` is
#' coerced to integer.
#' @return A list with the validated `eps`, `niter`, and (possibly switched)
#'   `algo`.
#' @keywords internal
#' @noRd
.validate_and_select_solver <- function(eps, niter, algo) {
  .assert_positive_number_or_null(eps)
  niter <- .coerce_niter_or_null(niter)
  if ((!is.null(eps) || !is.null(niter)) && algo == "prpack") {
    message(
      "`eps`/`niter` are only honoured by the ARPACK solver; switching ",
      "`algo` to \"arpack\". Pass `algo = \"arpack\"` explicitly to silence ",
      "this message."
    )
    algo <- "arpack"
  }
  list(eps = eps, niter = niter, algo = algo)
}

#' Error unless `eps` is NULL or a single positive number.
#' Sequential checks keep the short-circuit order and message identical.
#' @keywords internal
#' @noRd
.assert_positive_number_or_null <- function(eps) {
  if (is.null(eps)) {
    return(invisible(NULL))
  }
  bad <- function() {
    stop("`eps` must be a single positive number or NULL.", call. = FALSE)
  }
  if (!is.numeric(eps)) bad()
  if (length(eps) != 1) bad()
  if (is.na(eps)) bad()
  if (eps <= 0) bad()
  invisible(NULL)
}

#' Validate `niter` (NULL or a single positive integer) and coerce to integer.
#' @return `NULL`, or `as.integer(niter)`.
#' @keywords internal
#' @noRd
.coerce_niter_or_null <- function(niter) {
  if (is.null(niter)) {
    return(NULL)
  }
  bad <- function() {
    stop("`niter` must be a single positive integer or NULL.", call. = FALSE)
  }
  if (!is.numeric(niter)) bad()
  if (length(niter) != 1) bad()
  if (is.na(niter)) bad()
  if (niter < 1) bad()
  as.integer(niter)
}

#' Validate `compute_pagerank()` arguments (delegates to smaller validators).
#'
#' Preserves every error message and short-circuit order (including the
#' historical absence of an NA check on `damping`). Returns `invisible(NULL)`.
#' @keywords internal
#' @noRd
.validate_compute_pagerank_args <- function(edge_list_df,
                                            vertices_df,
                                            damping,
                                            reverse,
                                            pr_node_col,
                                            pr_value_col,
                                            weight_col,
                                            from_col,
                                            to_col,
                                            vertex_col_name) {
  .validate_edge_df(edge_list_df, from_col, to_col)
  .validate_vertices_df(vertices_df, vertex_col_name)
  if (!is.numeric(damping) || length(damping) != 1 ||
        damping < 0 || damping > 1) {
    stop(
      "`damping` must be a single numeric value between 0 and 1.",
      call. = FALSE
    )
  }
  .assert_flag(reverse, "reverse", allow_na = FALSE)
  .assert_nonempty_string(pr_node_col, "pr_node_col")
  .assert_nonempty_string(pr_value_col, "pr_value_col")
  if (pr_node_col == pr_value_col) {
    stop("`pr_node_col` and `pr_value_col` must be different.", call. = FALSE)
  }
  .validate_weight_col(weight_col, edge_list_df)
  invisible(NULL)
}

#' Error unless `edge_list_df` is a data frame with the from/to cols (if rows).
#' @keywords internal
#' @noRd
.validate_edge_df <- function(edge_list_df, from_col, to_col) {
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
  invisible(NULL)
}

#' Error unless `vertices_df` is NULL or a data frame carrying the vertex col.
#' @keywords internal
#' @noRd
.validate_vertices_df <- function(vertices_df, vertex_col_name) {
  if (is.null(vertices_df)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(vertices_df)) {
    stop("`vertices_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (nrow(vertices_df) > 0 && !(vertex_col_name %in% names(vertices_df))) {
    stop("`vertices_df` must have a column named '", vertex_col_name,
      "' if not empty.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Error unless `x` is a single non-empty character string.
#' @keywords internal
#' @noRd
.assert_nonempty_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1 || nchar(x) == 0) {
    stop("`", name, "` must be a non-empty character string.", call. = FALSE)
  }
  invisible(NULL)
}

#' Resolve the defined vertex set from `vertices_df` (NULL when none / empty).
#' @keywords internal
#' @noRd
.compute_defined_nodes <- function(vertices_df, vertex_col_name) {
  if (!is.null(vertices_df) && nrow(vertices_df) > 0 &&
        vertex_col_name %in% names(vertices_df)) {
    defined_nodes <- unique(
      stats::na.omit(as.character(vertices_df[[vertex_col_name]]))
    )
    # Treat empty after na.omit as NULL
    if (length(defined_nodes) == 0) {
      return(NULL)
    }
    return(defined_nodes)
  }
  NULL
}

#' Select edge columns, drop NA-endpoint rows, and extract the weight vector.
#'
#' When `weight_col` is supplied and any valid edges remain, weights are
#' validated via [validate_edge_weights()] and returned aligned to the surviving
#' rows; the returned `valid_edges_df` then carries only the from/to columns
#' (weights travel via the separate vector, set as an edge attribute later).
#' @return A list with `valid_edges_df` and `weight_vector` (`NULL` if
#'   unweighted).
#' @keywords internal
#' @noRd
.prepare_graph_edges <- function(edge_list_df,
                                 from_col,
                                 to_col,
                                 weight_col,
                                 weight_expected_total,
                                 weight_tolerance,
                                 weight_validation) {
  weight_vector <- NULL
  if (nrow(edge_list_df) > 0) {
    # Select from/to (and weight if present) columns
    cols_to_keep <- c(from_col, to_col)
    if (!is.null(weight_col)) cols_to_keep <- c(cols_to_keep, weight_col)
    edges_for_graph <- edge_list_df[, cols_to_keep, drop = FALSE]
    edges_for_graph[[from_col]] <- as.character(edges_for_graph[[from_col]])
    edges_for_graph[[to_col]] <- as.character(edges_for_graph[[to_col]])

    # igraph cannot handle NAs in edge lists for graph_from_data_frame
    na_in_edges <- is.na(edges_for_graph[[from_col]]) |
      is.na(edges_for_graph[[to_col]])
    valid_edges_df <- edges_for_graph[!na_in_edges, , drop = FALSE]

    # Extract aligned weight vector after NA removal
    if (!is.null(weight_col) && nrow(valid_edges_df) > 0) {
      validate_edge_weights(
        edge_list_df = valid_edges_df,
        weight_col = weight_col,
        from_col = from_col,
        expected_total = weight_expected_total,
        tolerance = weight_tolerance,
        action = weight_validation
      )
      weight_vector <- valid_edges_df[[weight_col]]
      # Pass only from/to to graph_from_data_frame (weights set via edge attr)
      valid_edges_df <- valid_edges_df[, c(from_col, to_col), drop = FALSE]
    }
  } else {
    # No rows in edge_list_df, so valid_edges_df remains an empty structure
    valid_edges_df <- data.frame(
      matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c(from_col, to_col)))
    )
    valid_edges_df[[from_col]] <- character(0)
    valid_edges_df[[to_col]] <- character(0)
  }
  list(valid_edges_df = valid_edges_df, weight_vector = weight_vector)
}

#' Build the igraph object to score (edges and/or defined isolate vertices).
#'
#' Under `reverse = TRUE` edge orientation is flipped by feeding
#' `graph_from_data_frame` the columns in (to, from) order (it keys on column
#' POSITION, not name). Weights, when present, are set as the `weight` edge
#' attribute. Returns `NULL` in the (unreachable, guarded upstream) case of no
#' edges and no defined nodes.
#' @return An igraph graph, or `NULL`.
#' @keywords internal
#' @noRd
.build_pagerank_graph <- function(valid_edges_df,
                                  defined_nodes,
                                  weight_vector,
                                  reverse,
                                  from_col,
                                  to_col) {
  if (nrow(valid_edges_df) > 0) {
    edges_for_construction <- if (reverse) {
      valid_edges_df[, c(to_col, from_col), drop = FALSE]
    } else {
      valid_edges_df
    }
    current_graph <- igraph::graph_from_data_frame(
      d = edges_for_construction,
      directed = TRUE,
      vertices = defined_nodes
    )
    if (!is.null(weight_vector)) {
      igraph::E(current_graph)$weight <- weight_vector
    }
    return(current_graph)
  }
  # No valid edges, but defined_nodes might exist -> a graph of isolates.
  if (!is.null(defined_nodes) && length(defined_nodes) > 0) {
    current_graph <- igraph::make_empty_graph(
      n = length(defined_nodes), directed = TRUE
    )
    igraph::V(current_graph)$name <- defined_nodes
    return(current_graph)
  }
  NULL
}

#' Run `igraph::page_rank()` with the chosen solver, returning NULL on error.
#'
#' Builds the ARPACK options only when that solver is in use (PRPACK ignores
#' them). Extra `...` are forwarded to `igraph::page_rank()`.
#' @keywords internal
#' @noRd
.run_page_rank <- function(graph,
                           algo,
                           damping,
                           personalized_vec,
                           eps,
                           niter,
                           ...) {
  pr_options <- NULL
  if (algo == "arpack") {
    pr_options <- igraph::arpack_defaults()
    if (!is.null(eps)) pr_options$tol <- eps
    if (!is.null(niter)) pr_options$maxiter <- niter
  }
  tryCatch(
    {
      if (is.null(personalized_vec)) {
        igraph::page_rank(
          graph = graph, algo = algo, damping = damping,
          options = pr_options, ...
        )
      } else {
        igraph::page_rank(
          graph = graph, algo = algo, damping = damping,
          personalized = personalized_vec, options = pr_options, ...
        )
      }
    },
    error = function(e) {
      warning("igraph::page_rank computation failed: ", e$message,
        call. = FALSE
      )
      NULL # Return NULL on error to distinguish from valid empty results
    }
  )
}

#' Build the TIPR personalization vector aligned to the graph's vertex set.
#'
#' Returns `NULL` (uniform teleport) when no `prior_df` is supplied; otherwise
#' delegates to [align_prior_to_vertices()].
#' @keywords internal
#' @noRd
.compute_personalization <- function(graph,
                                     prior_df,
                                     prior_url_col,
                                     prior_weight_col,
                                     prior_transform,
                                     prior_alpha,
                                     prior_exclude_nodes,
                                     prior_verbose) {
  if (is.null(prior_df)) {
    return(NULL)
  }
  align_prior_to_vertices(
    vertex_names = igraph::V(graph)$name,
    prior_df = prior_df,
    prior_url_col = prior_url_col,
    prior_weight_col = prior_weight_col,
    transform = prior_transform,
    alpha = prior_alpha,
    exclude_nodes = prior_exclude_nodes,
    verbose = prior_verbose
  )
}

#' TRUE when a page_rank output cannot be formatted (NULL or empty vector).
#' @keywords internal
#' @noRd
.is_empty_pagerank_output <- function(pr_igraph_output) {
  is.null(pr_igraph_output) || is.null(pr_igraph_output$vector) ||
    length(pr_igraph_output$vector) == 0
}

#' Format the igraph page_rank output into the two-column result data frame.
#'
#' Attaches the aligned teleport share as a `prior_weight` column when a
#' personalization vector was used, so the input prior sits next to the score.
#' @return A data frame with `pr_node_col` / `pr_value_col` (+ optional
#'   `prior_weight`).
#' @keywords internal
#' @noRd
.format_pagerank_output <- function(pr_igraph_output,
                                    pr_node_col,
                                    pr_value_col,
                                    personalized_vec,
                                    graph) {
  pagerank_df <- data.frame(
    node = names(pr_igraph_output$vector),
    pagerank_val = pr_igraph_output$vector,
    row.names = NULL
  )
  names(pagerank_df) <- c(pr_node_col, pr_value_col)

  if (!is.null(personalized_vec)) {
    pw <- stats::setNames(personalized_vec, igraph::V(graph)$name)
    pagerank_df[["prior_weight"]] <- unname(pw[pagerank_df[[pr_node_col]]])
  }
  pagerank_df
}

#' Build the [pagerank_convergence] attribute for a computed result.
#'
#' The L1 residual is computed post hoc from the returned vector and is
#' solver-independent (PRPACK exposes no iteration count, but its direct
#' solution still has a measurable residual). ARPACK iterations / info are read
#' from `arpack_options` when available.
#' @return A `pagerank_convergence` object.
#' @keywords internal
#' @noRd
.build_convergence_attr <- function(graph,
                                    x,
                                    algo,
                                    damping,
                                    personalized_vec,
                                    eps,
                                    niter,
                                    arpack_options) {
  residual <- .pagerank_l1_residual(
    graph = graph,
    x = x,
    damping = damping,
    teleport = personalized_vec
  )
  tol <- if (!is.null(eps)) eps else 1e-3
  arpack_iters <- NA_integer_
  arpack_info <- NA_integer_
  if (algo == "arpack" && !is.null(arpack_options)) {
    arpack_iters <- as.integer(arpack_options$iter)
    arpack_info <- as.integer(arpack_options$info)
  }
  tol_met <- is.finite(residual) && residual <= tol &&
    (algo != "arpack" || (!is.na(arpack_info) && arpack_info == 0L))
  new_pagerank_convergence(
    algo = algo,
    iters = arpack_iters,
    residual = residual,
    tol = tol,
    tol_met = tol_met,
    info = arpack_info,
    eps = if (!is.null(eps)) eps else NA_real_,
    niter = if (!is.null(niter)) niter else NA_integer_
  )
}
