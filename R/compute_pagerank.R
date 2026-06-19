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
#' @import igraph
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C"), to = c("B", "C", "A"),
#'   stringsAsFactors = FALSE
#' )
#' pr_results <- compute_pagerank(edges)
#' print(pr_results)
#' if (nrow(pr_results) > 0) sum(pr_results$pagerank)
#'
#' # With specified vertices (e.g., from drop_isolates)
#' vertices <- data.frame(
#'   node_name = c("A", "B", "C", "D"),
#'   stringsAsFactors = FALSE
#' ) # D is an isolate
#' pr_results_isolates_kept <- compute_pagerank(edges, vertices_df = vertices)
#' print(pr_results_isolates_kept)
#' if (nrow(pr_results_isolates_kept) > 0) {
#'   sum(pr_results_isolates_kept$pagerank)
#' }
#'
#' # Single node graph with self-loop
#' single_node_edges <- data.frame(
#'   from = "A", to = "A",
#'   stringsAsFactors = FALSE
#' )
#' compute_pagerank(single_node_edges)
#'
#' # Single node, no edges, defined by vertices_df
#' single_node_no_loop <- data.frame(
#'   from = character(0), to = character(0),
#'   stringsAsFactors = FALSE
#' )
#' compute_pagerank(
#'   single_node_no_loop,
#'   vertices_df = data.frame(node_name = "A")
#' )
#'
#' # Empty graph (no edges, no vertices defined)
#' empty_edges <- data.frame(
#'   from = character(), to = character(),
#'   stringsAsFactors = FALSE
#' )
#' compute_pagerank(empty_edges)
#'
#' # Edges with NAs (these edges will be dropped)
#' edges_with_na <- data.frame(
#'   from = c("A", NA, "C"), to = c("B", "D", NA),
#'   stringsAsFactors = FALSE
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
  # Modern igraph (2.x) dropped the old page_rank() `eps` / `niter` arguments.
  # We re-expose them as friendly aliases for the ARPACK back-end's
  # options$tol / options$maxiter. PRPACK (the default, direct solver) has no
  # such knobs, so supplying either control transparently switches to ARPACK.
  if (!is.null(eps)) {
    if (!is.numeric(eps) || length(eps) != 1 || is.na(eps) || eps <= 0) {
      stop("`eps` must be a single positive number or NULL.", call. = FALSE)
    }
  }
  if (!is.null(niter)) {
    if (!is.numeric(niter) || length(niter) != 1 || is.na(niter) ||
          niter < 1) {
      stop("`niter` must be a single positive integer or NULL.", call. = FALSE)
    }
    niter <- as.integer(niter)
  }
  if ((!is.null(eps) || !is.null(niter)) && algo == "prpack") {
    message(
      "`eps`/`niter` are only honoured by the ARPACK solver; switching ",
      "`algo` to \"arpack\". Pass `algo = \"arpack\"` explicitly to silence ",
      "this message."
    )
    algo <- "arpack"
  }

  # --- Input Validation ---
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
  if (!is.numeric(damping) || length(damping) != 1 ||
        damping < 0 || damping > 1) {
    stop(
      "`damping` must be a single numeric value between 0 and 1.",
      call. = FALSE
    )
  }
  if (!is.logical(reverse) || length(reverse) != 1 || is.na(reverse)) {
    stop("`reverse` must be a single logical value.", call. = FALSE)
  }
  if (!is.character(pr_node_col) || length(pr_node_col) != 1 ||
        nchar(pr_node_col) == 0) {
    stop("`pr_node_col` must be a non-empty character string.", call. = FALSE)
  }
  if (!is.character(pr_value_col) || length(pr_value_col) != 1 ||
        nchar(pr_value_col) == 0) {
    stop("`pr_value_col` must be a non-empty character string.", call. = FALSE)
  }
  if (pr_node_col == pr_value_col) {
    stop("`pr_node_col` and `pr_value_col` must be different.", call. = FALSE)
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

  # --- Prepare Empty Result & Graph Vertices ---
  empty_pr_result <- stats::setNames(
    data.frame(matrix(ncol = 2, nrow = 0)),
    c(pr_node_col, pr_value_col)
  )
  # Ensure columns of empty result are character and numeric respectively
  empty_pr_result[[pr_node_col]] <- character(0)
  empty_pr_result[[pr_value_col]] <- numeric(0)

  defined_nodes <- NULL
  if (!is.null(vertices_df) && nrow(vertices_df) > 0 &&
        vertex_col_name %in% names(vertices_df)) {
    defined_nodes <- unique(
      stats::na.omit(as.character(vertices_df[[vertex_col_name]]))
    )
    # Treat empty after na.omit as NULL
    if (length(defined_nodes) == 0) defined_nodes <- NULL
  }

  # --- Prepare Edges (Remove NAs) ---
  valid_edges_df <- NULL
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

  # --- Create Graph ---
  # If no valid edges and no defined nodes, result is an empty graph/empty
  # PR results.
  if (nrow(valid_edges_df) == 0 && is.null(defined_nodes)) {
    return(empty_pr_result)
  }

  current_graph <- NULL
  if (nrow(valid_edges_df) > 0) {
    # Reverse / inverse PageRank (CheiRank): flip edge orientation by feeding
    # graph_from_data_frame the columns in (to, from) order. It keys on column
    # POSITION (1 = source, 2 = target), not name, so swapping the order
    # transposes the graph. Weights are aligned by row and the vertex set is
    # order-independent, so neither is affected by the flip.
    edges_for_construction <- if (reverse) {
      valid_edges_df[, c(to_col, from_col), drop = FALSE]
    } else {
      valid_edges_df
    }
    # If defined_nodes is NULL, igraph infers vertices from valid_edges_df.
    # If defined_nodes is provided, it uses them (and adds any from
    # valid_edges_df not in defined_nodes).
    current_graph <- igraph::graph_from_data_frame(
      d = edges_for_construction,
      directed = TRUE,
      vertices = defined_nodes
    )
    # Set edge weights if provided (igraph::page_rank auto-detects weight attr)
    if (!is.null(weight_vector)) {
      igraph::E(current_graph)$weight <- weight_vector
    }
  } else { # No valid edges, but defined_nodes might exist
    if (!is.null(defined_nodes) && length(defined_nodes) > 0) {
      current_graph <- igraph::make_empty_graph(
        n = length(defined_nodes), directed = TRUE
      )
      igraph::V(current_graph)$name <- defined_nodes
    } else {
      # Should have been caught by the check above, but as a safeguard.
      return(empty_pr_result)
    }
  }

  # If graph has no vertices (e.g. defined_nodes was empty and edges were
  # empty), return empty.
  if (igraph::vcount(current_graph) == 0) {
    return(empty_pr_result)
  }

  # --- Build TIPR personalization vector (aligned to final vertex set) ---
  personalized_vec <- NULL
  if (!is.null(prior_df)) {
    personalized_vec <- align_prior_to_vertices(
      vertex_names = igraph::V(current_graph)$name,
      prior_df = prior_df,
      prior_url_col = prior_url_col,
      prior_weight_col = prior_weight_col,
      transform = prior_transform,
      alpha = prior_alpha,
      exclude_nodes = prior_exclude_nodes,
      verbose = prior_verbose
    )
  }

  # --- Compute PageRank ---
  # Build ARPACK options only when that solver is in use; PRPACK ignores them.
  pr_options <- NULL
  if (algo == "arpack") {
    pr_options <- igraph::arpack_defaults()
    if (!is.null(eps)) pr_options$tol <- eps
    if (!is.null(niter)) pr_options$maxiter <- niter
  }
  pr_igraph_output <- tryCatch(
    {
      if (is.null(personalized_vec)) {
        igraph::page_rank(
          graph = current_graph, algo = algo, damping = damping,
          options = pr_options, ...
        )
      } else {
        igraph::page_rank(
          graph = current_graph, algo = algo, damping = damping,
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

  if (is.null(pr_igraph_output) || is.null(pr_igraph_output$vector) ||
        length(pr_igraph_output$vector) == 0) {
    # This handles errors from page_rank or cases where it returns an empty
    # vector (e.g. graph with nodes but no edges)
    # For a graph with nodes but no edges, igraph::page_rank gives equal
    # scores. If vcount > 0, vector shouldn't be empty.
    # However, if an error occurred or if somehow an empty vector is returned
    # for vcount > 0, return empty_pr_result.
    # If defined_nodes existed but no edges, PR is 1/N for each. Let's ensure
    # this is handled.
    if (igraph::vcount(current_graph) > 0 &&
          (is.null(pr_igraph_output) ||
             length(pr_igraph_output$vector) == 0)) {
      # This case implies something went wrong, or a graph of isolates for
      # which PR might be 1/N for each.
      # igraph::page_rank on isolates assigns them 1/vcount.
      # If pr_igraph_output$vector is truly empty when it shouldn't be, it's
      # an issue.
      # Let's assume if vcount > 0, pr_result$vector will be non-empty from
      # igraph for isolates.
      # If pr_igraph_output is NULL (error) or its vector is empty
      # unexpectedly, return empty_pr_result.
      return(empty_pr_result)
    }
    # If caught by length(pr_result$vector) == 0, implies no nodes had PR
    # computed or graph was empty.
  }

  # --- Format Results ---
  pagerank_df <- data.frame(
    node = names(pr_igraph_output$vector),
    pagerank_val = pr_igraph_output$vector,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  names(pagerank_df) <- c(pr_node_col, pr_value_col)

  # Attach the aligned teleport share so the input prior sits next to the
  # score.
  if (!is.null(personalized_vec)) {
    pw <- stats::setNames(personalized_vec, igraph::V(current_graph)$name)
    pagerank_df[["prior_weight"]] <- unname(pw[pagerank_df[[pr_node_col]]])
  }

  # Order by pagerank descending by default (optional, but common)

  # --- Convergence diagnostic ---
  # Residual is computed post hoc from the returned vector and is therefore
  # solver-independent (PRPACK exposes no iteration count, but its direct
  # solution still has a measurable residual). Iterations / ARPACK info are
  # read from the solver output when available. See [pagerank_convergence].
  residual <- .pagerank_l1_residual(
    graph = current_graph,
    x = pr_igraph_output$vector,
    damping = damping,
    teleport = personalized_vec
  )
  tol <- if (!is.null(eps)) eps else 1e-3
  arpack_iters <- NA_integer_
  arpack_info <- NA_integer_
  if (algo == "arpack" && !is.null(pr_igraph_output$options)) {
    arpack_iters <- as.integer(pr_igraph_output$options$iter)
    arpack_info <- as.integer(pr_igraph_output$options$info)
  }
  tol_met <- is.finite(residual) && residual <= tol &&
    (algo != "arpack" || (!is.na(arpack_info) && arpack_info == 0L))
  attr(pagerank_df, "convergence") <- new_pagerank_convergence(
    algo = algo,
    iters = arpack_iters,
    residual = residual,
    tol = tol,
    tol_met = tol_met,
    info = arpack_info,
    eps = if (!is.null(eps)) eps else NA_real_,
    niter = if (!is.null(niter)) niter else NA_integer_
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
