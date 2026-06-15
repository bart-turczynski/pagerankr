#' @title Resolve Redirects in an Edge List
#' @description Updates an edge list by replacing URLs with their final
#'   destinations based on a redirect data frame. Handles redirect chains,
#'   detects cycles, and resolves conflicting redirects using configurable
#'   policies.
#'
#' @param edge_list_df A data frame representing the edge list.
#' @param redirects_df A data frame containing redirect rules, with 'from' and
#'   'to' columns specifying the source and target of a redirect.
#' @param edge_from_col Character, the name of the column in `edge_list_df`
#'   containing source URLs. Default "from".
#' @param edge_to_col Character, the name of the column in `edge_list_df`
#'   containing target URLs. Default "to".
#' @param redirect_from_col Character, the name of the column in `redirects_df`
#'   containing source URLs of redirects. Default "from".
#' @param redirect_to_col Character, the name of the column in `redirects_df`
#'   containing target URLs of redirects. Default "to".
#' @param duplicate_from_policy Character, how to handle conflicting redirects
#'   (same source URL mapping to multiple distinct targets). One of:
#'   \describe{
#'     \item{"strict"}{(Default) Error on any conflict.}
#'     \item{"first_wins"}{Keep the first occurrence for each conflicting
#'       source.}
#'     \item{"last_wins"}{Keep the last occurrence for each conflicting source.}
#'     \item{"most_frequent"}{Keep the most common target. Ties broken by first
#'       occurrence.}
#'     \item{"prune_source"}{Remove ALL redirects from any conflicting source.}
#'     \item{"resolve_if_consistent"}{Allow exact duplicates; error only on
#'       true conflicts where targets differ.}
#'   }
#' @param loop_handling Character, how to handle redirect cycles (loops).
#'   One of:
#'   \describe{
#'     \item{"error"}{(Default) Error when a redirect cycle is detected.}
#'     \item{"prune_loop"}{Remove all edges involved in cycles. URLs in the
#'       loop remain unresolved (map to themselves).}
#'     \item{"break_arrow"}{For each cycle, keep the node with the highest
#'       in-degree as the sink and remove edges pointing away from it within
#'       the cycle. This preserves as much of the chain as possible.}
#'   }
#'
#' @return An updated `edge_list_df` with URLs in `edge_from_col` and
#'   `edge_to_col` replaced by their final resolved destinations.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C"),
#'   to = c("B", "C", "D"),
#'   stringsAsFactors = FALSE
#' )
#' redirects <- data.frame(
#'   from = c("B", "C", "E"),
#'   to = c("B_final", "C_final", "E_final"),
#'   stringsAsFactors = FALSE
#' )
#' resolve_redirects(edges, redirects)
#'
#' # Example with a redirect chain
#' edges_chain <- data.frame(from = "X", to = "Y", stringsAsFactors = FALSE)
#' redirects_chain <- data.frame(
#'   from = c("Y", "Z"),
#'   to = c("Z", "Z_final"),
#'   stringsAsFactors = FALSE
#' )
#' resolve_redirects(edges_chain, redirects_chain)
#'
#' # Example with conflicting redirects resolved via first_wins
#' edges_conflict <- data.frame(
#'   from = "A", to = "B", stringsAsFactors = FALSE
#' )
#' redirects_conflict <- data.frame(
#'   from = c("B", "B"),
#'   to = c("C", "D"),
#'   stringsAsFactors = FALSE
#' )
#' resolve_redirects(edges_conflict, redirects_conflict,
#'   duplicate_from_policy = "first_wins"
#' )
#'
#' # Example with different column names
#' edges_custom <- data.frame(
#'   source_url = "Page1", target_url = "Page2"
#' )
#' redirects_custom <- data.frame(
#'   original = "Page2", final = "Page2_resolved"
#' )
#' resolve_redirects(edges_custom, redirects_custom,
#'   edge_from_col = "source_url",
#'   edge_to_col = "target_url",
#'   redirect_from_col = "original",
#'   redirect_to_col = "final"
#' )
#' @details
#' Self-referencing redirects (where from == to) and any redirects with NA
#' in from or to are automatically filtered out before processing.
#'
#' When crawl data contains conflicting redirects (the same URL redirecting
#' to different targets), use \code{duplicate_from_policy} to control the
#' behavior. The default \code{"strict"} preserves backward compatibility
#' by erroring on any conflict.
#'
#' Redirect resolution uses a graph-based approach: an igraph is built from
#' the redirect rules, strongly connected components (SCCs) are used to
#' detect loops, and the \code{loop_handling} policy determines what happens
#' to cycles. After loop handling, each URL is mapped to its terminal
#' destination by traversing the acyclic graph.

resolve_redirects <- function(edge_list_df,
                              redirects_df,
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
                              )) {
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)

  # --- Input Validation ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0 &&
        !all(c(edge_from_col, edge_to_col) %in% names(edge_list_df))) {
    stop(paste0(
      "`edge_list_df` must have '", edge_from_col, "' and '",
      edge_to_col, "' columns if not empty."
    ), call. = FALSE)
  }

  if (!is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(redirects_df) > 0 &&
        !all(c(redirect_from_col, redirect_to_col) %in% names(redirects_df))) {
    stop("`redirects_df` must have '", redirect_from_col, "' and '",
      redirect_to_col, "' columns if not empty.",
      call. = FALSE
    )
  }

  # If redirects_df is empty or has no valid rules, return edge_list_df as is.
  if (nrow(redirects_df) == 0) {
    return(edge_list_df)
  }

  # Filter out NA values and self-referencing redirects (from == to).
  na_mask <- !is.na(redirects_df[[redirect_from_col]]) &
    !is.na(redirects_df[[redirect_to_col]])
  redirects_df <- redirects_df[na_mask, , drop = FALSE]

  if (nrow(redirects_df) > 0) {
    self_ref <- as.character(redirects_df[[redirect_from_col]]) ==
      as.character(redirects_df[[redirect_to_col]])
    redirects_df <- redirects_df[!self_ref, , drop = FALSE]
  }

  # --- Preprocess: handle conflicting redirects ---
  redirect_sources <- as.character(redirects_df[[redirect_from_col]])
  redirect_targets <- as.character(redirects_df[[redirect_to_col]])

  valid_redirect_indices <- !is.na(redirect_sources) & !is.na(redirect_targets)
  redirect_sources <- redirect_sources[valid_redirect_indices]
  redirect_targets <- redirect_targets[valid_redirect_indices]

  if (length(redirect_sources) == 0) {
    return(edge_list_df)
  }

  clean_df <- data.frame(
    from = redirect_sources, to = redirect_targets,
    stringsAsFactors = FALSE
  )
  clean_df <- .preprocess_redirects(clean_df, "from", "to",
    policy = duplicate_from_policy
  )

  if (nrow(clean_df) == 0) {
    return(edge_list_df)
  }

  # --- Build canonical redirect map via graph ---
  canonical_map <- .resolve_via_graph(clean_df$from, clean_df$to,
    loop_handling = loop_handling
  )

  # --- Apply map to edge list (vectorized) ---
  resolved_edge_list <- edge_list_df

  for (col_name in c(edge_from_col, edge_to_col)) {
    if (col_name %in% names(resolved_edge_list)) {
      original_urls <- as.character(resolved_edge_list[[col_name]])
      idx <- match(original_urls, names(canonical_map))
      resolved_urls <- ifelse(!is.na(idx), canonical_map[idx], original_urls)
      # Preserve original NAs
      resolved_urls[is.na(original_urls)] <- NA_character_
      resolved_edge_list[[col_name]] <- resolved_urls
    }
  }

  resolved_edge_list
}


# --- Internal: graph-based redirect resolution ---

#' Resolve redirects using igraph and SCC-based loop detection
#'
#' Builds a directed graph from redirect pairs, detects loops via strongly
#' connected components, applies the chosen loop_handling policy, then
#' traverses the resulting DAG to produce a canonical redirect map.
#'
#' @param from Character vector of redirect sources.
#' @param to Character vector of redirect targets.
#' @param loop_handling One of "error", "prune_loop", "break_arrow".
#' @return Named character vector mapping every reachable URL to its final
#'   resolved destination.
#' @noRd
.resolve_via_graph <- function(from, to, loop_handling = "error") {
  # Build redirect graph
  redirect_edges <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(redirect_edges, directed = TRUE)

  # --- Detect loops via SCCs ---
  scc <- igraph::components(g, mode = "strong")
  # SCCs of size > 1 are cycles; also check self-loops
  loop_sccs <- which(scc$csize > 1)
  self_loop_eids <- which(igraph::which_loop(g))
  has_self_loops <- length(self_loop_eids) > 0
  has_loops <- length(loop_sccs) > 0 || has_self_loops

  if (has_loops) {
    if (loop_handling == "error") {
      # Report the first cycle found
      if (length(loop_sccs) > 0) {
        first_scc_id <- loop_sccs[1]
        cycle_verts <- igraph::V(g)[scc$membership == first_scc_id]
        cycle_names <- igraph::V(g)$name[cycle_verts]
        # Build a readable cycle path
        cycle_path <- .format_cycle_path(g, cycle_names)
        stop("Redirect cycle detected: ", cycle_path, call. = FALSE)
      } else {
        # Self-loop only
        sl_ends <- igraph::ends(g, self_loop_eids[1])
        sl_name <- sl_ends[1, 1]
        stop("Redirect cycle detected: ", sl_name, " -> ", sl_name,
          call. = FALSE
        )
      }
    }

    # Remove self-loops first (they should have been filtered, but belt &
    # suspenders)
    if (has_self_loops) {
      g <- igraph::delete_edges(g, self_loop_eids)
    }

    if (loop_handling == "prune_loop") {
      g <- .prune_loop_edges(g, scc, loop_sccs)
    } else if (loop_handling == "break_arrow") {
      g <- .break_arrow_loops(g, scc, loop_sccs)
    }
  }

  # --- Build canonical map by traversing the DAG ---
  .build_canonical_map(g)
}


#' Format a readable cycle path from SCC vertices
#' @noRd
.format_cycle_path <- function(g, cycle_names) {
  # Walk from the first cycle vertex following edges within the SCC
  # to produce a readable A -> B -> C -> A path
  visited <- character(0)
  current <- cycle_names[1]
  path <- current

  for (i in seq_along(cycle_names)) {
    neighbors <- igraph::neighbors(g, current, mode = "out")
    next_in_cycle <- intersect(igraph::V(g)$name[neighbors], cycle_names)
    # Pick a neighbor we haven't visited yet, or the first one to close cycle
    unvisited <- setdiff(next_in_cycle, path)
    if (length(unvisited) > 0) {
      current <- unvisited[1]
      path <- c(path, current)
    } else if (length(next_in_cycle) > 0) {
      # Close the cycle
      path <- c(path, next_in_cycle[1])
      break
    } else {
      break
    }
  }

  paste(path, collapse = " -> ")
}


#' Remove all edges within SCC loops
#' @noRd
.prune_loop_edges <- function(g, scc, loop_sccs) {
  for (scc_id in loop_sccs) {
    loop_verts <- igraph::V(g)[scc$membership == scc_id]
    loop_names <- igraph::V(g)$name[loop_verts]
    # Remove all edges where both endpoints are in this SCC
    edges_to_remove <- igraph::E(g)[.inc(loop_verts)] # nolint
    # Filter to only edges fully within the SCC (not edges leaving/entering)
    el <- igraph::ends(g, edges_to_remove)
    within_scc <- el[, 1] %in% loop_names & el[, 2] %in% loop_names
    g <- igraph::delete_edges(g, edges_to_remove[within_scc])
  }
  g
}


#' Break loops by keeping the highest in-degree node as a sink
#' @noRd
.break_arrow_loops <- function(g, scc, loop_sccs) {
  for (scc_id in loop_sccs) {
    loop_verts <- igraph::V(g)[scc$membership == scc_id]
    loop_names <- igraph::V(g)$name[loop_verts]

    # Find node with highest in-degree within the cycle
    in_deg <- igraph::degree(g, v = loop_verts, mode = "in")
    sink_idx <- which.max(in_deg)
    sink_name <- loop_names[sink_idx]

    # Remove outgoing edges FROM the sink that stay within the SCC
    out_edges <- igraph::E(g)[.from(igraph::V(g)[sink_name])] # nolint
    el <- igraph::ends(g, out_edges)
    within_scc <- el[, 2] %in% loop_names
    g <- igraph::delete_edges(g, out_edges[within_scc])
  }
  g
}


#' Build a canonical URL map by traversing the redirect graph
#'
#' For every vertex, follow outgoing edges until a terminal vertex (one with
#' no outgoing edges in the redirect graph, i.e. a final destination) is
#' reached. Returns a named vector: source -> final_destination.
#' @noRd
.build_canonical_map <- function(g) {
  vnames <- igraph::V(g)$name
  n <- length(vnames)

  if (n == 0) {
    return(stats::setNames(character(0), character(0)))
  }

  # Pre-compute adjacency for speed: for each vertex, its single out-neighbor
  # (redirect graphs should have out-degree <= 1 per vertex after dedup)
  out_list <- igraph::as_adj_list(g, mode = "out")

  # Map vertex names to indices for fast lookup
  name_to_idx <- stats::setNames(seq_len(n), vnames)

  resolved <- rep(NA_character_, n)

  # Iterative traversal with memoisation

  for (i in seq_len(n)) {
    if (!is.na(resolved[i])) next

    # Walk the chain, collecting indices
    chain <- integer(0)
    current <- i
    while (TRUE) {
      if (!is.na(resolved[current])) {
        # Already resolved: apply to whole chain
        final <- resolved[current]
        for (ci in chain) resolved[ci] <- final
        break
      }

      chain <- c(chain, current)
      out_neighbors <- out_list[[current]]
      if (length(out_neighbors) == 0) {
        # Terminal node
        final <- vnames[current]
        for (ci in chain) resolved[ci] <- final
        break
      }

      # Follow the first (and ideally only) outgoing edge
      next_idx <- as.integer(out_neighbors[1])
      current <- next_idx
    }
  }

  # Return map: only include entries where a redirect actually changes the URL
  # (source vertices from the original redirect data)
  canonical <- stats::setNames(resolved, vnames)
  # Keep all entries -- the caller filters by match()
  canonical
}


# --- Internal: preprocess conflicting redirects ---

#' Preprocess redirects to handle conflicting sources
#'
#' Given a two-column data frame of (from, to) redirect pairs (already cleaned
#' of NAs and self-refs), detect conflicting sources and apply the chosen
#' policy.
#'
#' @param redirects_df Data frame with columns named by `from_col` and `to_col`.
#' @param from_col Character, name of the source column.
#' @param to_col Character, name of the target column.
#' @param policy One of "strict", "first_wins", "last_wins", "most_frequent",
#'   "prune_source", "resolve_if_consistent".
#' @return A deduplicated data frame where each source maps to exactly one
#'   target.
#' @noRd
.preprocess_redirects <- function(redirects_df, from_col, to_col,
                                  policy = "strict") {
  sources <- redirects_df[[from_col]]
  targets <- redirects_df[[to_col]]

  # --- Detect conflicting sources ---
  # A source is "conflicting" if it has >1 distinct target
  src_target_pairs <- paste0(sources, "\t", targets)
  unique_pairs_df <- redirects_df[!duplicated(src_target_pairs), , drop = FALSE]
  dup_sources <- unique_pairs_df[[from_col]][
    duplicated(unique_pairs_df[[from_col]])
  ]
  conflicting_sources <- unique(dup_sources)

  # No conflicts: just deduplicate exact duplicates and return

  if (length(conflicting_sources) == 0) {
    return(unique_pairs_df)
  }

  # --- Apply policy ---
  if (policy == "strict") {
    first_conflict <- conflicting_sources[1]
    conflict_targets <- unique(targets[sources == first_conflict])
    stop("Ambiguous redirect: URL '", first_conflict,
      "' maps to multiple distinct targets: ",
      paste(conflict_targets, collapse = ", "),
      call. = FALSE
    )
  }

  if (policy == "resolve_if_consistent") {
    first_conflict <- conflicting_sources[1]
    conflict_targets <- unique(targets[sources == first_conflict])
    stop("Ambiguous redirect: URL '", first_conflict,
      "' maps to multiple distinct targets: ",
      paste(conflict_targets, collapse = ", "),
      call. = FALSE
    )
  }

  if (policy == "first_wins") {
    return(redirects_df[!duplicated(sources), , drop = FALSE])
  }

  if (policy == "last_wins") {
    return(redirects_df[!duplicated(sources, fromLast = TRUE), , drop = FALSE])
  }

  if (policy == "prune_source") {
    keep <- !(sources %in% conflicting_sources)
    # Also deduplicate the remaining non-conflicting redirects
    result <- redirects_df[keep, , drop = FALSE]
    if (nrow(result) > 0) {
      dedup_key <- paste0(result[[from_col]], "\t", result[[to_col]])
      result <- result[!duplicated(dedup_key), , drop = FALSE]
    }
    return(result)
  }

  if (policy == "most_frequent") {
    # For non-conflicting sources: just deduplicate
    is_conflict <- sources %in% conflicting_sources
    non_conflict <- redirects_df[!is_conflict, , drop = FALSE]
    if (nrow(non_conflict) > 0) {
      nc_key <- paste0(non_conflict[[from_col]], "\t", non_conflict[[to_col]])
      non_conflict <- non_conflict[!duplicated(nc_key), , drop = FALSE]
    }

    # For conflicting sources: find the mode target per source
    conflict_df <- redirects_df[is_conflict, , drop = FALSE]
    resolved_rows <- lapply(conflicting_sources, function(src) {
      idx <- conflict_df[[from_col]] == src
      src_targets <- conflict_df[[to_col]][idx]
      freq <- table(src_targets)
      max_freq <- max(freq)
      candidates <- names(freq)[freq == max_freq]
      # Tie-break: first occurrence in original data
      winner <- candidates[1]
      for (cand in candidates) {
        if (match(cand, src_targets) < match(winner, src_targets)) {
          winner <- cand
        }
      }
      data.frame(from = src, to = winner, stringsAsFactors = FALSE)
    })
    resolved <- do.call(rbind, resolved_rows)
    names(resolved) <- c(from_col, to_col)
    return(rbind(non_conflict, resolved))
  }

  # Should not reach here due to match.arg in caller, but as safeguard
  stop("Unknown duplicate_from_policy: ", policy, call. = FALSE) # nocov
}
