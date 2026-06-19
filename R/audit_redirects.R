#' @title Audit Redirect Rules
#' @description Analyses a redirect data frame and returns a diagnostic report
#'   covering chain lengths, loops, conflicting sources, self-referencing
#'   redirects, and terminal destinations. Useful as a pre-flight check before
#'   running \code{\link{resolve_redirects}} or \code{\link{pagerank}}.
#'
#' @param redirects_df A data frame containing redirect rules.
#' @param edge_list_df Optional data frame of edges. If provided, orphaned
#'   redirects (rules whose source URL does not appear in the edge list) are
#'   identified.
#' @param redirect_from_col Character, name of the source column in
#'   \code{redirects_df}. Default \code{"from"}.
#' @param redirect_to_col Character, name of the target column in
#'   \code{redirects_df}. Default \code{"to"}.
#' @param edge_from_col Character, name of the source column in
#'   \code{edge_list_df}. Default \code{"from"}.
#' @param edge_to_col Character, name of the target column in
#'   \code{edge_list_df}. Default \code{"to"}.
#'
#' @return A list with class \code{"redirect_audit"} containing:
#'   \describe{
#'     \item{n_rules}{Total number of redirect rules (after NA removal).}
#'     \item{n_self_refs}{Number of self-referencing redirects (from == to).}
#'     \item{self_refs}{Data frame of self-referencing redirects.}
#'     \item{n_conflicts}{Number of source URLs with conflicting targets.}
#'     \item{conflicts}{Data frame listing each conflicting source and its
#'       distinct targets.}
#'     \item{n_loops}{Number of redirect loops detected.}
#'     \item{loops}{List of character vectors, each describing a cycle path.}
#'     \item{chains}{Data frame with columns \code{from}, \code{to_final}, and
#'       \code{chain_length} showing the terminal destination and hop count
#'       for every source URL.}
#'     \item{max_chain_length}{Maximum chain length found.}
#'     \item{orphaned_redirects}{Data frame of redirect sources not found in
#'       the edge list (only when \code{edge_list_df} is provided).}
#'   }
#'
#' @export
#' @examples
#' redirects <- data.frame(
#'   from = c("A", "B", "C", "D", "D", "E"),
#'   to = c("B", "C", "final", "X", "Y", "E")
#' )
#' audit <- audit_redirects(redirects)
#' print(audit)
#'
#' # With an edge list to detect orphaned redirects
#' edges <- data.frame(from = "Z", to = "A")
#' audit2 <- audit_redirects(redirects, edge_list_df = edges)
#' audit2$orphaned_redirects
audit_redirects <- function(redirects_df,
                            edge_list_df = NULL,
                            redirect_from_col = "from",
                            redirect_to_col = "to",
                            edge_from_col = "from",
                            edge_to_col = "to") {
  result <- .audit_signal(
    signal_df = redirects_df,
    edge_list_df = edge_list_df,
    from_col = redirect_from_col,
    to_col = redirect_to_col,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    what = "redirects_df"
  )
  class(result) <- "redirect_audit"
  result
}


# --- Internal: neutral, signal-agnostic audit core ---

#' Audit a single URL fold signal (redirects or canonicals)
#'
#' Signal-agnostic engine shared by [audit_redirects()] and
#' [audit_canonicals()]: computes rule counts, self-references, conflicting
#' sources, cycles, chain
#' lengths, and (optionally) orphaned rules against an edge list. Knows nothing
#' about whether the pairs are 3xx redirects or declared canonicals; callers
#' attach the appropriate S3 class.
#'
#' @param signal_df Data frame of (from, to) pairs for one signal.
#' @param edge_list_df Optional edge list for orphan detection.
#' @param from_col,to_col Column names in `signal_df`.
#' @param edge_from_col,edge_to_col Column names in `edge_list_df`.
#' @param what Label used in validation error messages (e.g. `"redirects_df"`).
#' @return An unclassed list with the audit fields (see [audit_redirects()]).
#' @noRd
.audit_signal <- function(signal_df,
                          edge_list_df = NULL,
                          from_col = "from",
                          to_col = "to",
                          edge_from_col = "from",
                          edge_to_col = "to",
                          what = "redirects_df") {
  redirects_df <- signal_df
  redirect_from_col <- from_col
  redirect_to_col <- to_col

  # --- Validation ---
  if (!is.data.frame(redirects_df)) {
    stop("`", what, "` must be a data frame.", call. = FALSE)
  }
  if (nrow(redirects_df) > 0 &&
        !all(c(redirect_from_col, redirect_to_col) %in% names(redirects_df))) {
    stop("`", what, "` must have '", redirect_from_col, "' and '",
      redirect_to_col, "' columns.",
      call. = FALSE
    )
  }
  if (!is.null(edge_list_df) && !is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame or NULL.", call. = FALSE)
  }

  result <- list()

  # --- Clean: remove NAs ---
  sources <- as.character(redirects_df[[redirect_from_col]])
  targets <- as.character(redirects_df[[redirect_to_col]])
  valid <- !is.na(sources) & !is.na(targets)
  sources <- sources[valid]
  targets <- targets[valid]
  result$n_rules <- length(sources)

  if (result$n_rules == 0) {
    result$n_self_refs <- 0L
    result$self_refs <- data.frame(
      from = character(0), to = character(0)
    )
    result$n_conflicts <- 0L
    result$conflicts <- data.frame(
      source = character(0),
      targets = character(0),
      n_targets = integer(0)
    )
    result$n_loops <- 0L
    result$loops <- list()
    result$chains <- data.frame(
      from = character(0), to_final = character(0),
      chain_length = integer(0)
    )
    result$max_chain_length <- 0L
    result$orphaned_redirects <- data.frame(
      from = character(0),
      to = character(0)
    )
    return(result)
  }

  # --- Self-referencing redirects ---
  self_ref_mask <- sources == targets
  result$n_self_refs <- sum(self_ref_mask)
  result$self_refs <- data.frame(
    from = sources[self_ref_mask], to = targets[self_ref_mask]
  )

  # Work with non-self-ref redirects for remaining analysis
  clean_sources <- sources[!self_ref_mask]
  clean_targets <- targets[!self_ref_mask]

  # --- Conflicting sources ---
  if (length(clean_sources) > 0) {
    pair_key <- paste0(clean_sources, "\t", clean_targets)
    unique_mask <- !duplicated(pair_key)
    u_sources <- clean_sources[unique_mask]
    u_targets <- clean_targets[unique_mask]
    dup_srcs <- unique(u_sources[duplicated(u_sources)])
    result$n_conflicts <- length(dup_srcs)

    if (length(dup_srcs) > 0) {
      conflict_rows <- lapply(dup_srcs, function(src) {
        tgts <- unique(u_targets[u_sources == src])
        data.frame(
          source = src,
          targets = paste(tgts, collapse = ", "),
          n_targets = length(tgts)
        )
      })
      result$conflicts <- do.call(rbind, conflict_rows)
    } else {
      result$conflicts <- data.frame(
        source = character(0),
        targets = character(0),
        n_targets = integer(0)
      )
    }
  } else {
    result$n_conflicts <- 0L
    result$conflicts <- data.frame(
      source = character(0),
      targets = character(0),
      n_targets = integer(0)
    )
  }

  # --- Loop detection and chain analysis via graph ---
  if (length(clean_sources) > 0) {
    # Deduplicate for graph building (take first occurrence per source)
    dedup_mask <- !duplicated(clean_sources)
    g_sources <- clean_sources[dedup_mask]
    g_targets <- clean_targets[dedup_mask]

    g <- igraph::graph_from_data_frame(
      data.frame(from = g_sources, to = g_targets),
      directed = TRUE
    )

    # Loops via SCCs
    scc <- igraph::components(g, mode = "strong")
    loop_scc_ids <- which(scc$csize > 1)
    loop_paths <- list()
    for (scc_id in loop_scc_ids) {
      cycle_verts <- igraph::V(g)[scc$membership == scc_id]
      cycle_names <- igraph::V(g)$name[cycle_verts]
      path_str <- .format_cycle_path(g, cycle_names)
      loop_paths <- c(loop_paths, list(path_str))
    }
    result$n_loops <- length(loop_paths)
    result$loops <- loop_paths

    # Chain analysis: for each source, trace to terminal destination
    # Use a version of the graph with loops pruned so traversal terminates
    g_clean <- g
    for (scc_id in loop_scc_ids) {
      loop_verts <- igraph::V(g_clean)[scc$membership == scc_id]
      loop_names <- igraph::V(g_clean)$name[loop_verts]
      edges_to_rm <- igraph::E(g_clean)[.inc(loop_verts)]
      el <- igraph::ends(g_clean, edges_to_rm)
      within <- el[, 1] %in% loop_names & el[, 2] %in% loop_names
      if (any(within)) {
        g_clean <- igraph::delete_edges(g_clean, edges_to_rm[within])
      }
    }

    canonical <- .build_canonical_map(g_clean)
    vnames <- igraph::V(g_clean)$name

    # Compute chain lengths via traversal
    out_list <- igraph::as_adj_list(g_clean, mode = "out")
    chain_lengths <- rep(0L, length(vnames))
    names(chain_lengths) <- vnames

    for (i in seq_along(vnames)) {
      hops <- 0L
      current <- i
      visited <- integer(0)
      while (TRUE) {
        out_n <- out_list[[current]]
        if (length(out_n) == 0) break
        visited <- c(visited, current)
        current <- as.integer(out_n[1])
        if (current %in% visited) break # safety
        hops <- hops + 1L
      }
      chain_lengths[i] <- hops
    }

    # Only report source URLs (those in the original redirect from column)
    unique_sources <- unique(g_sources)
    chains_df <- data.frame(
      from = unique_sources,
      to_final = unname(canonical[unique_sources]),
      chain_length = unname(chain_lengths[unique_sources])
    )
    # Mark loop members
    loop_members <- character(0)
    for (scc_id in loop_scc_ids) {
      loop_members <- c(
        loop_members,
        igraph::V(g)$name[scc$membership == scc_id]
      )
    }
    chains_df$in_loop <- chains_df$from %in% loop_members

    result$chains <- chains_df
    result$max_chain_length <- max(chains_df$chain_length, 0L)
  } else {
    result$n_loops <- 0L
    result$loops <- list()
    result$chains <- data.frame(
      from = character(0), to_final = character(0),
      chain_length = integer(0),
      in_loop = logical(0)
    )
    result$max_chain_length <- 0L
  }

  # --- Orphaned redirects ---
  if (!is.null(edge_list_df) && nrow(edge_list_df) > 0) {
    edge_urls <- unique(c(
      as.character(edge_list_df[[edge_from_col]]),
      as.character(edge_list_df[[edge_to_col]])
    ))
    orphan_mask <- !(clean_sources %in% edge_urls)
    # Deduplicate orphans
    orphan_df <- data.frame(
      from = clean_sources[orphan_mask],
      to = clean_targets[orphan_mask]
    )
    orphan_key <- paste0(orphan_df$from, "\t", orphan_df$to)
    result$orphaned_redirects <- orphan_df[!duplicated(orphan_key), ,
      drop = FALSE
    ]
  } else {
    result$orphaned_redirects <- NULL
  }

  result
}


#' @export
print.redirect_audit <- function(x, ...) {
  cat("=== Redirect Audit Report ===\n\n")
  cat("Total rules (after NA removal):", x$n_rules, "\n")
  cat("Self-referencing redirects:     ", x$n_self_refs, "\n")
  cat("Conflicting sources:            ", x$n_conflicts, "\n")
  cat("Redirect loops:                 ", x$n_loops, "\n")
  cat("Max chain length:               ", x$max_chain_length, "\n")

  if (x$n_self_refs > 0) {
    cat("\n--- Self-referencing redirects ---\n")
    print(x$self_refs, row.names = FALSE)
  }

  if (x$n_conflicts > 0) {
    cat("\n--- Conflicting sources ---\n")
    print(x$conflicts, row.names = FALSE)
  }

  if (x$n_loops > 0) {
    cat("\n--- Loops ---\n")
    for (i in seq_along(x$loops)) {
      cat("  ", x$loops[[i]], "\n")
    }
  }

  if (!is.null(x$orphaned_redirects) && nrow(x$orphaned_redirects) > 0) {
    cat(
      "\nOrphaned redirects (not in edge list):",
      nrow(x$orphaned_redirects), "\n"
    )
  }

  if (nrow(x$chains) > 0) {
    long <- x$chains[x$chains$chain_length > 1, , drop = FALSE]
    if (nrow(long) > 0) {
      cat("\n--- Long chains (>1 hop) ---\n")
      print(long, row.names = FALSE)
    }
  }

  invisible(x)
}
