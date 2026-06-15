#' @title Export PageRank Graph
#' @description Exports a PageRank result and its edge list as a graph file
#'   suitable for visualisation in external tools (Gephi, yEd, Graphviz, etc.).
#'   Supports GraphML, GEXF (via GraphML with attributes), DOT, and edge list
#'   CSV formats.
#'
#' @param pagerank_df A data frame with at least \code{url} and
#'   \code{pagerank} columns, as returned by \code{\link{pagerank}}.
#' @param edge_list_df A data frame of edges with from/to columns.
#' @param file Character, path to the output file.
#' @param format Character, output format. One of \code{"graphml"},
#'   \code{"dot"}, \code{"edgelist"}, or \code{"pajek"}.
#' @param edge_from_col,edge_to_col Names of from/to columns in
#'   \code{edge_list_df}. Default \code{"from"} and \code{"to"}.
#' @param pr_url_col Name of the URL column in \code{pagerank_df}. Default
#'   \code{"node_name"} (matching \code{\link{pagerank}} output).
#' @param pr_score_col Name of the PageRank score column. Default
#'   \code{"pagerank"}.
#' @param node_attrs Optional named list of additional vertex attribute columns
#'   from \code{pagerank_df} to include (e.g., \code{list(rank = "rank")}).
#' @param edge_attrs Optional character vector of additional columns from
#'   \code{edge_list_df} to include as edge attributes (e.g., \code{"weight"}).
#'
#' @return The file path (invisibly). Called for its side effect of writing a
#'   file.
#'
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C"),
#'   to = c("B", "C", "A"),
#'   stringsAsFactors = FALSE
#' )
#' pr <- pagerank(edges, clean_edge_urls = FALSE)
#'
#' # Export to GraphML (for Gephi)
#' tmp <- tempfile(fileext = ".graphml")
#' export_graph(pr, edges, file = tmp, format = "graphml")
#'
#' # Export as DOT (for Graphviz)
#' tmp_dot <- tempfile(fileext = ".dot")
#' export_graph(pr, edges, file = tmp_dot, format = "dot")
export_graph <- function(pagerank_df,
                         edge_list_df,
                         file,
                         format = c("graphml", "dot", "edgelist", "pajek"),
                         edge_from_col = "from",
                         edge_to_col = "to",
                         pr_url_col = "node_name",
                         pr_score_col = "pagerank",
                         node_attrs = NULL,
                         edge_attrs = NULL) {
  format <- match.arg(format)

  # --- Validation ---
  if (!is.data.frame(pagerank_df)) {
    stop("`pagerank_df` must be a data frame.", call. = FALSE)
  }
  if (!pr_url_col %in% names(pagerank_df)) {
    stop("`pagerank_df` must have a '", pr_url_col, "' column.", call. = FALSE)
  }
  if (!pr_score_col %in% names(pagerank_df)) {
    stop("`pagerank_df` must have a '", pr_score_col, "' column.",
      call. = FALSE
    )
  }
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!all(c(edge_from_col, edge_to_col) %in% names(edge_list_df))) {
    stop("`edge_list_df` must have '", edge_from_col, "' and '",
      edge_to_col, "' columns.",
      call. = FALSE
    )
  }
  if (!is.character(file) || length(file) != 1) {
    stop("`file` must be a single file path string.", call. = FALSE)
  }

  # --- Build igraph ---
  edges_for_graph <- data.frame(
    from = as.character(edge_list_df[[edge_from_col]]),
    to = as.character(edge_list_df[[edge_to_col]]),
    stringsAsFactors = FALSE
  )

  # Add edge attributes
  if (!is.null(edge_attrs)) {
    for (attr_name in edge_attrs) {
      if (attr_name %in% names(edge_list_df)) {
        edges_for_graph[[attr_name]] <- edge_list_df[[attr_name]]
      }
    }
  }

  # Build vertex data from pagerank_df
  pr_urls <- as.character(pagerank_df[[pr_url_col]])
  vertices <- data.frame(name = pr_urls, stringsAsFactors = FALSE)
  vertices$pagerank <- pagerank_df[[pr_score_col]]

  # Add optional node attributes
  if (!is.null(node_attrs) && is.list(node_attrs)) {
    for (attr_name in names(node_attrs)) {
      col_name <- node_attrs[[attr_name]]
      if (col_name %in% names(pagerank_df)) {
        vertices[[attr_name]] <- pagerank_df[[col_name]]
      }
    }
  }

  # Include any edge-list-only vertices not in pagerank_df
  edge_verts <- unique(c(edges_for_graph$from, edges_for_graph$to))
  missing_verts <- setdiff(edge_verts, vertices$name)
  if (length(missing_verts) > 0) {
    extra <- data.frame(name = missing_verts, stringsAsFactors = FALSE)
    extra$pagerank <- 0
    vertices <- rbind(vertices, extra)
  }

  g <- igraph::graph_from_data_frame(edges_for_graph,
    directed = TRUE,
    vertices = vertices
  )

  # --- Export ---
  if (format == "graphml") {
    igraph::write_graph(g, file, format = "graphml")
  } else if (format == "dot") {
    .write_dot(g, file)
  } else if (format == "edgelist") {
    .write_edgelist_csv(edges_for_graph, vertices, file)
  } else if (format == "pajek") {
    igraph::write_graph(g, file, format = "pajek")
  }

  invisible(file)
}


#' Write a DOT format file with attributes
#' @noRd
.write_dot <- function(g, file) {
  vnames <- igraph::V(g)$name
  pr_scores <- igraph::V(g)$pagerank
  max_pr <- max(pr_scores, na.rm = TRUE)
  if (max_pr == 0) max_pr <- 1

  lines <- character(0)
  lines <- c(lines, "digraph pagerank {")
  lines <- c(lines, "  rankdir=LR;")
  lines <- c(lines, "  node [shape=ellipse];")
  lines <- c(lines, "")

  # Nodes with size proportional to PR
  for (i in seq_along(vnames)) {
    label <- gsub('"', '\\"', vnames[i], fixed = TRUE)
    pr_val <- round(pr_scores[i], 6)
    # Scale node width: 0.5 to 2.0
    width <- round(0.5 + 1.5 * (pr_scores[i] / max_pr), 2)
    lines <- c(lines, sprintf(
      '  "%s" [label="%s\\nPR: %s" width=%s];',
      label, label, pr_val, width
    ))
  }

  lines <- c(lines, "")

  # Edges
  el <- igraph::as_edgelist(g)
  for (i in seq_len(nrow(el))) {
    from_label <- gsub('"', '\\"', el[i, 1], fixed = TRUE)
    to_label <- gsub('"', '\\"', el[i, 2], fixed = TRUE)
    lines <- c(lines, sprintf('  "%s" -> "%s";', from_label, to_label))
  }

  lines <- c(lines, "}")
  writeLines(lines, file)
}


#' Write edge list and node list as CSV
#' @noRd
.write_edgelist_csv <- function(edges_df, vertices_df, file) {
  # Write two files: file_edges.csv and file_nodes.csv
  base <- tools::file_path_sans_ext(file)
  ext <- tools::file_ext(file)
  if (ext == "") ext <- "csv"

  edges_file <- paste0(base, "_edges.", ext)
  nodes_file <- paste0(base, "_nodes.", ext)

  utils::write.csv(edges_df, edges_file, row.names = FALSE)
  utils::write.csv(vertices_df, nodes_file, row.names = FALSE)

  message("Wrote: ", edges_file, "\nWrote: ", nodes_file)
}
