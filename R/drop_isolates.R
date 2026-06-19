#' @title Identify and Optionally Drop Isolated Nodes from an Edge List
#' @description From an edge list, identifies isolated nodes (nodes that do not
#'   participate in any complete edge, i.e., a row where both from and to are
#'   non-NA). It can return only connected nodes (degree > 0) or the full vertex
#'   universe (all unique non-NA URLs from both columns, including those from
#'   partial/incomplete rows).
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns "from" and "to" (or as specified by `from_col`, `to_col`).
#'   Rows where both columns are non-NA represent edges. Rows where one column
#'   is NA represent known nodes that do not participate in a complete edge
#'   (potential isolates).
#' @param drop Logical. If `TRUE`, returns a single-column data frame containing
#'   only node names that participate in at least one complete edge (both from
#'   and to are non-NA in the same row). If `FALSE` (default), returns a
#'   single-column data frame of all unique non-NA node names present in either
#'   column of `edge_list_df` (the full vertex universe, including isolates).
#' @param from_col Name of the source node column in `edge_list_df`.
#'   Default "from".
#' @param to_col Name of the target node column in `edge_list_df`. Default "to".
#' @param node_col_name Name for the output column containing node names.
#'   Default "node_name". When used with [compute_pagerank()], this should match
#'   its `vertex_col_name` parameter.
#'
#' @return A single-column data frame named according to `node_col_name`.
#'   If `drop = TRUE`, contains unique node names with degree > 0 (from complete
#'   edges only). If `drop = FALSE`, contains all unique non-NA node names from
#'   both columns of the input edge list (full vertex universe).
#'   Returns an empty data frame with the correct column name if no nodes meet
#'   the criteria or if the input edge list is empty/all NAs.
#' @export
#' @examples
#' # Edge list with partial rows
#' # (NA in one column = known node, not a complete edge)
#' edges <- data.frame(
#'   from = c("A", "B", "C", NA, "D"),
#'   to = c("B", "C", "A", "E", NA)
#' )
#' # Complete edges: A->B, B->C, C->A.
#' # Partial rows: NA->E (E is isolate), D->NA (D is isolate).
#'
#' # Get only nodes participating in complete edges (A, B, C)
#' active_nodes <- drop_isolates(edges, drop = TRUE)
#' print(active_nodes)
#'
#' # Get all unique nodes including isolates from partial rows (A, B, C, D, E)
#' all_nodes <- drop_isolates(edges, drop = FALSE)
#' print(all_nodes)
#'
#' # Edge list with no isolates (all rows are complete edges)
#' edges_complete <- data.frame(
#'   from = c("X", "Y"),
#'   to = c("Y", "X")
#' )
#' drop_isolates(edges_complete, drop = TRUE) # X, Y
#' drop_isolates(edges_complete, drop = FALSE) # X, Y (same, no partial rows)
#'
#' # Empty edge list
#' empty_edges <- data.frame(
#'   from = character(0), to = character(0)
#' )
#' drop_isolates(empty_edges, drop = TRUE)
#' drop_isolates(empty_edges, drop = FALSE)
#'
#' # Edge list with only NAs
#' na_edges <- data.frame(
#'   from = NA_character_, to = NA_character_
#' )
#' drop_isolates(na_edges, drop = TRUE)
#' drop_isolates(na_edges, drop = FALSE)
#'
#' # Custom column names
#' custom_edges <- data.frame(
#'   source = c("S1"), target = c("T1")
#' )
#' drop_isolates(
#'   custom_edges,
#'   from_col = "source", to_col = "target", node_col_name = "vertex"
#' )
drop_isolates <- function(edge_list_df,
                          drop = FALSE,
                          from_col = "from",
                          to_col = "to",
                          node_col_name = "node_name") {
  # --- Input Validation ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0 &&
        !all(c(from_col, to_col) %in% names(edge_list_df))) {
    stop(
      "`edge_list_df` must have specified 'from' and 'to' ",
      "columns if not empty.",
      call. = FALSE
    )
  }
  if (!is.logical(drop) || length(drop) != 1) {
    stop("`drop` must be a single logical value.", call. = FALSE)
  }
  if (!is.character(node_col_name) || length(node_col_name) != 1 ||
        nchar(node_col_name) == 0) {
    stop(
      "`node_col_name` must be a non-empty single character string.",
      call. = FALSE
    )
  }

  # --- Prepare Empty Result Frame ---
  # This structure is returned if no nodes are found or input is empty.
  empty_result_df <- stats::setNames(
    data.frame(character(0)),
    node_col_name
  )

  if (nrow(edge_list_df) == 0) {
    return(empty_result_df)
  }

  # --- Extract and Combine All Node Mentions ---
  # Ensure columns are character to handle factors correctly and remove NAs.
  from_nodes <- as.character(edge_list_df[[from_col]])
  to_nodes <- as.character(edge_list_df[[to_col]])

  all_mentioned_nodes <- unique(stats::na.omit(c(from_nodes, to_nodes)))

  if (length(all_mentioned_nodes) == 0) {
    # This happens if edge_list_df contained only NAs in from/to columns.
    return(empty_result_df)
  }

  # --- Determine Nodes to Return ---
  nodes_to_return <- character(0)

  if (drop) {
    # If drop = TRUE, return only nodes that participate in at least one
    # complete edge (both from and to are non-NA in the same row).
    complete_edge_mask <- !is.na(from_nodes) & !is.na(to_nodes)
    nodes_to_return <- unique(c(
      from_nodes[complete_edge_mask],
      to_nodes[complete_edge_mask]
    ))
  } else {
    # If drop = FALSE, return the full vertex universe: all unique non-NA
    # nodes from both columns, including those from partial/incomplete rows.
    nodes_to_return <- all_mentioned_nodes
  }

  if (length(nodes_to_return) == 0) {
    return(empty_result_df)
  }

  # --- Format Output Data Frame ---
  result_df <- stats::setNames(
    data.frame(nodes_to_return),
    node_col_name
  )

  # Sort for consistency, though not strictly required by spec
  # (can be helpful for tests)
  if (nrow(result_df) > 0) {
    result_df <- result_df[order(result_df[[node_col_name]]), , drop = FALSE]
    row.names(result_df) <- NULL
  }

  result_df
}
