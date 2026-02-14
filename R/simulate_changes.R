#' @title Simulate the PageRank Impact of Link and Redirect Changes
#' @description Compares PageRank before and after proposed changes to the link
#'   graph. Useful for SEOs who want to predict the impact of adding or removing
#'   internal links, or implementing redirects, before making changes in
#'   production.
#'
#' @param edge_list_df A data frame representing the current link edge list.
#' @param add_links_df Optional data frame of links to add. Must have the same
#'   from/to column names as \code{edge_list_df}. Default \code{NULL}.
#' @param remove_links_df Optional data frame of links to remove. Matching is
#'   by exact from+to pair. Must have the same from/to column names as
#'   \code{edge_list_df}. Default \code{NULL}.
#' @param add_redirects_df Optional data frame of new redirects to add. Must
#'   have from/to columns (names controlled by \code{pagerank()}'s
#'   \code{redirect_from_col} / \code{redirect_to_col} defaults).
#'   Default \code{NULL}.
#' @param redirects_df Optional data frame of existing redirects (baseline).
#'   Default \code{NULL}.
#' @param ... Additional arguments passed to both \code{pagerank()} calls
#'   (e.g., \code{clean_edge_urls}, \code{damping}, \code{nofollow_col},
#'   \code{indexability_df}, etc.).
#' @param edge_from_col Name of the from column in edge list data frames.
#'   Default \code{"from"}.
#' @param edge_to_col Name of the to column in edge list data frames.
#'   Default \code{"to"}.
#' @param label_baseline Label for the baseline model in the comparison output.
#'   Default \code{"baseline"}.
#' @param label_proposed Label for the proposed model in the comparison output.
#'   Default \code{"proposed"}.
#'
#' @return The output of \code{\link{compare_pagerank}}: a data frame with
#'   per-node deltas, percentage changes, and rank changes between baseline and
#'   proposed. A \code{"summary"} attribute contains aggregate statistics
#'   (Spearman rho, mean absolute delta, nodes gained/lost).
#'
#' @export
#' @examples
#' # Current site links
#' edges <- data.frame(
#'   from = c("Home", "Home", "About", "Blog"),
#'   to = c("About", "Blog", "Home", "Home"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Propose adding a link from Blog to About
#' new_links <- data.frame(
#'   from = "Blog", to = "About", stringsAsFactors = FALSE
#' )
#' result <- simulate_changes(edges, add_links_df = new_links,
#'                            clean_edge_urls = FALSE)
#' print(result)
#' attr(result, "summary")
simulate_changes <- function(edge_list_df,
                             add_links_df = NULL,
                             remove_links_df = NULL,
                             add_redirects_df = NULL,
                             redirects_df = NULL,
                             ...,
                             edge_from_col = "from",
                             edge_to_col = "to",
                             label_baseline = "baseline",
                             label_proposed = "proposed") {

  # --- Validation ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  required_cols <- c(edge_from_col, edge_to_col)

  if (!is.null(add_links_df)) {
    if (!is.data.frame(add_links_df)) {
      stop("`add_links_df` must be a data frame or NULL.", call. = FALSE)
    }
    if (nrow(add_links_df) > 0 &&
        !all(required_cols %in% names(add_links_df))) {
      stop("`add_links_df` must have '", edge_from_col, "' and '",
           edge_to_col, "' columns.", call. = FALSE)
    }
  }

  if (!is.null(remove_links_df)) {
    if (!is.data.frame(remove_links_df)) {
      stop("`remove_links_df` must be a data frame or NULL.", call. = FALSE)
    }
    if (nrow(remove_links_df) > 0 &&
        !all(required_cols %in% names(remove_links_df))) {
      stop("`remove_links_df` must have '", edge_from_col, "' and '",
           edge_to_col, "' columns.", call. = FALSE)
    }
  }

  if (!is.null(add_redirects_df) && !is.data.frame(add_redirects_df)) {
    stop("`add_redirects_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.null(redirects_df) && !is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame or NULL.", call. = FALSE)
  }

  # --- Build proposed edge list ---
  proposed_edges <- edge_list_df

  if (!is.null(add_links_df) && nrow(add_links_df) > 0) {
    common_cols <- intersect(names(proposed_edges), names(add_links_df))
    proposed_edges <- rbind(proposed_edges[, common_cols, drop = FALSE],
                            add_links_df[, common_cols, drop = FALSE])
  }

  if (!is.null(remove_links_df) && nrow(remove_links_df) > 0) {
    remove_key <- paste0(as.character(remove_links_df[[edge_from_col]]), "\t",
                         as.character(remove_links_df[[edge_to_col]]))
    current_key <- paste0(as.character(proposed_edges[[edge_from_col]]), "\t",
                          as.character(proposed_edges[[edge_to_col]]))
    proposed_edges <- proposed_edges[!(current_key %in% remove_key), ,
                                    drop = FALSE]
  }

  # --- Build proposed redirects ---
  proposed_redirects <- redirects_df

  if (!is.null(add_redirects_df) && nrow(add_redirects_df) > 0) {
    if (is.null(proposed_redirects)) {
      proposed_redirects <- add_redirects_df
    } else {
      common_cols <- intersect(names(proposed_redirects),
                               names(add_redirects_df))
      proposed_redirects <- rbind(
        proposed_redirects[, common_cols, drop = FALSE],
        add_redirects_df[, common_cols, drop = FALSE]
      )
    }
  }

  # --- Run both models ---
  pr_baseline <- pagerank(edge_list_df, redirects_df = redirects_df, ...)
  pr_proposed <- pagerank(proposed_edges, redirects_df = proposed_redirects,
                          ...)

  # --- Compare ---
  compare_pagerank(pr_baseline, pr_proposed,
                   label_a = label_baseline, label_b = label_proposed)
}
