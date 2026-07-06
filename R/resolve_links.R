#' @title Resolve Links Through Redirects
#' @description Applies redirect rules to an edge list and returns the resolved
#'   link graph without computing PageRank. Useful for inspecting what the
#'   link graph looks like after redirects are applied, deduplication, and
#'   optional URL cleaning.
#'
#' @param edge_list_df A data frame representing the edge list with at least
#'   two columns for source and target URLs.
#' @param redirects_df A data frame containing redirect rules with 'from' and
#'   'to' columns (or as specified by `redirect_from_col`/`redirect_to_col`).
#'   If `NULL`, no redirects are applied.
#' @param clean_urls Logical, whether to clean/normalize URLs using
#'   \code{rurl::clean_url} before resolving. Default `TRUE`.
#' @param self_loops Character, how to handle self-loops created after
#'   redirect resolution. One of `"drop"` (default) or `"keep"`.
#' @param edge_from_col,edge_to_col Names of the from/to columns in
#'   `edge_list_df`. Default `"from"` and `"to"`.
#' @param redirect_from_col,redirect_to_col Names of the from/to columns in
#'   `redirects_df`. Default `"from"` and `"to"`.
#' @param duplicate_from_policy How to handle conflicting redirects. Passed
#'   through to [resolve_redirects()]. Default `"strict"`.
#' @param loop_handling How to handle redirect cycles. Passed through to
#'   [resolve_redirects()]. Default `"error"`.
#' @param rurl_params Named list of additional arguments passed to
#'   \code{rurl::clean_url} when `clean_urls = TRUE`.
#'
#' @return A data frame with the same columns as `edge_list_df`, but with
#'   URLs replaced by their final redirect destinations, duplicate edges
#'   removed, and self-loops handled according to `self_loops`.
#'
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "A"),
#'   to = c("B", "C", "D", "B")
#' )
#' redirects <- data.frame(
#'   from = c("B", "C"),
#'   to = c("B_final", "C_final")
#' )
#' resolve_links(edges, redirects, clean_urls = FALSE)
#'
#' # Without redirects: just deduplicate and clean
#' resolve_links(edges, clean_urls = FALSE)
#'
#' # Inspect the graph before and after a redirect change
#' before <- resolve_links(edges, clean_urls = FALSE)
#' new_redirects <- data.frame(
#'   from = "D", to = "B_final"
#' )
#' after <- resolve_links(edges, new_redirects, clean_urls = FALSE)
resolve_links <- function(edge_list_df,
                          redirects_df = NULL,
                          clean_urls = TRUE,
                          self_loops = c("drop", "keep"),
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
                          rurl_params = list()) {
  self_loops <- match.arg(self_loops)
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)

  # --- Input Validation ---
  .validate_resolve_links_inputs(
    edge_list_df = edge_list_df,
    redirects_df = redirects_df,
    clean_urls = clean_urls,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col
  )

  current_edges <- edge_list_df
  current_redirects <- redirects_df

  # --- 1. Optional URL Cleaning ---
  if (clean_urls) {
    edge_url_cols <- c(edge_from_col, edge_to_col)
    current_edges <- do.call(
      clean_url_columns,
      c(list(data_frame = current_edges, columns = edge_url_cols), rurl_params)
    )
    if (!is.null(current_redirects) && nrow(current_redirects) > 0) {
      redirect_url_cols <- c(redirect_from_col, redirect_to_col)
      current_redirects <- do.call(
        clean_url_columns,
        c(
          list(data_frame = current_redirects, columns = redirect_url_cols),
          rurl_params
        )
      )
    }
  }

  # --- 2. Redirect Resolution ---
  if (!is.null(current_redirects) && nrow(current_redirects) > 0) {
    current_edges <- resolve_redirects(
      edge_list_df = current_edges,
      redirects_df = current_redirects,
      edge_from_col = edge_from_col,
      edge_to_col = edge_to_col,
      redirect_from_col = redirect_from_col,
      redirect_to_col = redirect_to_col,
      duplicate_from_policy = duplicate_from_policy,
      loop_handling = loop_handling
    )
  }

  # --- 3. Deduplicate and handle self-loops ---
  current_edges <- get_unique_edges(
    edge_list_df = current_edges,
    self_loops = self_loops,
    from_col = edge_from_col,
    to_col = edge_to_col
  )

  current_edges
}

#' Validate inputs for resolve_links
#'
#' Preserves the original validation order and error-message text.
#'
#' @noRd
.validate_resolve_links_inputs <- function(edge_list_df,
                                           redirects_df,
                                           clean_urls,
                                           edge_from_col,
                                           edge_to_col) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!all(c(edge_from_col, edge_to_col) %in% names(edge_list_df))) {
    stop("`edge_list_df` must have '", edge_from_col, "' and '",
      edge_to_col, "' columns.",
      call. = FALSE
    )
  }
  if (!is.null(redirects_df) && !is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.logical(clean_urls) || length(clean_urls) != 1) {
    stop("`clean_urls` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(NULL)
}
