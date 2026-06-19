#' @title Analyze PageRank Grid Results
#' @description
#' Computes distribution metrics for each model in a [pagerank_grid()] result,
#' producing a one-row-per-model summary. Useful for quickly comparing how
#' different parameter configurations affect the shape of the PageRank
#' distribution.
#'
#' @param grid_result A data frame returned by [pagerank_grid()], with columns
#'   `model_id`, a node column, and a PageRank value column.
#' @param model_id_col Name of the model identifier column. Default
#'   `"model_id"`.
#' @param pr_col Name of the PageRank value column. Default `"pagerank"`.
#'
#' @return A data frame with one row per model and the following columns:
#'   \describe{
#'     \item{model_id}{Model identifier}
#'     \item{num_nodes}{Number of nodes in the model}
#'     \item{pr_sum}{Sum of PageRank scores (1 for standard graphs, less when
#'       evaporation or vanish is active)}
#'     \item{pr_max}{Maximum PageRank score}
#'     \item{pr_gini}{Gini coefficient (see [pr_gini()])}
#'     \item{pr_entropy}{Shannon entropy (see [pr_entropy()])}
#'     \item{pr_top10_share}{Share of total PR held by the top 10 percent of
#'       nodes (see [pr_top_k_share()])}
#'   }
#'
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "A"),
#'   to = c("B", "C", "A", "C")
#' )
#' params <- list(
#'   low = list(damping = 0.5),
#'   high = list(damping = 0.95)
#' )
#' grid <- pagerank_grid(edges, params, clean_edge_urls = FALSE)
#' analyze_pagerank_grid(grid)
analyze_pagerank_grid <- function(grid_result,
                                  model_id_col = "model_id",
                                  pr_col = "pagerank") {
  # --- Validation ---
  if (!is.data.frame(grid_result)) {
    stop("`grid_result` must be a data frame.", call. = FALSE)
  }
  if (!(model_id_col %in% names(grid_result))) {
    stop(
      "Column '", model_id_col, "' not found in `grid_result`.",
      call. = FALSE
    )
  }
  if (!(pr_col %in% names(grid_result))) {
    stop("Column '", pr_col, "' not found in `grid_result`.", call. = FALSE)
  }

  models <- unique(grid_result[[model_id_col]])

  rows <- lapply(models, function(mid) {
    pr_vals <- grid_result[[pr_col]][grid_result[[model_id_col]] == mid]
    data.frame(
      model_id = mid,
      num_nodes = length(pr_vals),
      pr_sum = sum(pr_vals, na.rm = TRUE),
      pr_max = if (length(pr_vals) > 0) {
        max(pr_vals, na.rm = TRUE)
      } else {
        NA_real_
      },
      pr_gini = pr_gini(pr_vals),
      pr_entropy = pr_entropy(pr_vals),
      pr_top10_share = pr_top_k_share(pr_vals, k = 0.1)
    )
  })

  result <- do.call(rbind, rows)
  names(result)[1] <- model_id_col
  row.names(result) <- NULL
  result
}
