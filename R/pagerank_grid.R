#' @title Run PageRank Across Multiple Parameter Sets
#' @description Executes [pagerank()] for each entry in a named parameter grid,
#'   returning a single combined data frame with a `model_id` column
#'   identifying which configuration produced each row.
#'
#' @param edge_list_df A data frame representing the edge list (passed to every
#'   [pagerank()] call).
#' @param params_grid A named list of named lists. Each inner list contains
#'   parameter overrides for [pagerank()]. The top-level names become the
#'   `model_id` values in the output.
#' @param redirects_df An optional redirect data frame (passed to every call).
#'   Default `NULL`.
#' @param ... Common parameters shared across all models (e.g.,
#'   `clean_edge_urls`, `rurl_params`, `damping`). These are passed to every
#'   [pagerank()] call and can be overridden by entries in `params_grid`.
#' @param edge_from_col,edge_to_col Names of from/to columns in
#'   `edge_list_df`. Default `"from"` / `"to"`.
#'
#' @return A data frame with columns `model_id`, `node_name`, and `pagerank`
#'   (or the column names returned by [pagerank()]). Rows from different
#'   models are stacked via [rbind()].
#'
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C", "A"),
#'   to = c("B", "C", "A", "C")
#' )
#' params <- list(
#'   baseline = list(damping = 0.85, self_loops = "drop"),
#'   high_damp = list(damping = 0.95, self_loops = "drop"),
#'   keep_loops = list(damping = 0.85, self_loops = "keep")
#' )
#' grid <- pagerank_grid(edges, params, clean_edge_urls = FALSE)
#' print(grid)
#'
#' # Compare two models from the grid
#' baseline <- grid[grid$model_id == "baseline", ]
#' high_damp <- grid[grid$model_id == "high_damp", ]
#' compare_pagerank(baseline, high_damp)
pagerank_grid <- function(edge_list_df,
                          params_grid,
                          redirects_df = NULL,
                          ...,
                          edge_from_col = "from",
                          edge_to_col = "to") {
  .validate_pagerank_grid(edge_list_df, params_grid)

  results_list <- .run_pagerank_grid(
    edge_list_df = edge_list_df,
    params_grid = params_grid,
    redirects_df = redirects_df,
    common_args = list(...),
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col
  )

  .combine_pagerank_grid(results_list)
}

#' Validate inputs to [pagerank_grid()]
#' @noRd
.validate_pagerank_grid <- function(edge_list_df, params_grid) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!is.list(params_grid)) {
    stop(
      "`params_grid` must be a non-empty named list of parameter lists.",
      call. = FALSE
    )
  }
  if (length(params_grid) == 0) {
    stop(
      "`params_grid` must be a non-empty named list of parameter lists.",
      call. = FALSE
    )
  }
  if (is.null(names(params_grid))) {
    stop(
      "All entries in `params_grid` must be named (these become model IDs).",
      call. = FALSE
    )
  }
  if (any(names(params_grid) == "")) {
    stop(
      "All entries in `params_grid` must be named (these become model IDs).",
      call. = FALSE
    )
  }
  for (nm in names(params_grid)) {
    if (!is.list(params_grid[[nm]])) {
      stop(
        "Each entry in `params_grid` must be a list. '", nm, "' is not.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Build the argument list for a single [pagerank()] call in the grid
#' @noRd
.build_grid_call_args <- function(model_params,
                                  edge_list_df,
                                  redirects_df,
                                  edge_from_col,
                                  edge_to_col,
                                  common_args) {
  call_args <- c(
    list(
      edge_list_df = edge_list_df,
      redirects_df = redirects_df,
      edge_from_col = edge_from_col,
      edge_to_col = edge_to_col
    ),
    common_args
  )
  # Override with model-specific params
  for (pname in names(model_params)) {
    call_args[[pname]] <- model_params[[pname]]
  }
  call_args
}

#' Run [pagerank()] for every parameter set in the grid
#' @noRd
.run_pagerank_grid <- function(edge_list_df,
                               params_grid,
                               redirects_df,
                               common_args,
                               edge_from_col,
                               edge_to_col) {
  results_list <- vector("list", length(params_grid))
  model_names <- names(params_grid)

  for (i in seq_along(params_grid)) {
    call_args <- .build_grid_call_args(
      model_params = params_grid[[i]],
      edge_list_df = edge_list_df,
      redirects_df = redirects_df,
      edge_from_col = edge_from_col,
      edge_to_col = edge_to_col,
      common_args = common_args
    )

    pr_result <- do.call(pagerank, call_args)

    if (nrow(pr_result) > 0) {
      pr_result$model_id <- model_names[i]
    } else {
      pr_result$model_id <- character(0)
    }
    results_list[[i]] <- pr_result
  }

  results_list
}

#' Combine per-model results into a single data frame with `model_id` first
#' @noRd
.combine_pagerank_grid <- function(results_list) {
  combined <- do.call(rbind, results_list)
  row.names(combined) <- NULL

  # Reorder columns: model_id first
  col_order <- c("model_id", setdiff(names(combined), "model_id"))
  combined[, col_order, drop = FALSE]
}
