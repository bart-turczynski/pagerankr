#' @title Generate Parameter Grid for pagerank_grid()
#' @description
#' Creates a named list of parameter lists suitable for passing to
#' [pagerank_grid()]. Each combination of the supplied parameter values
#' becomes one entry, with an auto-generated model ID describing the
#' configuration.
#'
#' This is the "exhaustive search" complement to manually specifying a
#' `params_grid` -- it generates all combinations of the parameter values
#' you provide.
#'
#' @param ... Named arguments where each value is a vector of options to
#'   sweep. Parameter names must match [pagerank()] arguments.
#'
#' @return A named list of named lists, ready to pass as `params_grid` to
#'   [pagerank_grid()]. Names are auto-generated from the parameter values
#'   (e.g., `"damping=0.85_self_loops=drop"`).
#'
#' @export
#' @examples
#' # Generate all combinations of damping and self-loop handling
#' grid <- auto_grid(damping = c(0.85, 0.95), self_loops = c("drop", "keep"))
#' str(grid)
#' # $`damping=0.85_self_loops=drop`
#' # $`damping=0.85_self_loops=keep`
#' # $`damping=0.95_self_loops=drop`
#' # $`damping=0.95_self_loops=keep`
#'
#' # Use with pagerank_grid()
#' edges <- data.frame(from = c("A", "B"), to = c("B", "A"),
#'                     stringsAsFactors = FALSE)
#' results <- pagerank_grid(edges, auto_grid(damping = c(0.5, 0.85, 0.95)),
#'                          clean_edge_urls = FALSE)
auto_grid <- function(...) {
  params <- list(...)
  if (length(params) == 0) {
    stop("At least one parameter must be provided.", call. = FALSE)
  }
  if (is.null(names(params)) || any(names(params) == "")) {
    stop("All arguments must be named (parameter names for pagerank()).", call. = FALSE)
  }

  # Create all combinations
  combinations <- expand.grid(params, stringsAsFactors = FALSE)

  # Build the named list of named lists
  result <- vector("list", nrow(combinations))
  for (i in seq_len(nrow(combinations))) {
    row <- as.list(combinations[i, , drop = FALSE])
    # Generate a descriptive model ID
    label_parts <- vapply(names(row), function(nm) {
      paste0(nm, "=", row[[nm]])
    }, character(1))
    model_id <- paste(label_parts, collapse = "_")
    result[[i]] <- row
    names(result)[i] <- model_id
  }

  result
}
