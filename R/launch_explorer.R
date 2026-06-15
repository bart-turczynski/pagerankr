#' @title Launch PageRank Explorer
#' @description Opens an interactive Shiny application for exploring PageRank
#'   results. Upload CSV files for edge lists, redirects, and PageRank scores,
#'   then visualise the graph interactively, inspect distributions, audit
#'   redirects, and export in multiple formats.
#'
#' @param ... Additional arguments passed to \code{shiny::runApp} (e.g.,
#'   \code{port}, \code{host}, \code{launch.browser}).
#'
#' @details
#' The app requires the \code{shiny} and \code{DT} packages. For interactive
#' network visualisation, \code{visNetwork} is recommended (the app falls back
#' to a static igraph plot if visNetwork is not installed).
#'
#' Install optional dependencies with:
#' \preformatted{install.packages(c("shiny", "DT", "visNetwork"))}
#'
#' @return Called for its side effect (launches the app). Returns invisibly.
#' @export
#' @examples
#' \dontrun{
#' launch_pagerank_explorer()
#' }
launch_pagerank_explorer <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required. Install with: ",
      "install.packages('shiny')",
      call. = FALSE
    )
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("The 'DT' package is required. Install with: ",
      "install.packages('DT')",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", "pagerank_explorer", package = "pagerankr")
  if (app_dir == "") {
    stop("Could not find the Shiny app. Try reinstalling pagerankr.",
      call. = FALSE
    )
  }

  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    message(
      "Tip: Install 'visNetwork' for interactive graph visualisation: ",
      "install.packages('visNetwork')"
    )
  }

  shiny::runApp(app_dir, ...)
}
