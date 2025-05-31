#' pagerankr: A Modular Toolkit for PageRank Calculation
#'
#' The pagerankr package provides a series of functions to process web crawl data
#' (edge lists and redirect reports) and calculate PageRank scores. It emphasizes
#' modularity, base R for data manipulation, and CRAN compliance.
#'
#' @keywords internal
"_PACKAGE"
# The following block imports specific functions from other packages.
# This is generally preferred over adding packages to Depends in DESCRIPTION.
# It makes dependencies explicit and avoids masking functions from base R or other packages.
#' @importFrom igraph page_rank graph_from_data_frame degree V E
#' @importFrom rurl clean_url
#' @importFrom utils globalVariables # Only if truly needed and justified
NULL
# If you use globalVariables, list unquoted variable names used in non-standard ways:
# utils::globalVariables(c("variable_name_in_df_manipulation"))