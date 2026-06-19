#' pagerankr: A Modular Toolkit for Link-Graph Analysis and PageRank
#'
#' The pagerankr package provides pipeable functions for link-graph analysis
#' in SEO contexts, covering PageRank, HITS, SALSA, TrustRank, Topic-Sensitive
#' PageRank, and reverse-graph feeder PageRank. It includes Screaming Frog crawl
#' import adapters, GA4 behavioral transition modeling, convergence controls,
#' damping sensitivity sweeps, alpha-stability reporting, redirect and
#' rel=canonical resolution, URL folding, domain/host filtering, model
#' comparison, parameter grid search, and what-if simulation.
#'
#' @docType package
#' @name pagerankr-package
#' @aliases pagerankr
#' @keywords internal
#' @importFrom igraph arpack_defaults as_adj_list as_edgelist authority_score components degree
#' @importFrom igraph delete_edges E ecount ends graph_from_data_frame hits_scores hub_score
#' @importFrom igraph make_empty_graph neighbors page_rank strength V vcount which_loop write_graph
"_PACKAGE"
