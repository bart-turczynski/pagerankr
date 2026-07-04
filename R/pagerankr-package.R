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
#' @importFrom igraph arpack_defaults
#' @importFrom igraph as_adj_list
#' @importFrom igraph as_edgelist
#' @importFrom igraph authority_score
#' @importFrom igraph components
#' @importFrom igraph degree
#' @importFrom igraph delete_edges
#' @importFrom igraph E
#' @importFrom igraph ecount
#' @importFrom igraph ends
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph hits_scores
#' @importFrom igraph hub_score
#' @importFrom igraph make_empty_graph
#' @importFrom igraph neighbors
#' @importFrom igraph page_rank
#' @importFrom igraph strength
#' @importFrom igraph V
#' @importFrom igraph vcount
#' @importFrom igraph which_loop
#' @importFrom igraph write_graph
"_PACKAGE"

# `.from` and `.inc` are igraph's edge-selector NSE helpers, valid only inside
# `igraph::E(g)[...]` (e.g. `E(g)[.inc(v)]`, `E(g)[.from(v)]`). They are not
# ordinary bindings, so R CMD check's static analysis flags them as undefined
# globals; declare them here to silence that false positive.
utils::globalVariables(c(".from", ".inc"))
