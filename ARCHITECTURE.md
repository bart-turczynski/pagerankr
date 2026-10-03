# pagerankr architecture

pagerankr turns crawl data (edge lists, redirect reports, Screaming Frog
exports, GA4 transitions) into a link graph and scores it with PageRank and
related centrality measures. This file is the fixed entry point every package
in the fleet carries; the reference index on the
[docs site](https://bart-turczynski.gitlab.io/pagerankr/reference/) groups
the exported functions by the same layers.

## Layers

1. **Crawl import.** `R/screaming_frog_*.R` read Screaming Frog exports and
   check them against a column contract; `R/ga4_*.R` read behavioral
   transitions. Only `Hyperlink` rows become graph edges
   (`sf_graph_eligible()`).
2. **URL folding.** `R/clean_url_columns.R`, `R/canonicalization.R` and
   `R/fold_map.R` key every node through rurl. The canonicalization profile is
   pinned in `canonical_profile()`; changing it re-keys the graph.
3. **Graph preparation.** Redirect and canonical resolution
   (`R/resolve_*.R`, `R/audit_*.R`), edge deduplication and weighting
   (`R/get_unique_edges.R`, `R/aggregate_edges.R`, `R/transform_*.R`,
   `R/placement.R`, `R/position.R`, `R/boilerplate.R`) and isolate handling
   (`R/drop_isolates.R`). `page_state` (crawl-derived page condition) and
   `node_status` (graph role) stay separate axes.
4. **Scoring.** `R/compute_pagerank.R` wraps `igraph::page_rank()`;
   `R/pagerank.R` is the end-to-end wrapper. Variants live beside it: HITS,
   SALSA, TrustRank, topic-sensitive and reverse (feeder) PageRank, seed
   priors and convergence controls.
5. **Comparison and simulation.** `R/pagerank_grid.R`, `R/auto_grid.R`,
   `R/compare_pagerank.R`, `R/simulate_changes*.R` and the stability and
   sensitivity reports. Cheap variants become `pagerank_grid()` columns, not
   re-runs.
6. **Export and exploration.** `R/export_graph.R` and the optional Shiny
   explorer in `inst/shiny/` (`launch_explorer()`).

## Invariants

- Defaults are faithful: the untransformed choice shows the site as crawled.
  Log priors, folds and weighting are documented opt-ins.
- Base R does the data manipulation; igraph does the graph algorithms; rurl
  does URL identity. No other hard dependency.

## Where else to look

- Contributing, the verify gate and CI: [`CONTRIBUTING.md`](CONTRIBUTING.md).
- Measurements and modeling notes: `notes/` and the vignettes.
- Release history: [`NEWS.md`](NEWS.md).
