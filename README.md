pagerankr: SEO-Focused PageRank Modelling Toolkit
================

- [1 pagerankr
  <img src="man/figures/logo.png" align="right" height="139" alt="pagerankr hex sticker" />](#1-pagerankr-)
  - [1.1 Installation](#11-installation)
  - [1.2 Quick Start](#12-quick-start)
  - [1.3 Features](#13-features)
    - [1.3.1 Core Pipeline](#131-core-pipeline)
    - [1.3.2 SEO Modelling](#132-seo-modelling)
    - [1.3.3 Analysis & Comparison](#133-analysis--comparison)
    - [1.3.4 Simulation & Utilities](#134-simulation--utilities)
  - [1.4 Key Functions](#14-key-functions)
  - [1.5 Example: Comparing Models](#15-example-comparing-models)
  - [1.6 Example: Simulating a Link
    Change](#16-example-simulating-a-link-change)
  - [1.7 Further Information](#17-further-information)
  - [1.8 Code of Conduct](#18-code-of-conduct)
  - [1.9 License](#19-license)

# 1 pagerankr <img src="man/figures/logo.png" align="right" height="139" alt="pagerankr hex sticker" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bart-turczynski/pagerankr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bart-turczynski/pagerankr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`pagerankr` is a modular R toolkit for calculating, comparing, and
analysing PageRank scores from web crawl data. Built for SEO
professionals, it handles the full pipeline from raw crawl exports to
actionable insights: URL cleaning, redirect resolution,
nofollow/indexability modelling, weighted edges, domain filtering, model
comparison, and what-if simulation.

Data manipulation uses base R; `igraph` powers the PageRank algorithm
and graph-based redirect resolution; `rurl` handles URL
canonicalisation.

## 1.1 Installation

``` r
# install.packages("devtools")
devtools::install_github("bart-turczynski/pagerankr")
```

## 1.2 Quick Start

``` r
library(pagerankr)

edges <- data.frame(
  from = c("http://example.com/home", "http://example.com/about",
           "http://example.com/blog"),
  to   = c("http://example.com/about", "http://example.com/home",
           "http://example.com/home"),
  stringsAsFactors = FALSE
)

redirects <- data.frame(
  from = "http://example.com/old-blog",
  to   = "http://example.com/blog",
  stringsAsFactors = FALSE
)

pagerank(edges, redirects_df = redirects)
```

## 1.3 Features

### 1.3.1 Core Pipeline

- **URL Cleaning** – canonicalise URLs via `rurl::clean_url` with
  memoisation
- **Redirect Resolution** – graph-based chain resolution with SCC loop
  detection
- **Conflicting Redirects** – 6 policies: `strict`, `first_wins`,
  `last_wins`, `most_frequent`, `prune_source`, `resolve_if_consistent`
- **Redirect Loops** – 3 policies: `error`, `prune_loop`, `break_arrow`
- **Duplicate Edge Handling** – deduplication preserving extra columns
- **Self-Loop Control** – drop or keep self-referencing edges
- **Isolate Handling** – include or exclude disconnected nodes
- **Weighted Edges** – pass a `weight_col` to use link weights in
  PageRank

### 1.3.2 SEO Modelling

- **Nofollow Links** – `evaporate` (Google-like: PR splits across all
  links, nofollow share vanishes), `drop`, or `keep`
- **Indexability** – model `noindex` pages (outgoing links become
  nofollow) and `robots.txt`-blocked pages (outgoing links removed,
  inbound PR trapped or vanished)
- **Domain Filtering** – `filter_links_by_domain()` to scope edge lists
  to internal links, cross-domain links, or specific domains

### 1.3.3 Analysis & Comparison

- **Model Comparison** – `compare_pagerank()` diffs two PageRank results
  with rank shifts and Spearman correlation
- **Parameter Grid** – `pagerank_grid()` runs PageRank across parameter
  combinations; `auto_grid()` generates the grid
- **Grid Analysis** – `analyze_pagerank_grid()` computes distribution
  metrics across a grid
- **Distribution Metrics** – `pr_gini()`, `pr_entropy()`,
  `pr_top_k_share()`

### 1.3.4 Simulation & Utilities

- **What-If Simulation** – `simulate_changes()` models the impact of
  adding/removing links or redirects before production
- **Link Resolution** – `resolve_links()` applies redirects and
  deduplication without computing PageRank

## 1.4 Key Functions

| Function | Purpose |
|:---|:---|
| `pagerank()` | End-to-end pipeline: clean, resolve, compute |
| `resolve_links()` | Resolve redirects and deduplicate without PageRank |
| `simulate_changes()` | Compare baseline vs. proposed graph changes |
| `compare_pagerank()` | Diff two PageRank results |
| `pagerank_grid()` | Run PageRank across parameter combinations |
| `filter_links_by_domain()` | Scope edges by domain/host |
| `resolve_redirects()` | Apply redirect rules to an edge list |
| `clean_url_columns()` | Canonicalise URLs in a data frame |
| `get_unique_edges()` | Deduplicate edges, handle self-loops |
| `compute_pagerank()` | Low-level igraph PageRank wrapper |

## 1.5 Example: Comparing Models

``` r
# Run PageRank with different damping factors and nofollow handling
grid <- auto_grid(damping = c(0.85, 0.90), nofollow_action = c("evaporate", "drop"))
results <- pagerank_grid(edges, params_grid = grid, clean_edge_urls = FALSE)
analysis <- analyze_pagerank_grid(results)
print(analysis)
```

## 1.6 Example: Simulating a Link Change

``` r
# What happens if we add a link from Blog to About?
impact <- simulate_changes(
  edges,
  add_links_df = data.frame(from = "Blog", to = "About",
                            stringsAsFactors = FALSE),
  clean_edge_urls = FALSE
)
print(impact)
```

## 1.7 Further Information

``` r
help(package = "pagerankr")
vignette("pagerankr-usage")
```

## 1.8 Code of Conduct

Please note that the `pagerankr` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## 1.9 License

MIT License. See the `LICENSE` file for details.
