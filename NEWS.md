# pagerankr (development version)

## Breaking changes

* pagerankr now requires R >= 4.1.0 (was R >= 4.0.0), as `pslr`, which it needs through `rurl`, does (`SEOR-wurguiqa`).

## New features

* New `cli()` and an installed `pagerankr` script score an edge-list CSV (`pagerankr score`) or compare two score files (`pagerankr compare`) without an interactive R session (`PAGE-nsdtszsj`).

## Bug fixes

* Case-insensitive matching of Screaming Frog headers, XPath regions, placement labels and hosts no longer depends on the locale (`PAGE-vzetnwuu`).
* Help pages render their Markdown formatting instead of showing it as literal text (`PAGE-iyfuhqyz`).

# pagerankr 0.1.1

* pagerankr is on CRAN: install it with `install.packages("pagerankr")` (`PAGE-gwgjmtmw`).
* The test suite passes against rurl 3.1.0, whose `path_normalisation` alias tripped a canonicalization guard; node keys are unchanged (`PAGE-lgsbjjuf`).
* `analyse_pagerank_grid()` and `sf_normalise_position()` are exported as British-spelling aliases of `analyze_pagerank_grid()` and `sf_normalize_position()` (`SEOR-qwomlgjd`).
* Help pages, and the message `compute_pagerank()` prints when switching to ARPACK, use US spellings (`SEOR-kfiqpymb`).
* The documentation site moves to <https://bart-turczynski.gitlab.io/pagerankr/>; the old Pages domain stops resolving (`SEOR-vujgdjfv`).

# pagerankr 0.1.0

* pagerankr is hosted on GitLab: install it with `devtools::install_gitlab("bart-turczynski/pagerankr")`; bug reports go to the GitLab tracker, security reports by email (see `SECURITY.md`).
* `rurl` is required at >= 3.0.1 and installs from CRAN; the `Remotes:` field is gone (`PAGE-tsbkxhoz`).
* **Breaking:** `canonical_profile()` pins `url_standard = "whatwg"` and `path_encoding = "keep"`, re-keying nodes: `%2F` no longer folds to `/`, and `/a%7Eb` and `/a~b` are distinct.
* `rurl_params` can no longer override `case_handling` or `path_normalization`, which are now node identity.
* **Breaking:** `canonical_profile()` keeps non-default ports and contentful query parameters as distinct nodes and folds IDN hosts to punycode; tracking parameters are still dropped.
* `canonical_profile()` pins `credential_handling = "strip"`, so a credentialed URL still yields a node key; no keys change.
* `clean_url_columns()` keeps tokens `rurl` cannot parse, such as bare labels, as opaque nodes instead of `NA`.
* `pagerank()` and `compute_pagerank()` reject a fractional `niter`, a non-finite `eps`, and a typed-missing or infinite `damping` with a named error.
* `pagerank()` and `compute_pagerank()` gain `algo`, `eps` and `niter`; results carry a `"convergence"` attribute with the solver, iterations and an L1 residual.
* **Breaking:** noindex, robots-blocked and 4xx/5xx pages collect PageRank but pass none, routing to a shared waste sink; any graph containing them scores differently (`PAGE-qzskzcfd`).
* `pagerank()` gains `prior_exclude_waste` (default `TRUE`), keeping that class out of the teleport vector; `FALSE` restores uniform teleport, matching `igraph::page_rank()`.
* `pagerank()` gains `status_df`, marking pages that returned 4xx or 5xx as response-dead; `pagerank_screaming_frog()` feeds it from the crawl.
* **Breaking:** `robots_blocked_action` takes `"show"` or `"vanish"`; the self-looping `"trap"` is gone.
* A noindex page always routes to the waste sink; `nofollow_action` now governs only `rel=nofollow` edges.
* With `indexability_df` or `status_df`, `pagerank()` returns `page_state` (`"live"`, `"noindex"`, `"robots_blocked"`, `"response_dead"`) and `wasted_mass`, each page's share of the sink.
* **Breaking:** `simulate_changes()` replaces `add_redirects_df` with `redirect_urls_df`, which retires a page behind a redirect: the target inherits its inbound authority, never its outlinks.
* `simulate_changes()` gains `remove_urls`, modeling URLs that return 404, and `on_unknown_target` for targets not yet in the graph.
* `simulate_changes()` output gains a `node_status` column and `proposed` and `manifest` attributes.
* New `simulate_changes_screaming_frog()` applies the same what-ifs to a `screaming_frog_bundle()`.
* `pagerank()` gains `placement_col`, `accepted_placements` and `placement_weights`, so any crawler that reports link regions can filter or weight edges by region.
* `pagerank()` gains `position_col`, `position_transform` and `position_floor` to weight links by reading order within a page; off unless `position_col` is supplied.
* `pagerank()` gains `container_col`, `boilerplate_threshold`, `min_container_pages` and `boilerplate_weight` to discount repetitive template links in the main content.
* Placement and boilerplate discounts do not multiply: the strongest applicable discount wins.
* The synthetic weight column is renamed `.__pr_edge_weight__`, as shown in the transition audit's `config$weight_col`.
* New `preset` argument on `pagerank()` and new `pr_preset()` bundle arguments into named views; an explicit argument wins over the preset, also through wrappers.
* Presets `"raw"`, `"declared"`, `"reversed"` and `"content"` ship; `"content"` weights links in site chrome at a tenth of in-content links.
* The transition audit records the preset in `config$preset`, and an error about placement arguments names the preset that set them.
* New vignettes: `vignette("presets")`, `vignette("boilerplate")`, `vignette("case-study")`, `vignette("trustrank")` and `vignette("topic_feeder_pagerank")`.
* A two-crawl Screaming Frog fixture of one site, before and after an internal-linking change, ships in `inst/extdata/`.
* **Breaking:** the six exported `.sf_*` helpers lose their leading dot (`sf_contract()`, `sf_read_input()` and others) and are now documented.
* **Breaking:** `resolve_urls()` is renamed `resolve_redirect_urls()`.
* **Breaking:** the placement `"sidebar"` is renamed `"aside"` in `sf_normalize_position()`, `accepted_placements` and `placement_weights`.
* New `sf_region_from_path()` derives Screaming Frog placement from the DOM path, so footer and header navigation no longer read as `nav`.
* New `sf_container_from_path()` derives a link's component from its `Link Path`; `screaming_frog_links()` adds `container` and `position_index` columns.
* `pagerank_screaming_frog()` gains `preset`, `apply_canonicals` and `apply_redirects`; `preset = "raw"` switches off the bundle's canonical, redirect and indexability tables.
* In the `screaming_frog_import` attribute, `scoring$scored_edge_rows` is renamed `scoring$edge_rows_to_pagerank` and `scoring$effective_weight_col` is dropped.
* `screaming_frog_bundle()` counts off-domain canonicals in `counts$canonicals_off_domain`.
* `pagerank()` warns when a canonical or redirect folds a crawled page onto an uncrawled URL that is also linked, listing them in the audit's `fold$collisions`.
* `pagerank()` warns when a domain or host filter value matched the input but was folded away before filtering.
* `pagerank()` gains `out_of_scope_fold` (`"relabel"`, `"keep"` or `"leak"`) for canonicals and redirects that point outside the crawl.
* `pagerank()` gains `duplicate_edge_policy`: `"collapse"` (the default, unchanged), `"aggregate"` or `"count_instances"`.
* `pagerank()` results carry a `transition_audit` attribute recording counts, dropped data, mass and configuration, with a `print()` method.
* New `topic_sensitive_pagerank()` computes per-topic PageRank for caller-supplied seed clusters, plus a blended score (Haveliwala 2002).
* New `topic_feeder_pagerank()` finds the pages that feed a cluster by running seeded PageRank on the reversed graph.
* New `trustrank()` and `seed_prior()` compute seed-biased, TrustRank-style PageRank (Gyöngyi, Garcia-Molina & Pedersen 2004).
* New `hits()` and `compute_hits()` compute Kleinberg's HITS hub and authority scores over the same link graph as `pagerank()`.
* New `salsa()` and `compute_salsa()` compute SALSA hub and authority scores (Lempel & Moran 2001) over the same link graph as `pagerank()`.
* New `smooth_transitions()` shrinks sparse empirical transition shares toward the crawl's link structure, so no crawled link gets zero probability.
* New `screaming_frog_internal()` imports Internal: All exports into node, redirect, canonical and indexability tables.
* New `screaming_frog_links()` imports All Inlinks and All Outlinks exports into raw observations and Hyperlink-only graph edges.
* New `screaming_frog_bundle()` combines those imports into one crawl object, and new `pagerank_screaming_frog()` scores it through `pagerank()`.
* New `resolve_canonicals()`, `resolve_canonical_urls()` and `resolve_folded_urls()` resolve rel=canonical and composed redirect and canonical folds.
* New `aggregate_edges()` aggregates edges after folding with per-column rules.
* New `transform_edge_weights()` turns per-source weights into a `transition_probability` that sums to 1 within each source.
* New `ga4_page_transitions()` counts consecutive page-view transitions from a GA4 BigQuery export.
* New `ga4_entrance_teleport()` builds a teleport vector from GA4 entrance counts.
