R package `pagerankr`: SEO-focused PageRank modeling on crawl data (edge lists, redirect
reports, Screaming Frog exports). On CRAN; lifecycle experimental.

- `page_state` (crawl-derived page condition) and `node_status` (graph role) are separate axes.
- Node identity is rurl's canonicalization profile, pinned in `canonical_profile()`. Changing
  it re-keys the graph and must stay in sync with the other rurl consumers.
- Defaults are faithful: the untransformed choice shows the site as crawled; log priors, folds
  and weighting are documented opt-ins. Cheap variants become `pagerank_grid()` columns, not re-runs.
- Crawl exports under `_scratch/crawls/` exceed 1 GB. Aggregate in Rscript with
  `data.table::fread(select = ...)`, print summaries only, and bound schema peeks with `head -c`.
- `NEWS.md` covers library changes. Crawl measurements and ranking predictions go in a vignette
  or `notes/`.
- A gate red on an untouched tree: run `scripts/check-toolchain.R` first. If it passes, report
  the red gate with its evidence instead of assuming the change caused it.

For the push gate, arming it, its skip flags and the WORDLIST rule, see CONTRIBUTING.md.
