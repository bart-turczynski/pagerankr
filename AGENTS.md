R package `pagerankr`: SEO-focused PageRank modeling on crawl data (edge lists, redirect
reports, Screaming Frog exports). On CRAN; lifecycle experimental.

- `page_state` (crawl-derived page condition) and `node_status` (graph role) are separate axes.
- Node identity is rurl's canonicalization profile, pinned in `canonical_profile()`. Changing
  it re-keys the graph and must stay in sync with the other rurl consumers.
- Defaults are faithful: the untransformed choice shows the site as crawled; log priors, folds
  and weighting are documented opt-ins. Cheap variants become `pagerank_grid()` columns, not re-runs.
- Crawl exports under `_scratch/crawls/` exceed 1 GB. Aggregate in Rscript with
  `data.table::fread(select = ...)`, print summaries only, and bound schema peeks with `head -c`.
- `NEWS.md` lists only what a package user acts on or notices, each bullet one terse line (at
  most 32 words). Cite a governing standard as a short clause, end with at most one issue or MR
  reference. No housekeeping bullets (CI, gates, lint, agent files, metadata), no Internal
  section; the `news-terse` hook enforces it. Crawl measurements and ranking predictions go in a
  vignette or `notes/`.
- A gate red on an untouched tree: run `scripts/check-toolchain.R` first. If it passes, report
  the red gate with its evidence instead of assuming the change caused it.

## Verification

Gate: local
Verify: .fleet-ci/gate auto

The merge gate is fleet-ci's local gate, run in the fleet image by the pre-push hook; install it
once per clone with `.fleet-ci/pre-push --install`, and its settings are `.fleet-ci.yml`. A diff
the classifier calls docs-only gets T0 (spelling, `pkgdown::check_pkgdown()`); any other gets T1:
the NEWS heading, codemeta, docs drift, lintr, README drift, the URL check and one `R CMD check`.
Branch pushes, merge requests and pushes to `main` create no pipeline. CI runs fleet-ci's
template: T2 on the weekly `deep-check` schedule, T3 on a `RELEASE=1` web pipeline and on the
`v*` tag, and `x-rurl-devel` plus `coverage` on the `rurl-devel` schedule (seor ADR 0012;
design/ci.md).

For installing the hooks and the WORDLIST rule, see CONTRIBUTING.md.
