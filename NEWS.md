# pagerankr (development version)

* New `hits()` and `compute_hits()` add Kleinberg's HITS hub and authority
  scores, computed with `igraph::hits_scores()` over the same cleaned,
  redirect/canonical-folded, domain-filtered, deduplicated link graph as
  `pagerank()`, so hub, authority, and PageRank share node identities. Docs
  cover the matrix formulation (authority = dominant eigenvector of `A^T A`,
  hub = dominant eigenvector of `A A^T`) and the whole-graph caveat: unlike
  Kleinberg's query-focused base set, these are site-wide structural
  centralities (PAGE-ymcxurfd).
* Documented the indexed-corpus assumption used by `pagerank()`: noindex
  pages may receive authority but their outlinks are treated as nofollow for
  propagation within the indexed graph. The docs now distinguish
  slot-consuming `"evaporate"`, slot-removing `"drop"`, and normally followed
  `"keep"` without attributing this package model to Google (PAGE-aniklatq).
* New `screaming_frog_links()` imports **All Inlinks** and **All Outlinks**
  with identical Source-to-Destination orientation, preserving raw duplicate
  observations while deriving explicit Hyperlink-only graph edges with
  nofollow, placement, origin, endpoint, and exclusion diagnostics
  (PAGE-gzitxahc).
* New `screaming_frog_internal()` imports UTF-8/BOM **Internal: All** exports
  with alias-insensitive schema detection and selective file reads. It returns
  deterministic node, redirect, canonical, and indexability tables while
  preserving raw URLs and reporting missing, duplicate, invalid, and ignored
  input facts (PAGE-bleererh).
* `pagerank()` now attaches a `transition_audit` provenance object to its result
  as `attr(result, "transition_audit")` (backward-compatible): row/edge counts,
  behavioral-weight coverage, normalization total, dropped data (NA / dedup /
  self-loop rows, unmatched prior URLs), robots-blocked count, and the model
  configuration used. Has a `print` method (PAGE-czbpthiz).
* The `transition_audit` object's `mass` field now decomposes the page-mass
  deficit precisely into `reported` (visible page mass), `sink` (evaporated
  nofollow-sink mass), `hidden` (robots-blocked mass), and `total` (= 1 by
  construction) — replacing undifferentiated "leakage" language with precise
  evaporated/hidden accounting (PAGE-mqsxrcdz).
* New `aggregate_edges()`: loss-aware post-fold edge aggregation with explicit
  per-column semantics (sum counts, boolean conflict policy `any`/`all`/
  `majority`/`error`, `preserve_cols` list-columns for placement features)
  (PAGE-aiigeiyz).
* New `transform_edge_weights()`: per-source grouped weight transforms reusing
  `transform_weights()` methods, emitting a per-source `transition_probability`
  that sums to 1 within each `by` group (PAGE-bvhojxhd).
* New `ga4_page_transitions()`: consecutive-page-view transition counts from a
  GA4 BigQuery export with a deterministic session/event ordering contract
  (timestamp + batch tie-breaks). A transition signal, not a link-click signal
  (PAGE-tcjwgtqd).
* New `ga4_entrance_teleport()`: entrance/landing-page counts as a teleport
  (reset) vector reusing the `prior_df` / `align_prior_to_vertices()` machinery;
  documented as a proxy, distinct from the backlink-authority prior
  (PAGE-bajcmzez).

* Initial CI and lint baseline.
