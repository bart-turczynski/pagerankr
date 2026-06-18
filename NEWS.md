# pagerankr (development version)

* Recorded representative Screaming Frog crawl acceptance results and added
  package-level operational documentation for required exports, default graph
  policy, optional origin/placement policies, and contract pinning
  (PAGE-ikcyjqic).
* New `pagerank_screaming_frog()` scores a `screaming_frog_bundle()` through
  the existing `pagerank()` pipeline, feeding only graph-eligible hyperlink
  edges while attaching Screaming Frog import diagnostics beside the transition
  audit. Placement/origin filtering and placement-derived weighting are
  explicit opt-ins (PAGE-sgyfclym).
* New `screaming_frog_bundle()` composes Internal: All and All Inlinks/Outlinks
  adapters into the stable crawl handoff object with raw observations, graph
  edges, node signals, cross-table reconciliation diagnostics, provenance, and
  concise print/summary methods (PAGE-uggwyfop).
* New canonical and composed URL-resolution helpers:
  `resolve_canonicals()`, `resolve_canonical_urls()`, and
  `resolve_folded_urls()` expose the existing fold-map engine for rel=canonical
  and redirect+canonical URL folding without duplicating resolver logic
  (PAGE-mhtjirux).
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
