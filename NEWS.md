# pagerankr (development version)

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
