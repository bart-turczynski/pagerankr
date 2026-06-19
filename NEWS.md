# pagerankr (development version)

* New `topic_feeder_pagerank()` and `feeder_seed_prior()` answer the inverse of
  `topic_sensitive_pagerank()`: not "which page is most authoritative *for* this
  cluster" but "which pages *feed / power* this cluster" — the internal hubs
  whose outlinks point into the target pages. It seeds the teleport prior on the
  cluster and runs `pagerank()` on the transposed graph (`reverse = TRUE`), so
  mass walks backward along links and accumulates on the feeders, attenuating
  with link distance. This is the reverse-graph sibling of Topic-Sensitive
  PageRank and the cluster-biased counterpart to the global inverse PageRank
  (`pagerank(reverse = TRUE)`); the feeders are the high-`pagerank` rows with
  `prior_weight == 0` (cluster pages carry teleport mass directly). Pure
  orchestration over the existing TIPR personalization path on the reversed
  graph — no new solver. New `topic_feeder_pagerank` vignette walks through the
  AI-Agent-cluster use case and contrasts it with the forward authority view and
  with HITS hubs (PAGE-gwjdxmlm).
* New `salsa()` and `compute_salsa()` add Lempel & Moran's (2001) SALSA hub and
  authority scores: a stochastic variant of HITS that runs the
  mutual-reinforcement step as PageRank-style random walks on the bipartite
  hub/authority graph, so the scores are stationary distributions rather than
  dominant eigenvectors. Computed over the same cleaned,
  redirect/canonical-folded, domain-filtered, deduplicated link graph as
  `pagerank()`, so hub, authority, and PageRank share node identities. Uses the
  degree-based closed form (Proposition 6) — no eigenvector iteration — with the
  required weakly-connected-component mass-weighting correction so
  cross-component scores stay comparable on crawls with orphan clusters. Each
  side sums to 1; coverage differs from PageRank by design (`hub` is `NA` for
  pure sinks, `authority` is `NA` for pure sources). v1 is unweighted; a
  weighted extension is deferred. Documented as a site-graph adaptation of the
  original focused-subgraph algorithm (PAGE-sghubtxk).
* New `trustrank()` and `trust_seed_prior()` add TrustRank-style seed-biased
  PageRank (Gyöngyi, Garcia-Molina & Pedersen, 2004): personalized PageRank
  whose teleport vector is concentrated on a set of trusted seed pages, so
  trust flows outward from the seeds and attenuates with distance (the damping
  factor *is* the attenuation). `trust_seed_prior()` builds a `prior_df` from a
  seed set (character vector or weighted `data.frame`; equal weights reproduce
  the original uniform seed distribution), and `trustrank()` is the one-call
  wrapper that builds the seed prior and runs `pagerank()` with it. Pure
  orchestration over the existing TIPR personalization path — no new solver;
  seed selection is the caller's (it is seed-biased PageRank, not a spam
  classifier). New `trustrank` vignette walks through a worked example
  (PAGE-bdgzgwmb).
* New `topic_sensitive_pagerank()` computes per-topic PageRank by running the
  existing `pagerank()` engine once per topic with a teleport prior biased
  toward each topic's seed cluster, then blends the per-topic scores into a
  single ranking. This is Haveliwala's (2002) Topic-Sensitive PageRank adapted
  to a single site: each "topic" is a content cluster (e.g. the *pricing* or
  *AI-Agent* area) given as a character vector of seed URLs or a weighted
  `data.frame`. Returns one score column per topic plus a weight-normalized
  `blended` column, with the per-topic `transition_audit` objects attached.
  Pure orchestration over the TIPR personalization path — no new solver, and
  topic membership is supplied by the caller, not inferred (PAGE-yubrkleu).
* New `smooth_transitions()` shrinks sparse empirical page-transition shares
  (e.g. from `ga4_page_transitions()`) toward the crawl-graph link structure,
  so no valid crawled link is ever assigned exactly zero probability. Uses a
  per-source Dirichlet/pseudocount shrinkage weight `lambda_i = n_i / (n_i + k)`
  that increases with the source page's sample size, with `min_support`
  fallback to the prior, optional weighted priors, and an `origin` diagnostic
  (`both` / `empirical_only` / `structural_only`). Time decay and
  device/template/channel segmentation are handled upstream by shaping the
  count input (PAGE-hafjjear).
* New `hits()` and `compute_hits()` add Kleinberg's HITS hub and authority
  scores, computed with `igraph::hits_scores()` over the same cleaned,
  redirect/canonical-folded, domain-filtered, deduplicated link graph as
  `pagerank()`, so hub, authority, and PageRank share node identities. Docs
  cover the matrix formulation (authority = dominant eigenvector of `A^T A`,
  hub = dominant eigenvector of `A A^T`) and the whole-graph caveat: unlike
  Kleinberg's query-focused base set, these are site-wide structural
  centralities (PAGE-ymcxurfd).
* `pagerank()` now has an explicit `duplicate_edge_policy` for repeated
  `from -> to` rows after URL folding and filtering. The default `"collapse"`
  preserves the standard binary/destination-level PageRank convention and
  previous results; opt-in `"aggregate"` sums duplicate numeric weights with
  `aggregate_edges()` semantics; opt-in `"count_instances"` models a
  link-slot surfer where repeated links increase transition probability and
  records instance-count details in the transition audit (PAGE-akyureac).
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
