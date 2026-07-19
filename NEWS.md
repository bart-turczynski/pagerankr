# pagerankr 0.1.0

_Released 2026-07-11._

* Documented that `canonical_profile()` deliberately leaves `rurl`'s
  component-dropping knobs unpinned (`query_handling`, `port_handling`, and the
  `url_standard` selector added in `rurl` 2.2.0) -- they have no effect on the
  scheme+host+path node key at their defaults. Added a behavioral guard in
  `test-canonicalization.R` asserting a canonical key drops the port, query, and
  fragment, so a future `rurl` default flip on an unpinned knob is caught here
  rather than silently changing node identity. No node keys change; verified
  against `rurl` 2.2.0.

* `canonical_profile()` now pins `path_normalization = "dot_segments"` and
  `path_encoding = "decode"` (previously `"none"` / `"keep"`). `rurl` 2.1.0
  silently redefined those two default values to keep the path verbatim, which
  changed node keys for any URL with dot-segments (`/a/../b`) or percent-encoding
  (`/%41`, `%20`) and desynced the pagerankr <-> semantic node join. Pinning the
  explicit values restores the original committed key (path percent-decoded,
  dot-segments removed) and keeps node identities stable across the `rurl`
  upgrade. The `semantic` sibling pins the identical profile (changed together).

* `clean_url_columns()` now preserves tokens that `rurl` cannot parse as a URL
  (e.g. a dotless bare label such as `"A"`) as their raw value instead of
  turning them into NA. Newer `rurl` (>= 2.1.0) normalizes such dotless tokens
  to NA; combined with the `rurl` floor bump in the follow-up,
  this had silently collapsed non-URL node identities to NA — they were then
  dropped by `get_unique_edges()`, so [pagerank()] returned an empty result for
  any graph built from bare labels. Unparseable-but-present tokens are now kept
  as opaque nodes (only genuinely missing NA inputs stay NA), mirroring
  `.apply_fold_map()`'s leave-unmapped-values-untouched contract.

* `filter_links_by_domain()`'s encoding-independent registrable-domain matching
  now reads `rurl`'s new `domain_ascii` column (`rurl` >= 2.1.0) instead of a
  separate IDNA-forced parse. `.build_url_maps()` parses each unique URL once
  (host + `domain_ascii`) rather than twice, and the `.domain_profile()`
  forced-idna helper is gone. Behavior is unchanged — `münchen.de` and
  `xn--mnchen-3ya.de` still fold to one key under every `host_encoding`.

* `pagerank_screaming_frog()` gains `apply_canonicals` and `apply_redirects`
  toggles (both `TRUE` by default, preserving current behavior). Setting either
  to `FALSE` skips folding the bundle's canonical / redirect signals into
  [pagerank()] (passes `canonicals_df` / `redirects_df` as `NULL`), giving a
  supported as-crawled run that keeps the crawled node identities — the escape
  hatch for crawls whose canonicals point off the crawled domain (mirror /
  staging hosts) and would otherwise relabel crawled pages onto uncrawled
  targets. The reserved-argument guard still blocks the raw `canonicals_df` /
  `redirects_df` pagerank arguments. `screaming_frog_bundle()` now reports an
  off-domain canonical count (`counts$canonicals_off_domain`, surfaced in
  `summary()`/`print()`), reusing the existing absent-target classification, and
  the wrapper exposes it on the `screaming_frog_import` audit so the
  mirror-staging scenario is visible at import and scoring time.

* `pagerank()` now detects **fold-target collisions**: when a canonical/redirect
  relabels a crawled page's node onto an *uncrawled* URL that is ALSO
  independently referenced as a genuine link endpoint, the two silently merge
  into one vertex and the crawled page absorbs the inbound link equity of that
  uncrawled URL.
  `pagerank()` emits a `warning()` naming the merged URL(s) and records them in
  the `fold` section of the `transition_audit` object under a new `collisions`
  field (a data frame of `target` / `n_independent_refs` / `source`, or `NULL`
  when none). The crawl's known-URL set (`indexability_df`) is used to tell an
  uncrawled fold target from a genuinely crawled leaf page, so the diagnostic is
  only computed when an `indexability_df` is supplied.

* `pagerank()` now warns when a `keep_domains` / `exclude_domains` /
  `keep_hosts` / `exclude_hosts` value matched the crawled input but no node
  after folding — i.e. an out-of-scope canonical/redirect rewrote the crawled
  domain/host away before filtering (which runs after folding). The warning
  names the folded-away value(s) and points at the fold as the cause. The
  fold-then-filter ordering is now documented explicitly in the `pagerank()`
  and `filter_links_by_domain()` docs; to scope the crawled input, filter with
  `filter_links_by_domain()` before calling `pagerank()`.

* `out_of_scope_fold` gains a third policy, `"leak"`: a crawled page whose
  canonical/redirect folds out of scope is treated like an external redirect —
  its inbound equity is routed onto a dedicated leak sink and evaporates out of
  the measured graph (its outbound edges are dropped), so it does not rank and
  its equity is not credited to any surviving page. The evaporated equity is
  reported as a new `leaked` term in the `transition_audit` `mass` accounting,
  which now decomposes as `reported + sink + leaked + hidden = total` (`= 1`);
  `leaked` is `0` for `"relabel"`/`"keep"` runs so their totals are unchanged.
  The `fold` audit section reports `policy == "leak"`.

* `pagerank()` gains an `out_of_scope_fold` argument (`"relabel"` default, or
  `"keep"`) governing composed fold-map entries whose target is not itself a
  crawled node. `"relabel"` preserves current behavior (fold crawled sources
  onto uncrawled canonical/redirect targets); `"keep"` drops those out-of-scope
  entries before folding so crawled pages retain their as-crawled identity
  rather than being relabeled to phantom vertices (the same filtered map folds
  the TIPR prior). Regardless of policy, the count and list of out-of-scope
  folds (source, target, signal) are recorded in a new `fold` section of the
  `transition_audit` object.

* Code-quality pass: `anyNA()` replaces `any(is.na())`, `!all(x)` replaces
  `any(!x)`, nested `ifelse()` replaced with vectorized assignment,
  `expect_gt()`/`expect_lt()`/`expect_length()`/`expect_null()` adopted where
  applicable, and redundant `c()` wrappers around single-string aliases removed.

* `pagerank()` and `compute_pagerank()` gain convergence controls and reporting.
  The new `algo` argument selects the `igraph::page_rank()` back-end
  (`"prpack"`, the fast exact default, or `"arpack"`, the iterative solver), and
  the friendly `eps` / `niter` aliases re-introduce the L1 tolerance and maximum
  iteration count that modern `igraph` dropped, mapping onto the ARPACK
  `options$tol` / `options$maxiter`; supplying either transparently switches to
  ARPACK. Every non-empty result now carries a `"convergence"` attribute (a
  `pagerank_convergence` object) reporting the solver, iteration count (when the
  solver exposes it), and a solver-independent post-hoc L1 residual
  `||Gx - x||_1` of the returned vector — a genuine quality check comparable
  across both back-ends. Docs cover the damping/iteration-count rule of thumb
  `log10(eps) / log10(damping)`.
* New `topic_feeder_pagerank()` answers the inverse of
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
  with HITS hubs.
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
  original focused-subgraph algorithm.
* New `trustrank()` and the shared `seed_prior()` builder add TrustRank-style
  seed-biased PageRank (Gyöngyi, Garcia-Molina & Pedersen, 2004): personalized
  PageRank whose teleport vector is concentrated on a set of trusted seed pages,
  so trust flows outward from the seeds and attenuates with distance (the
  damping factor *is* the attenuation). `seed_prior()` builds a `prior_df` from
  a seed set (character vector or weighted `data.frame`; equal weights reproduce
  the original uniform seed distribution) and is orientation-agnostic — the same
  builder feeds `topic_feeder_pagerank()` on the reversed graph. `trustrank()`
  is the one-call wrapper that builds the seed prior and runs `pagerank()` with
  it. Pure
  orchestration over the existing TIPR personalization path — no new solver;
  seed selection is the caller's (it is seed-biased PageRank, not a spam
  classifier). New `trustrank` vignette walks through a worked example.
* New `topic_sensitive_pagerank()` computes per-topic PageRank by running the
  existing `pagerank()` engine once per topic with a teleport prior biased
  toward each topic's seed cluster, then blends the per-topic scores into a
  single ranking. This is Haveliwala's (2002) Topic-Sensitive PageRank adapted
  to a single site: each "topic" is a content cluster (e.g. the *pricing* or
  *AI-Agent* area) given as a character vector of seed URLs or a weighted
  `data.frame`. Returns one score column per topic plus a weight-normalized
  `blended` column, with the per-topic `transition_audit` objects attached.
  Pure orchestration over the TIPR personalization path — no new solver, and
  topic membership is supplied by the caller, not inferred.
* New `smooth_transitions()` shrinks sparse empirical page-transition shares
  (e.g. from `ga4_page_transitions()`) toward the crawl-graph link structure,
  so no valid crawled link is ever assigned exactly zero probability. Uses a
  per-source Dirichlet/pseudocount shrinkage weight `lambda_i = n_i / (n_i + k)`
  that increases with the source page's sample size, with `min_support`
  fallback to the prior, optional weighted priors, and an `origin` diagnostic
  (`both` / `empirical_only` / `structural_only`). Time decay and
  device/template/channel segmentation are handled upstream by shaping the
  count input.
* New `hits()` and `compute_hits()` add Kleinberg's HITS hub and authority
  scores, computed with `igraph::hits_scores()` over the same cleaned,
  redirect/canonical-folded, domain-filtered, deduplicated link graph as
  `pagerank()`, so hub, authority, and PageRank share node identities. Docs
  cover the matrix formulation (authority = dominant eigenvector of `A^T A`,
  hub = dominant eigenvector of `A A^T`) and the whole-graph caveat: unlike
  Kleinberg's query-focused base set, these are site-wide structural
  centralities.
* `pagerank()` now has an explicit `duplicate_edge_policy` for repeated
  `from -> to` rows after URL folding and filtering. The default `"collapse"`
  preserves the standard binary/destination-level PageRank convention and
  previous results; opt-in `"aggregate"` sums duplicate numeric weights with
  `aggregate_edges()` semantics; opt-in `"count_instances"` models a
  link-slot surfer where repeated links increase transition probability and
  records instance-count details in the transition audit.
* Recorded representative Screaming Frog crawl acceptance results and added
  package-level operational documentation for required exports, default graph
  policy, optional origin/placement policies, and contract pinning.
* New `pagerank_screaming_frog()` scores a `screaming_frog_bundle()` through
  the existing `pagerank()` pipeline, feeding only graph-eligible hyperlink
  edges while attaching Screaming Frog import diagnostics beside the transition
  audit. Placement/origin filtering and placement-derived weighting are
  explicit opt-ins.
* New `screaming_frog_bundle()` composes Internal: All and All Inlinks/Outlinks
  adapters into the stable crawl handoff object with raw observations, graph
  edges, node signals, cross-table reconciliation diagnostics, provenance, and
  concise print/summary methods.
* New canonical and composed URL-resolution helpers:
  `resolve_canonicals()`, `resolve_canonical_urls()`, and
  `resolve_folded_urls()` expose the existing fold-map engine for rel=canonical
  and redirect+canonical URL folding without duplicating resolver logic.
* Documented the indexed-corpus assumption used by `pagerank()`: noindex
  pages may receive authority but their outlinks are treated as nofollow for
  propagation within the indexed graph. The docs now distinguish
  slot-consuming `"evaporate"`, slot-removing `"drop"`, and normally followed
  `"keep"` without attributing this package model to Google.
* New `screaming_frog_links()` imports **All Inlinks** and **All Outlinks**
  with identical Source-to-Destination orientation, preserving raw duplicate
  observations while deriving explicit Hyperlink-only graph edges with
  nofollow, placement, origin, endpoint, and exclusion diagnostics.
* New `screaming_frog_internal()` imports UTF-8/BOM **Internal: All** exports
  with alias-insensitive schema detection and selective file reads. It returns
  deterministic node, redirect, canonical, and indexability tables while
  preserving raw URLs and reporting missing, duplicate, invalid, and ignored
  input facts.
* `pagerank()` now attaches a `transition_audit` provenance object to its result
  as `attr(result, "transition_audit")` (backward-compatible): row/edge counts,
  behavioral-weight coverage, normalization total, dropped data (NA / dedup /
  self-loop rows, unmatched prior URLs), robots-blocked count, and the model
  configuration used. Has a `print` method.
* The `transition_audit` object's `mass` field now decomposes the page-mass
  deficit precisely into `reported` (visible page mass), `sink` (evaporated
  nofollow-sink mass), `hidden` (robots-blocked mass), and `total` (= 1 by
  construction) — replacing undifferentiated "leakage" language with precise
  evaporated/hidden accounting.
* New `aggregate_edges()`: loss-aware post-fold edge aggregation with explicit
  per-column semantics (sum counts, boolean conflict policy `any`/`all`/
  `majority`/`error`, `preserve_cols` list-columns for placement features).
* New `transform_edge_weights()`: per-source grouped weight transforms reusing
  `transform_weights()` methods, emitting a per-source `transition_probability`
  that sums to 1 within each `by` group.
* New `ga4_page_transitions()`: consecutive-page-view transition counts from a
  GA4 BigQuery export with a deterministic session/event ordering contract
  (timestamp + batch tie-breaks). A transition signal, not a link-click signal.
* New `ga4_entrance_teleport()`: entrance/landing-page counts as a teleport
  (reset) vector reusing the `prior_df` / `align_prior_to_vertices()` machinery;
  documented as a proxy, distinct from the backlink-authority prior.

* Initial CI and lint baseline.
