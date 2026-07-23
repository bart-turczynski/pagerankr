#' @title Master PageRank Calculation Wrapper
#' @description Orchestrates the complete PageRank calculation workflow,
#' including URL cleaning, redirect resolution, edge deduplication,
#' indexability handling, nofollow handling, isolate handling, and
#' PageRank computation.
#' @name pagerank
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns like "from" and "to". Edges are expected to be page-to-page
#'   hyperlinks: `pagerank()` is graph-agnostic and treats every endpoint as a
#'   node, so resource links (images, CSS, JS, and other non-HTML references)
#'   must be filtered upstream or they collect authority as ordinary vertices.
#'   `pagerank_screaming_frog()` does this at the crawl boundary via
#'   [sf_graph_eligible()] (`Hyperlink` only); a hand-built or non-SF edge list
#'   should apply the same hyperlink-only filter before scoring.
#' @param redirects_df An optional data frame for redirect rules, typically
#'   with "from" and "to" columns. Defaults to NULL.
#' @param clean_edge_urls Logical, whether to clean URLs in the edge list.
#'   Defaults to TRUE.
#' @param clean_redirect_urls Logical, whether to clean URLs in the redirect
#'   list. Defaults to TRUE. Only effective if `redirects_df` is provided.
#' @param rurl_params A list of parameters to pass to `rurl::clean_url`.
#'   Defaults to an empty list. `protocol_handling` defaults to `"keep"` and
#'   `case_handling` to `"lower_host"` for cross-project canonicalization
#'   consistency. If you set `host_encoding` (`"idna"` or `"unicode"`) to fold
#'   internationalized (IDN) hosts, that same value is also passed to the
#'   domain-filtering step so its comparisons stay consistent with the cleaned
#'   node keys. (Registrable-domain matching is encoding-independent, so this
#'   only matters if host-level filtering is involved.)
#' @param self_loops A character string specifying how to handle self-loops.
#'   Either "drop" (default) or "keep".
#' @param drop_isolates_flag Logical, whether to drop isolated nodes before
#'   PageRank computation. Defaults to TRUE.
#' @param reverse Logical. If `TRUE`, PageRank is computed on the transposed
#'   (edge-reversed) graph, yielding reverse / inverse PageRank instead of the
#'   usual inflow score. Default `FALSE`. See the "Reverse / inverse PageRank"
#'   section in Details for what it measures and which other arguments are
#'   compatible.
#' @param damping The PageRank damping factor \eqn{\alpha} (the random surfer's
#'   continue probability; the teleport probability is \eqn{1 - \alpha}). A
#'   single number in `[0, 1]`, default `0.85` — the field convention from Brin
#'   & Page. Forwarded to [compute_pagerank()] and on to `igraph::page_rank()`.
#'   Higher values weight the link structure more heavily but converge more
#'   slowly: a power-iteration solve needs roughly
#'   \eqn{\log_{10}(\tau) / \log_{10}(\alpha)} iterations to reach residual
#'   \eqn{\tau}, so pushing \eqn{\alpha} toward 1 sharply raises the iteration
#'   count (raise `niter` accordingly when using the ARPACK solver). See the
#'   "Damping factor" section in Details for guidance on choosing it, and
#'   [damping_sensitivity()] to sweep a range of values.
#' @param weight_col Optional name of a numeric column in `edge_list_df`
#'   containing edge weights. Higher weights make edges more likely to be
#'   followed. If `NULL` (default), all edges have equal weight.
#' @param placement_col Optional name of a column in `edge_list_df` holding the
#'   page region each link sits in, using the crawler-neutral vocabulary
#'   `"content"`, `"nav"`, `"header"`, `"footer"`, `"aside"`. Matching is
#'   case-insensitive and whitespace is trimmed. `NULL` (default) means no
#'   placement handling. Placement is **not** a Screaming Frog concept: a
#'   per-crawler adapter maps vendor labels onto this vocabulary (see
#'   [sf_normalize_position()]) and `pagerank()` only consumes the result, so
#'   any crawler that reports link regions can drive placement-aware scoring.
#' @param accepted_placements Optional character vector of placements to
#'   retain; edges placed elsewhere (or with a missing placement) are dropped.
#'   `NULL` (default) keeps every edge. Requires `placement_col`.
#' @param placement_weights Optional named positive numeric vector assigning
#'   edge weights by placement, e.g.
#'   `c(content = 1, nav = 0.1, header = 0.1, footer = 0.1, aside = 0.1)`.
#'   Placements not named keep weight `1`, so name all five to state a complete
#'   recipe. Requires `placement_col` and cannot be combined with `weight_col`,
#'   which it supersedes by building a weight column of its own. Downweighting
#'   rather than filtering is deliberate: dropping a region changes the graph's
#'   *shape* (pages reachable only through nav become teleport-only, pages
#'   linking out only through nav become dangling), whereas a small weight
#'   leaves the topology intact and merely stops the region dominating.
#' @param container_col Optional name of a column in `edge_list_df` identifying
#'   the **source-side component** each link sits in -- the template element the
#'   link belongs to, stable across the pages that element appears on. Supplying
#'   it switches on the boilerplate detector; `NULL` (default) leaves it off.
#'   Like `placement_col` this is crawler-neutral data: a per-crawler adapter
#'   derives component identity from whatever the crawler reports (a DOM path,
#'   a CSS selector, a template ID) and `pagerank()` only consumes the result,
#'   so any crawler that can identify a link's component can drive the
#'   detector. Cannot be combined with
#'   `weight_col`, which it supersedes by building a weight column of its own.
#' @param boilerplate_threshold The container-conditioned recurrence ratio at
#'   or above which an edge is **classified** boilerplate, in `(0, 1]`. The
#'   ratio is the share of pages carrying the container on which that container
#'   points at this same target, so `1` means "every time this component
#'   appeared, it linked here" and values near `0` mean the component chooses a
#'   different target on each page. Default `0.5`. Only consulted when
#'   `container_col` is supplied.
#' @param min_container_pages Minimum number of pages a container must appear
#'   on before any of its edges may be classified. Default `10`. Small
#'   containers are excluded because their ratios are quantized -- a container
#'   on three pages can only score `0.33`, `0.67` or `1` -- so a high ratio
#'   there is thin evidence rather than a strong signal. A judgement call, not
#'   a measured cut.
#' @param boilerplate_weight The multiplier applied to an edge **classified**
#'   boilerplate, in `(0, 1]`. Default `0.5`. Note this is a different quantity
#'   from `boilerplate_threshold` despite sharing a default value: the
#'   threshold is a fraction of pages that decides *whether* an edge is
#'   boilerplate, this is the discount applied *once it is*. Placement and
#'   recurrence are two **detectors feeding one graded axis**, not two
#'   independent axes: a nav link is boilerplate by construction, so the
#'   factors are not multiplied -- that would discount the same link twice for
#'   the same fact. The strongest applicable discount wins, giving chrome
#'   `0.1`, repetitive in-content `0.5`, and unique in-content `1`. Both
#'   factors are recorded separately in the transition audit.
#' @param position_col Optional name of a numeric column in `edge_list_df`
#'   holding each link's **position index** within its source page -- `1` for
#'   the first link, `2` for the second, and so on in reading order. Supplying
#'   it switches on the positional-decay axis; `NULL` (default) leaves it off.
#'   This is the genuinely orthogonal axis of the edge-weighting model: where
#'   placement and recurrence describe *templatedness* (and feed one graded axis
#'   combined by minimum), position describes *reading order* and so composes by
#'   **multiplication** -- an above-the-fold boilerplate CTA (`0.5 * 1.0`)
#'   outranks a trailing organic link (`1.0 * 0.2`) with no special-casing. Like
#'   `placement_col` and `container_col` this is crawler-neutral data: the index
#'   must be materialized from document order **at ingest**, while it is still
#'   trustworthy, and never inferred from row order here, where a filter, join
#'   or dedup may already have destroyed it (for Screaming Frog it is read from
#'   an **All Outlinks** export, whose row order is document order, never All
#'   Inlinks, whose row order is destination-alphabetical). Edges with no index
#'   (`NA`) keep position weight `1`, so ranking only the source's main-content
#'   links -- leaving site chrome to the placement axis -- is expressed by
#'   indexing only those links. Cannot be combined with `weight_col`, which it
#'   supersedes by building a weight column of its own.
#' @param position_transform The reading-order decay applied to `position_col`,
#'   one of `"zipf"` (default) or `"rank_linear"`, reusing [transform_weights()]
#'   within each source page's choice set. `"zipf"` gives
#'   `weight = 1 / rank^position_alpha` (position 1 keeps weight `1`, later
#'   positions drop off as a power law); `"rank_linear"` gives
#'   `weight = (n - rank + 1) / n` across a source's `n` indexed links. Only
#'   consulted when `position_col` is supplied.
#' @param position_alpha The exponent for `position_transform = "zipf"`, a
#'   single positive number. Default `1`. Higher values make the drop-off
#'   steeper, so position 1 dominates its page more. Unused by `"rank_linear"`.
#' @param position_floor The smallest position weight, in `(0, 1]`. Default
#'   `0.01`. Decayed weights are clamped up to this floor so that compounding
#'   the two axes can never reach `0` -- an "effectively dropped" edge must not
#'   sneak back in through decay (the same downweight-not-drop rule that governs
#'   placement and boilerplate). Only consulted when `position_col` is supplied.
#' @param duplicate_edge_policy How repeated `from -> to` rows are represented
#'   after URL cleaning, redirect/canonical folding, and domain filtering. One
#'   of:
#'   \describe{
#'     \item{`"collapse"`}{(default) Destination-level surfer: repeated rows
#'       collapse to one unweighted destination edge, preserving legacy
#'       `get_unique_edges()` behavior and the common binary PageRank
#'       convention.}
#'     \item{`"aggregate"`}{Collapse each `from -> to` pair with
#'       [aggregate_edges()] semantics. Numeric columns, including
#'       `weight_col`, are summed; logical columns such as `nofollow` use the
#'       default `"any"` conflict policy.}
#'     \item{`"count_instances"`}{Link-slot / edge-level surfer: repeated
#'       rows increase transition probability. With no `weight_col`, each
#'       surviving `from -> to` pair receives an internal weight equal to its
#'       duplicate-row count. With `weight_col`, weights are summed and an
#'       `instance_count` audit column is retained.}
#'   }
#' @param nofollow_col Optional name of a logical or 0/1 column in
#'   `edge_list_df` indicating nofollow edges. If `NULL` (default),
#'   no nofollow handling is performed.
#' @param nofollow_action How to handle nofollow edges when `nofollow_col` is
#'   provided. One of:
#'   \describe{
#'     \item{`"evaporate"`}{(default) Nofollow links remain outgoing slots:
#'       they consume their weighted share of the source node's outgoing PR
#'       budget but pass nothing to their targets. Implemented via a sink node
#'       that absorbs the unpropagated PR.}
#'     \item{`"drop"`}{Remove nofollow edges before allocating the outgoing
#'       budget. They consume no slots, so followed edges divide the full
#'       budget among themselves.}
#'     \item{`"keep"`}{Retain and follow these edges normally, so their targets
#'       receive their allocated shares.}
#'   }
#' @param indexability_df Optional data frame mapping URLs to their indexability
#'   status (e.g., from an SEO crawl export). See Details.
#' @param indexability_url_col Name of the URL column in `indexability_df`.
#'   Default `"url"`.
#' @param indexability_status_col Name of the status column in
#'   `indexability_df`. Default `"indexability_status"`. Values are
#'   comma-separated strings; recognized statuses are `"Blocked by robots.txt"`
#'   and `"noindex"` (case-insensitive for noindex).
#' @param status_df Optional data frame mapping URLs to their HTTP response
#'   status code (e.g., from an SEO crawl export). Lets `pagerank()` recognize
#'   response-dead pages, which would otherwise be scored as ordinary live
#'   vertices. See the "HTTP response status" section in Details.
#' @param status_url_col Name of the URL column in `status_df`. Default
#'   `"url"`.
#' @param status_col Name of the HTTP status-code column in `status_df`.
#'   Default `"status_code"`. Values are HTTP status codes (integer, or
#'   coercible to integer); codes in `400:599` mark a page response-dead.
#' @param robots_blocked_action How to present robots.txt-blocked pages in
#'   results. Both values route the page's throughput to the shared waste sink
#'   (no self-loop); they differ only in whether the page itself is shown. One
#'   of:
#'   \describe{
#'     \item{`"show"`}{(default) Blocked pages appear in results showing the
#'       authority they collect, useful for seeing wasted PageRank. What they
#'       would pass on evaporates to the sink.}
#'     \item{`"vanish"`}{Blocked pages are removed from results; their own
#'       stationary mass is booked as hidden (their throughput still evaporates
#'       to the sink).}
#'   }
#' @param edge_from_col,edge_to_col Names of from/to columns in `edge_list_df`.
#' @param redirect_from_col,redirect_to_col Names of from/to columns in
#'   `redirects_df`.
#' @param duplicate_from_policy How to handle conflicting redirects in
#'   `redirects_df`. Passed through to [resolve_redirects()]. Default
#'   `"strict"` (error on conflicts). See [resolve_redirects()] for all
#'   available policies.
#' @param loop_handling How to handle redirect cycles. Passed through to
#'   [resolve_redirects()]. Default `"error"`. See [resolve_redirects()] for
#'   all available policies.
#' @param canonicals_df An optional data frame of declared `rel=canonical`
#'   links, with `from`/`to` columns (or as set by `canonical_from_col` /
#'   `canonical_to_col`) pairing a source URL with the canonical it declares.
#'   Default `NULL` (opt-in; the default preserves current behavior).
#'   Canonicals are a **distinct, advisory** signal from enforced 3xx
#'   `redirects_df`: they are tracked separately and audited via
#'   [audit_canonicals()] / [audit_fold()], then folded into the same composed
#'   map as redirects (see [build_fold_map()]). Self-canonicals drop as no-ops.
#' @param canonical_from_col,canonical_to_col From/to columns in
#'   `canonicals_df`. Default `"from"` / `"to"`.
#' @param clean_canonical_urls Logical, whether to clean URLs in `canonicals_df`
#'   using the same resolved `rurl_params` profile as edge and redirect
#'   cleaning. Default `TRUE`. Only effective when `canonicals_df` is provided.
#' @param canonical_duplicate_from_policy How to handle a canonical source that
#'   declares multiple distinct canonicals. Reuses the `duplicate_from_policy`
#'   enum (see [resolve_redirects()]). Default `"strict"`.
#' @param canonical_loop_handling How to handle cycles among declared
#'   canonicals. Reuses the `loop_handling` enum. Default `"error"`.
#' @param canonical_conflict_policy How to resolve a redirect-vs-canonical
#'   disagreement on the **same source** URL. One of `"redirect_wins"` (default;
#'   the 3xx wins and the canonical on a redirecting source is ignored and
#'   flagged), `"error"` (error on genuine disagreement), or `"canonical_wins"`
#'   (the declared canonical wins for that source, still flagged). See
#'   [build_fold_map()].
#' @param out_of_scope_fold Policy for composed fold-map entries whose
#'   **target** (the representative a source folds onto) is not itself a crawled
#'   node. The crawled node set is the unique, non-`NA` edge endpoints captured
#'   immediately before folding (indexability URLs are not part of scope). Such
#'   an out-of-scope fold silently relabels a crawled page onto an uncrawled
#'   URL, inventing a phantom vertex (e.g. a staging crawl whose canonicals all
#'   point at the uncrawled production domain). One of:
#'   \describe{
#'     \item{`"relabel"`}{(Default) Apply the full fold map unchanged,
#'       relabeling crawled sources onto their out-of-scope targets. Preserves
#'       historical behavior.}
#'     \item{`"keep"`}{Drop the out-of-scope entries from the fold map before
#'       applying it, so crawled source nodes retain their as-crawled identity.
#'       The same filtered map is applied to the TIPR prior fold, keeping edges
#'       and prior in one namespace.}
#'     \item{`"leak"`}{Treat each such crawled source like an external redirect:
#'       route it onto a synthetic **leak sink** node (distinct from the
#'       nofollow sink) so the equity flowing INTO it leaves the measured graph
#'       ("these pages won't rank; equity goes elsewhere"). The source's own
#'       teleport prior is routed to the sink too. The evaporated equity is
#'       reported as **leaked mass** in the `mass` section of the
#'       `transition_audit` (`reported + sink + hidden + leaked = 1`). The leak
#'       sink is created whenever there is at least one out-of-scope fold,
#'       regardless of `nofollow_action`.}
#'   }
#'   Regardless of policy, the count and list of out-of-scope folds (source,
#'   target, signal) are recorded in the `fold` section of the
#'   `transition_audit` object. See [transition_audit].
#' @param keep_domains Optional character vector of domains to keep. When
#'   provided, edges are filtered via [filter_links_by_domain()] so that only
#'   links where both endpoints belong to one of the specified domains are
#'   included. Useful for restricting to internal links. Default `NULL` (no
#'   domain filtering).
#'
#'   **Ordering:** filtering runs *after* redirect/canonical folding, so it
#'   scopes the post-fold (canonical) namespace, not the crawled input. If an
#'   out-of-scope canonical/redirect rewrites the crawled domain onto a
#'   different one, filtering on the crawled domain matches nothing (an empty
#'   graph). To scope the INPUT you crawled, run [filter_links_by_domain()] on
#'   the edge list *before* calling `pagerank()`.
#' @param exclude_domains Optional character vector of domains to exclude.
#'   Edges where either endpoint belongs to one of these domains are removed.
#'   Like `keep_domains`, this filters the post-fold namespace (see the ordering
#'   note above). Default `NULL` (no exclusion).
#' @param keep_hosts Optional character vector of exact hosts to keep (e.g.
#'   `"www.example.com"`), as opposed to registrable domains. Matched on the
#'   exact host using the same canonicalization profile as cleaning, so IDN
#'   folding (`host_encoding` in `rurl_params`) applies consistently. Default
#'   `NULL`.
#' @param exclude_hosts Optional character vector of exact hosts to exclude.
#'   Edges where either endpoint matches one of these hosts are removed. Ignore
#'   rules override keep rules. Default `NULL`.
#' @param prior_df Optional per-URL external-authority prior for TIPR
#'   (authority-weighted teleport). A data frame with one row per URL and a
#'   numeric weight. The prior URLs are canonicalized with the same
#'   `rurl_params` and folded through the same redirect map as the edges,
#'   weights for URLs that coalesce are summed, and the result is aligned to the
#'   final vertex set via [align_prior_to_vertices()] and passed to
#'   `igraph::page_rank(personalized = )`. Default `NULL` (uniform teleport).
#'
#'   The weight column must be an **additive raw count** — the redirect fold
#'   sums it (see `prior_weight_col`), which is only meaningful for quantities
#'   that add when URLs coalesce. This keeps the prior **source-agnostic**: the
#'   default is Ahrefs **referring domains**, but any backlink-source count is a
#'   drop-in swap (Ahrefs *links-to-target* or *dofollow-only referring
#'   domains*; SEMrush backlink/referring-domain counts; or even non-backlink
#'   counts such as GA4 entrances), simply by pointing `prior_weight_col` at it.
#'   Do **not** pass a calculated authority *score* (Ahrefs UR / DR, or any
#'   0–100 rating): scores are not additive (folding two redirect variants is a
#'   `max`, not a `sum`), and a per-URL score like UR is itself a PageRank-style
#'   metric — using it as a teleport prior for PageRank is circular. See
#'   [align_prior_to_vertices()] for the full contract.
#' @param prior_url_col,prior_weight_col Column names in `prior_df`. Defaults
#'   `"url"` / `"weight"`. Swapping `prior_weight_col` between additive count
#'   columns is the supported way to A/B alternative authority metrics (e.g. via
#'   [pagerank_grid()]); see `prior_df` for which metrics qualify.
#' @param prior_transform How to shape raw authority before it becomes teleport
#'   mass. One of `"none"` (default, faithful linear share), `"log"`,
#'   `"percentile"`, `"minmax"`, `"zipf"`, `"rank_linear"`. See
#'   [transform_weights()]. Counts are summed on the raw scale before any
#'   transform.
#' @param prior_alpha Mixture weight in `[0, 1]` between uniform and
#'   authority-weighted teleport (`p = alpha * uniform + (1 - alpha) *
#'   authority_share`). `0` (default) is pure authority teleport; `1` reproduces
#'   uniform PageRank. See [align_prior_to_vertices()].
#' @param prior_inject_unmatched Logical. If `TRUE`, authoritative prior URLs
#'   that do not fold onto any existing vertex are added as edge-less isolate
#'   vertices so they appear in results carrying their teleport prior. Default
#'   `FALSE` (align-only: such URLs are dropped and logged).
#' @param prior_exclude_waste Logical. If `TRUE` (default), the
#'   collect-but-cannot-pass class — noindex, robots-blocked, and 4xx/5xx pages
#'   (see `indexability_df` / `status_df`) — is excluded from the teleport
#'   vector: those pages keep the authority that reaches them through inlinks
#'   but are no longer paid the uniform teleport share for merely existing. This
#'   stops a page from manufacturing authority by linking to many dead ends
#'   (Page & Brin 1998 criticize uniform teleport for "valuing pages simply
#'   because they exist"). Set `FALSE` to give every page uniform teleport,
#'   matching `igraph::page_rank()` for canonical comparisons. Has no effect
#'   unless `indexability_df` or `status_df` supplies the class; the synthetic
#'   evaporation and leak sinks are excluded from teleport regardless.
#' @param prior_verbose Logical, whether to emit prior-alignment coverage
#'   diagnostics. Default `TRUE`. Only relevant when `prior_df` is supplied.
#' @param ... Additional arguments passed to [compute_pagerank()] and
#'   subsequently to `igraph::page_rank()`. Besides `damping`, the recognized
#'   convergence controls `algo` (`"prpack"` / `"arpack"`), `eps`, and `niter`
#'   are forwarded here; see the "Convergence controls" section below.
#' @param preset Optional named argument bundle describing a common view of
#'   the graph: a preset name (`"raw"`, `"declared"`, `"reversed"`,
#'   `"content"`), a [pr_preset()]
#'   result, or `NULL` (default, no preset). Preset values are applied only to
#'   arguments you did not name yourself, so precedence is **explicit argument
#'   > preset > base default**. Must be named in full (it sits after `...`).
#'   See [pr_preset()] for the exact expansion of each preset.
#'
#' @details
#' ## Damping factor
#'
#' The `damping` factor \eqn{\alpha} is the probability that the random surfer
#' follows a link rather than teleporting; the remaining \eqn{1 - \alpha} is
#' spread over the teleport vector (uniform, or the supplied TIPR `prior_df`).
#'
#' The default `0.85` is the original Brin & Page value and remains the field
#' convention, but it is *eminently empirical* — Boldi, Santini & Vigna
#' (\emph{PageRank as a Function of the Damping Factor}, WWW 2005) show it has
#' no analytical claim to being uniquely correct. A common misconception is that
#' values close to 1 yield "more accurate" rankings by trusting the link graph
#' more; for real-world graphs they instead make the ranking dominated by the
#' graph's largest near-cyclic component and, in the limit \eqn{\alpha \to 1},
#' degenerate rather than converge to a more meaningful order.
#'
#' Raising \eqn{\alpha} also degrades convergence sharply. A power-iteration
#' solve needs about \eqn{\log_{10}(\tau) / \log_{10}(\alpha)} iterations to
#' reach residual \eqn{\tau} (Langville & Meyer, \emph{Deeper Inside PageRank},
#' Internet Mathematics 2004). At \eqn{\tau = 10^{-8}}: \eqn{\alpha = 0.85}
#' needs ~114 iterations, \eqn{\alpha = 0.95} ~362, and \eqn{\alpha = 0.99}
#' ~1,833 — so a high damping factor is both slower and rarely better. When you
#' do raise it on the ARPACK solver, raise `niter` to match (see "Convergence
#' controls" below).
#'
#' Both of those papers study the open web. Whether `0.85` is still the right
#' convention for a site-scale intranet graph is an open empirical question;
#' [damping_sensitivity()] sweeps a range of \eqn{\alpha} values so you can see
#' how much the ranking on *your* graph actually moves.
#'
#' ## Convergence controls
#'
#' `igraph::page_rank()` is called through one of two solver back-ends, selected
#' with `algo` (forwarded via `...`):
#' \describe{
#'   \item{`"prpack"`}{(default) A fast, exact direct solver. It has **no**
#'     tunable tolerance or iteration cap, and reports no iteration count.}
#'   \item{`"arpack"`}{An iterative eigensolver that honors `eps` (the L1
#'     tolerance) and `niter` (the maximum iterations), and reports how many
#'     iterations it used.}
#' }
#'
#' Modern `igraph` (2.x) removed the legacy `page_rank()` `eps` / `niter`
#' arguments; this package re-exposes them as friendly aliases for the ARPACK
#' `options$tol` / `options$maxiter` controls. Because PRPACK ignores them,
#' supplying either `eps` or `niter` transparently switches `algo` to
#' `"arpack"`. As a rule of thumb a power-iteration solve needs about
#' `log10(eps) / log10(damping)` iterations, so raise `niter` when you push
#' `damping` toward 1.
#'
#' Every non-empty result carries a `"convergence"` attribute (a
#' [pagerank_convergence] object) reporting the solver, iteration count (when
#' the solver exposes it), and the solver-independent post-hoc L1 residual
#' \eqn{\|G x - x\|_1} of the returned vector. Retrieve it with
#' `attr(result, "convergence")`.
#'
#' ## The waste class (noindex, robots-blocked, response-dead)
#'
#' `pagerankr` models one **"collects PageRank but cannot pass it"** class and
#' routes every member through a single shared **waste sink** with the same
#' mechanism: the member loses all of its outgoing edges and gains exactly one
#' edge to the sink, so it still absorbs the authority its inlinks send but
#' passes none of it back into the graph. The sink is an internal accounting
#' bucket (never a page, always stripped from the returned result); the mass it
#' collects is reported as **evaporated** mass in the transition audit. Removing
#' the old robots-blocked self-loop is deliberate: a self-loop is an absorbing
#' rank sink that compounds inbound authority every iteration (a measured 8.3×
#' inflation), whereas the waste sink lets authority flow in and stop.
#'
#' Members come from three signals:
#'
#' **noindex** (`indexability_df`): `pagerankr` models the ranked corpus as the
#' set of indexed documents, so a noindex page is outside it — it may receive
#' authority through inlinks but cannot redistribute it within the indexed
#' graph. This is a PageRank modeling choice; it does not assert that Google
#' defines noindex as a nofollow directive. noindex routing to the sink is
#' independent of `nofollow_action` (which governs only real `rel=nofollow`
#' edges): a noindex page always routes to the sink. noindex pages still appear
#' in results so their received authority remains auditable.
#'
#' **robots.txt-blocked** (`indexability_df`): Google cannot access the page
#' content, so there are no visible outgoing links. `robots_blocked_action`
#' controls only whether the page appears in results (`"show"`, the default) or
#' is removed with its own mass booked as hidden (`"vanish"`) — both route the
#' page's throughput to the sink.
#'
#' **Priority rule:** robots.txt always takes precedence over noindex. If a
#' page is both robots-blocked and noindex, it is treated as robots-blocked.
#'
#' ## HTTP response status
#'
#' When `status_df` is provided, pages whose HTTP status code falls in
#' `400:599` are recognized as **response-dead**: at crawl time they returned
#' no content and expose no outgoing links, so they can collect authority
#' through their inlinks but cannot pass any of it on. They belong to the same
#' waste class as noindex pages and route to the same sink; because a dead page
#' typically has no outlinks, this ADDS the one edge to the sink that stops it
#' from dangling and recycling its inbound authority to every page via teleport.
#'
#' `pagerankr` does **not** split 4xx from 5xx. The crawl is a snapshot, and at
#' crawl time a transient `503` and a permanent `404` are indistinguishable:
#' both return no content and expose no links. Modeling one as recoverable
#' would require guessing about a future the crawl has no data on — the same
#' reason `pagerankr` folds a `302` exactly like a `301`. A caller who knows a
#' given `5xx` was a blip should re-crawl rather than have the tool assume
#' recovery on its behalf.
#'
#' `3xx` redirects are **not** part of this class; they are modeled through
#' `redirects_df`. Codes below `400`, and rows whose status is missing or
#' cannot be parsed as an integer, are treated as live. Response-dead pages
#' that are present in the graph are counted in the returned `transition_audit`
#' (`config$has_status` and `n_status_dead`).
#'
#' ## Reverse / inverse PageRank (`reverse = TRUE`)
#'
#' Standard PageRank measures **inflow** importance ("who points to me"). With
#' `reverse = TRUE` the link graph is transposed before computation, yielding
#' **outflow centrality** ("does this page funnel authority outward"). This is
#' the *reverse PageRank* of Bar-Yossef & Mashiach (CIKM 2008), equivalent to
#' the **CheiRank** of the transposed Google matrix, and the PageRank-flavored
#' analogue of the *hub* score in Kleinberg's HITS. The sibling `semantic`
#' project consumes this as an outflow signal.
#'
#' Only edge orientation is flipped; URL cleaning, redirect folding,
#' duplicate-edge policy, edge weights, domain/host filtering, and the teleport
#' prior all behave identically (they are direction-agnostic). To obtain it
#' directly from an edge list, swapping the from/to columns and running ordinary
#' `pagerank()` is equivalent — `reverse = TRUE` just performs that flip
#' internally so weight, redirect, and sink handling cannot be mis-wired by a
#' manual swap.
#'
#' **This is unrelated to the TIPR / personalized-prior feature (`prior_df`).**
#' That seeds the *teleport* vector with external authority (e.g. backlinks) but
#' still computes inflow PageRank on the forward graph; `reverse` is a pure
#' *graph operation* on edge direction. The two are orthogonal and may be
#' combined.
#'
#' **Direction-sensitive features are rejected under `reverse = TRUE`** because
#' their semantics do not transpose:
#' \describe{
#'   \item{`nofollow_action = "evaporate"`}{Errors. The evaporation sink models
#'     a *source* wasting its outgoing budget; reversed, it would inject rank
#'     instead. Use `"drop"` — the correct treatment of a nofollowed link for
#'     outflow centrality, since it funnels no authority outward — or `"keep"`.}
#'   \item{`indexability_df`}{Errors. noindex and robots.txt blocking (route
#'     the page's outgoing budget to the waste sink) encode forward crawl/index
#'     behavior with no meaningful transpose.}
#' }
#'
#' ## Duplicate edge policy
#'
#' The original PageRank papers define a page's vote as divided by its outgoing
#' link count but do not pin down how repeated hyperlinks from one source page
#' to the same target are represented. The standard textbook / binary
#' operationalization treats the outgoing set as a destination relation, so
#' multiple `A -> C` rows collapse to one destination edge. `pagerankr` keeps
#' that as the default (`duplicate_edge_policy = "collapse"`) for backward
#' compatibility and as the less spam-sensitive model.
#'
#' Weighted / multigraph PageRank is also valid when repeated link slots are
#' the intended unit. Use `duplicate_edge_policy = "count_instances"` for a
#' link-slot surfer: `A -> B, A -> C, A -> C` sends twice as much outgoing mass
#' to `C` as to `B`, equivalent to explicit weights `B = 1, C = 2` and to
#' igraph's treatment of parallel edges. Use `"aggregate"` when duplicate rows
#' should be collapsed loss-aware, especially with an existing `weight_col`;
#' numeric duplicate weights are summed instead of silently keeping the first
#' row.
#'
#' ## Fold-then-filter ordering (domain / host scope)
#'
#' Redirect and canonical folding runs **before** the
#' `keep_domains` / `exclude_domains` / `keep_hosts` / `exclude_hosts`
#' filter. Folding can rewrite the node namespace: an out-of-scope canonical
#' (e.g. every `staging.example.dev` page declaring a `example.com` canonical)
#' relabels crawled nodes onto a domain you never crawled. Because the filter
#' then sees only the post-fold (canonical) namespace, filtering on the domain
#' you actually crawled matches nothing and returns an empty graph.
#'
#' `pagerank()` detects this specific case -- a filter value that classified
#' one or more crawled (pre-fold) nodes but no surviving post-fold node -- and
#' emits an actionable `warning()` naming the folded-away value(s) and pointing
#' at the out-of-scope fold as the cause. This is a diagnostic only; the
#' fold-then-filter order is unchanged.
#'
#' To scope the **input you crawled**, filter first: run
#' [filter_links_by_domain()] on the edge list (and, if used, the redirect /
#' canonical data frames) *before* calling `pagerank()`. To scope the folded
#' graph, filter on the post-fold (canonical) domain/host instead.
#'
#' @return A data frame with node names and their PageRank scores. When
#'   nofollow evaporation, the waste class (noindex / robots-blocked /
#'   response-dead), or `robots_blocked_action = "vanish"` is active, the
#'   returned scores may sum to less than 1. The difference is not
#'   undifferentiated "leakage": it is decomposed into **evaporated mass**
#'   (authority sent to the shared waste sink, i.e. what the class and every
#'   real nofollowed link passed on but could not deliver), **leaked mass**
#'   (authority sent to the leak sink under `out_of_scope_fold = "leak"`), and
#'   **hidden mass** (the own stationary mass of robots-blocked nodes removed
#'   from the results). The full breakdown — reported / evaporated (sink) /
#'   leaked / hidden / total (= 1) — is recorded in the `mass` field of the
#'   transition audit (see below).
#'
#'   When `indexability_df` or `status_df` is supplied, the result gains two
#'   per-URL waste-attribution columns, present only with those inputs
#'   (mirroring how `prior_weight` appears only with `prior_df`), so the result
#'   is otherwise unchanged:
#'   \describe{
#'     \item{`page_state`}{The page's health/indexability state: `"live"`,
#'       `"noindex"`, `"robots_blocked"`, or `"response_dead"` (robots-blocked
#'       > response-dead > noindex > live when a page carries more than one
#'       signal).}
#'     \item{`wasted_mass`}{The authority the page collected and black-holed —
#'       its share of the shared waste sink's stationary mass. A waste-class
#'       page routes its whole throughput to the absorbing sink, so this is
#'       `damping / (1 - damping)` times its own reported score: larger than,
#'       and distinct from, that score, which answers the "how much did this
#'       page amass and evaporate" question `page_state` only labels. It sums
#'       across the waste class to the evaporated mass reported in the
#'       transition audit (`mass$sink`). A `"live"` page routes nothing to the
#'       sink, so its `wasted_mass` is `0`.}
#'   }
#'   `page_state` is the page's *health* state; the `node_status` column
#'   returned by [simulate_changes()] is a distinct axis — a node's *role in a
#'   before/after comparison* (`normal` / `new-target` / `removed-dead`) — not a
#'   second name for the same thing.
#'
#'   The data frame additionally carries a `"transition_audit"` attribute (a
#'   [transition_audit] object) recording how the transition graph was built:
#'   row/edge counts, behavioral-weight coverage, normalization totals, the
#'   page-mass decomposition (reported / evaporated / leaked / hidden / total),
#'   dropped
#'   data (rows lost to NA / dedup / self-loops, unmatched prior URLs), and the
#'   model configuration used. Retrieve it with
#'   `attr(result, "transition_audit")`.
#'
#'   The result additionally carries a `"convergence"` attribute (a
#'   [pagerank_convergence] object); see the "Convergence controls" section.
#' @export
#' @examples
#' # Basic example
#' edges <- data.frame(
#'   from = c("http://A.com/", "B", "C?q=1", "D"),
#'   to = c("B", "http://A.com", "D#frag", "D")
#' )
#' redirects <- data.frame(
#'   from = c("C?q=1", "B"),
#'   to = c("http://C_resolved.com", "A") # B redirects to A, C to C_resolved
#' )
#'
#' # Run full pipeline
#' pr_full <- pagerank(
#'   edges,
#'   redirects_df = redirects, self_loops = "drop", drop_isolates_flag = TRUE
#' )
#' print(pr_full)
#'
#' # Run without URL cleaning for edges
#' # (warning expected if query params present)
#' pr_no_edge_clean <- pagerank(
#'   edges,
#'   redirects_df = redirects, clean_edge_urls = FALSE
#' )
#' print(pr_no_edge_clean)
#'
#' # Keep isolates
#' edges_isol <- rbind(edges, data.frame(from = "ISO", to = "LAND"))
#' pr_keep_isolates <- pagerank(edges_isol, drop_isolates_flag = FALSE)
#' print(pr_keep_isolates)
#'
#' # With nofollow edges (evaporate mode)
#' edges_nf <- data.frame(
#'   from = c("A", "A", "B"), to = c("B", "C", "A"),
#'   nofollow = c(FALSE, TRUE, FALSE)
#' )
#' pr_nf <- pagerank(edges_nf,
#'   nofollow_col = "nofollow",
#'   nofollow_action = "evaporate", clean_edge_urls = FALSE
#' )
#' print(pr_nf)
#'
#' # Reverse / inverse PageRank (outflow centrality, a.k.a. CheiRank):
#' # a page that funnels authority outward scores high.
#' pr_reverse <- pagerank(edges, redirects_df = redirects, reverse = TRUE)
#' print(pr_reverse)
pagerank <- function(
  edge_list_df,
  redirects_df = NULL,
  clean_edge_urls = TRUE,
  clean_redirect_urls = TRUE,
  rurl_params = list(),
  self_loops = c("drop", "keep"),
  drop_isolates_flag = TRUE,
  reverse = FALSE,
  weight_col = NULL,
  placement_col = NULL,
  accepted_placements = NULL,
  placement_weights = NULL,
  container_col = NULL,
  boilerplate_threshold = 0.5,
  min_container_pages = 10,
  boilerplate_weight = 0.5,
  position_col = NULL,
  position_transform = c("zipf", "rank_linear"),
  position_alpha = 1,
  position_floor = 0.01,
  duplicate_edge_policy = c(
    "collapse",
    "aggregate",
    "count_instances"
  ),
  nofollow_col = NULL,
  nofollow_action = c("evaporate", "drop", "keep"),
  indexability_df = NULL,
  indexability_url_col = "url",
  indexability_status_col = "indexability_status",
  status_df = NULL,
  status_url_col = "url",
  status_col = "status_code",
  robots_blocked_action = c("show", "vanish"),
  edge_from_col = "from",
  edge_to_col = "to",
  redirect_from_col = "from",
  redirect_to_col = "to",
  duplicate_from_policy = c(
    "strict",
    "first_wins",
    "last_wins",
    "most_frequent",
    "prune_source",
    "resolve_if_consistent"
  ),
  loop_handling = c(
    "error",
    "prune_loop",
    "break_arrow"
  ),
  canonicals_df = NULL,
  canonical_from_col = "from",
  canonical_to_col = "to",
  clean_canonical_urls = TRUE,
  canonical_duplicate_from_policy = c(
    "strict",
    "first_wins",
    "last_wins",
    "most_frequent",
    "prune_source",
    "resolve_if_consistent"
  ),
  canonical_loop_handling = c(
    "error",
    "prune_loop",
    "break_arrow"
  ),
  canonical_conflict_policy = c(
    "redirect_wins",
    "error",
    "canonical_wins"
  ),
  out_of_scope_fold = c("relabel", "keep", "leak"),
  keep_domains = NULL,
  exclude_domains = NULL,
  keep_hosts = NULL,
  exclude_hosts = NULL,
  prior_df = NULL,
  prior_url_col = "url",
  prior_weight_col = "weight",
  prior_transform = c(
    "none",
    "log",
    "percentile",
    "minmax",
    "zipf",
    "rank_linear"
  ),
  prior_alpha = 0,
  prior_inject_unmatched = FALSE,
  prior_exclude_waste = TRUE,
  prior_verbose = TRUE,
  damping = 0.85,
  ...,
  preset = NULL
) {
  # --- Preset expansion (before anything reads an argument) ---
  # Writes preset values into this frame for arguments the caller did not
  # name, so precedence stays explicit arg > preset > base default. See
  # .pr_apply_preset() in R/presets.R.
  .pr_matched_call <- match.call()
  .pr_preset_applied <- .pr_apply_preset(
    preset, .pr_matched_call, environment()
  )

  # --- Argument Matching and Basic Validation ---
  self_loops <- match.arg(self_loops)
  nofollow_action <- match.arg(nofollow_action)
  robots_blocked_action <- match.arg(robots_blocked_action)
  duplicate_edge_policy <- match.arg(duplicate_edge_policy)
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)
  canonical_duplicate_from_policy <- match.arg(canonical_duplicate_from_policy)
  canonical_loop_handling <- match.arg(canonical_loop_handling)
  canonical_conflict_policy <- match.arg(canonical_conflict_policy)
  out_of_scope_fold <- match.arg(out_of_scope_fold)
  prior_transform <- match.arg(prior_transform)
  position_transform <- match.arg(position_transform)

  # Hoisted above the main validation block: `placement_weights` builds a
  # weight column of its own, so pairing it with `weight_col` is a contradiction
  # worth reporting before `weight_col` is checked against the edge list.
  .pr_check_weight_col_exclusivity(weight_col, placement_weights)
  .pr_check_container_weight_exclusivity(weight_col, container_col)
  .pr_check_position_weight_exclusivity(weight_col, position_col)

  # Validate all arguments up front (see .validate_pagerank_args below). Kept in
  # a dedicated helper so this orchestrator stays readable; error messages and
  # `call. = FALSE` behavior are unchanged.
  .validate_pagerank_args(
    edge_list_df = edge_list_df,
    redirects_df = redirects_df,
    canonicals_df = canonicals_df,
    clean_canonical_urls = clean_canonical_urls,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    clean_edge_urls = clean_edge_urls,
    clean_redirect_urls = clean_redirect_urls,
    rurl_params = rurl_params,
    drop_isolates_flag = drop_isolates_flag,
    reverse = reverse,
    damping = damping,
    weight_col = weight_col,
    nofollow_col = nofollow_col,
    nofollow_action = nofollow_action,
    indexability_df = indexability_df,
    indexability_url_col = indexability_url_col,
    indexability_status_col = indexability_status_col,
    status_df = status_df,
    status_url_col = status_url_col,
    status_col = status_col,
    prior_df = prior_df,
    prior_url_col = prior_url_col,
    prior_weight_col = prior_weight_col,
    prior_alpha = prior_alpha,
    prior_inject_unmatched = prior_inject_unmatched,
    prior_exclude_waste = prior_exclude_waste
  )

  # --- 0. Placement: region filter + region weighting -----------------------
  # Runs before URL cleaning so the rest of the pipeline sees a plain weighted
  # edge list and knows nothing about page regions. `placement_weights` builds
  # a synthetic weight column and hands it back as `weight_col`; see
  # .pr_apply_placement() in R/placement.R.
  .placement <- .pr_apply_placement(
    edge_list_df = edge_list_df,
    placement_col = placement_col,
    accepted_placements = accepted_placements,
    placement_weights = placement_weights,
    weight_col = weight_col,
    preset_source = .pr_placement_preset_source(preset, .pr_preset_applied)
  )
  edge_list_df <- .placement$edge_list_df
  weight_col <- .placement$weight_col

  # Dots for igraph params are handled by compute_pagerank directly.

  # --- 1. Working copies, input-row count, URL cleaning + fold-scope snap ---
  # See .pagerank_clean_inputs: resolves the canonicalization profile once,
  # cleans the edge / redirect / canonical URL columns through it, and captures
  # the raw input-row count and the pre-fold crawled node snapshot used by the
  # domain filter (step 2.7).
  .prep <- .pagerank_clean_inputs(
    edge_list_df = edge_list_df,
    redirects_df = redirects_df,
    canonicals_df = canonicals_df,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    redirect_from_col = redirect_from_col,
    redirect_to_col = redirect_to_col,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    clean_edge_urls = clean_edge_urls,
    clean_redirect_urls = clean_redirect_urls,
    clean_canonical_urls = clean_canonical_urls,
    rurl_params = rurl_params
  )
  current_edge_list <- .prep$edge_list_df

  # --- 1.5. Boilerplate: container-conditioned recurrence discount ----------
  # Runs *after* URL cleaning, unlike placement: placement reads a categorical
  # column and does not care what the URLs look like, whereas this detector
  # counts distinct source pages and distinct targets, so it needs identities
  # already normalized or the same page counts twice under two spellings. It
  # runs *before* the fold (step 2) so a container is judged on what it links
  # to as crawled. See .pr_apply_boilerplate() in R/boilerplate.R.
  .boilerplate <- .pr_apply_boilerplate(
    edge_list_df = current_edge_list,
    container_col = container_col,
    from_col = edge_from_col,
    to_col = edge_to_col,
    boilerplate_threshold = boilerplate_threshold,
    min_container_pages = min_container_pages,
    boilerplate_weight = boilerplate_weight,
    weight_col = weight_col
  )
  current_edge_list <- .boilerplate$edge_list_df
  weight_col <- .boilerplate$weight_col

  # --- 1.6. Position: reading-order decay within the source page -----------
  # The orthogonal axis. Placement and recurrence feed one graded axis (via
  # `pmin`); position measures reading order instead and so *multiplies* into
  # the weight. Runs after boilerplate so it composes on top of whatever the
  # graded axis produced, and before the fold so the per-source choice sets are
  # still the ones that were crawled. The per-source position index is data the
  # caller supplies (materialized at ingest); pagerank() never reads order from
  # row order here. See .pr_apply_position() in R/position.R.
  .position <- .pr_apply_position(
    edge_list_df = current_edge_list,
    position_col = position_col,
    from_col = edge_from_col,
    position_transform = position_transform,
    position_alpha = position_alpha,
    position_floor = position_floor,
    weight_col = weight_col
  )
  current_edge_list <- .position$edge_list_df
  weight_col <- .position$weight_col

  current_redirects_list <- .prep$redirects_df
  current_canonicals_list <- .prep$canonicals_df
  effective_rurl_params <- .prep$effective_rurl_params
  sf_prefold_nodes <- .prep$sf_prefold_nodes
  audit_n_input_rows <- .prep$n_input_rows

  # --- 2. Redirect + canonical resolution (one composed fold map) ---
  # Build and apply the single composed fold map (redirects + canonicals),
  # handling out-of-scope-fold policy and fold-target collision detection. See
  # .resolve_fold_and_apply below. `.fold$fold_map` remains the source of truth
  # for TIPR prior folding (step 2.8). The leak sink is a synthetic node,
  # distinct from the nofollow sink, that later absorbs equity flowing into
  # out-of-scope-folded sources under `out_of_scope_fold = "leak"`.
  leak_sink_name <- "__pr_leak_sink__"
  # `applied` is TRUE when out-of-scope folds were acted upon (relabeled
  # through, or routed to the leak sink) and FALSE when skipped / kept.
  audit_oos_applied <- out_of_scope_fold %in% c("relabel", "leak")

  .fold <- .resolve_fold_and_apply(
    edge_list_df = current_edge_list,
    redirects_df = current_redirects_list,
    canonicals_df = current_canonicals_list,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    redirect_from_col = redirect_from_col,
    redirect_to_col = redirect_to_col,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    duplicate_from_policy = duplicate_from_policy,
    loop_handling = loop_handling,
    canonical_duplicate_from_policy = canonical_duplicate_from_policy,
    canonical_loop_handling = canonical_loop_handling,
    canonical_conflict_policy = canonical_conflict_policy,
    out_of_scope_fold = out_of_scope_fold,
    indexability_df = indexability_df,
    indexability_url_col = indexability_url_col,
    clean_edge_urls = clean_edge_urls,
    effective_rurl_params = effective_rurl_params
  )
  current_edge_list <- .fold$edge_list_df
  fold_map <- .fold$fold_map
  audit_has_redirects <- .fold$audit_has_redirects
  audit_has_canonicals <- .fold$audit_has_canonicals
  audit_oos_sources <- .fold$audit_oos_sources
  audit_oos_targets <- .fold$audit_oos_targets
  audit_oos_signals <- .fold$audit_oos_signals
  audit_collisions_df <- .fold$audit_collisions_df
  leak_sources <- .fold$leak_sources
  used_leak_sink <- .fold$used_leak_sink

  # --- 2.5-2.8. Scope the post-fold namespace + prepare the prior ---
  # See .pagerank_scope_and_prior: applies domain/host filtering (AFTER the
  # fold), snapshots the full vertex universe BEFORE NA rows are stripped (so
  # drop_isolates_flag = FALSE can keep partial-row nodes as isolates), routes
  # out-of-scope-folded sources to the leak sink, and folds the TIPR prior into
  # the same namespace. The leak-sink self-loop is added later (after dedup) so
  # self_loops = "drop" cannot strip it.
  temp_node_col_name <- "node_name"
  .scoped <- .pagerank_scope_and_prior(
    edge_list_df = current_edge_list,
    sf_prefold_nodes = sf_prefold_nodes,
    keep_domains = keep_domains,
    exclude_domains = exclude_domains,
    keep_hosts = keep_hosts,
    exclude_hosts = exclude_hosts,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    effective_rurl_params = effective_rurl_params,
    used_leak_sink = used_leak_sink,
    leak_sources = leak_sources,
    leak_sink_name = leak_sink_name,
    prior_df = prior_df,
    prior_url_col = prior_url_col,
    prior_weight_col = prior_weight_col,
    clean_edge_urls = clean_edge_urls,
    fold_map = fold_map
  )
  current_edge_list <- .scoped$edge_list_df
  all_vertex_universe <- .scoped$all_vertex_universe
  folded_prior_df <- .scoped$folded_prior_df

  # Response-dead pages (HTTP 4xx/5xx) among the surviving vertices. Classified
  # against the post-fold universe so URLs folded away by a redirect/canonical
  # are not counted. Surfaced in the transition audit; see
  # .classify_status_dead.
  status_dead_urls <- .classify_status_dead(
    status_df = status_df,
    status_url_col = status_url_col,
    status_col = status_col,
    vertex_universe = all_vertex_universe
  )

  # --- Transition audit: account for rows dropped (dedup / NA / self-loops) ---
  # Measured against the post-fold/post-filter edge list, mirroring exactly what
  # get_unique_edges() removes, so the audit reflects the data that actually
  # reached the deduplication step.
  .dropped <- .count_dropped_edge_rows(
    edge_list_df = current_edge_list,
    self_loops = self_loops,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col
  )
  audit_n_rows_na <- .dropped$n_rows_na
  audit_n_self_loops <- .dropped$n_self_loops
  audit_n_rows_duplicate <- .dropped$n_rows_duplicate

  # --- 3. Edge policies: dedup, waste-sink routing, leak-sink self-loop ---
  # See .pagerank_apply_edge_policies: dedups per duplicate_edge_policy (fixing
  # n_edges before any synthetic rows are added), evaporates real nofollow edges
  # per nofollow_action, routes the whole "collects PR but cannot pass it" class
  # (noindex, robots-blocked, 4xx/5xx) through the shared waste sink -- exactly
  # one member -> sink edge each -- and adds the leak-sink self-loop. All after
  # dedup so self_loops = "drop" cannot strip the synthetic rows.
  waste_sink_name <- "__pr_waste_sink__"
  .edges <- .pagerank_apply_edge_policies(
    edge_list_df = current_edge_list,
    duplicate_edge_policy = duplicate_edge_policy,
    self_loops = self_loops,
    weight_col = weight_col,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    indexability_df = indexability_df,
    indexability_url_col = indexability_url_col,
    indexability_status_col = indexability_status_col,
    status_dead_urls = status_dead_urls,
    nofollow_col = nofollow_col,
    all_vertex_universe = all_vertex_universe,
    nofollow_action = nofollow_action,
    waste_sink_name = waste_sink_name,
    used_leak_sink = used_leak_sink,
    leak_sink_name = leak_sink_name
  )
  current_edge_list <- .edges$edge_list_df
  audit_instance_count_col <- .edges$instance_count_col
  effective_weight_col <- .edges$effective_weight_col
  audit_duplicate_edges <- .edges$duplicate_edges
  audit_n_duplicate_instances <- .edges$n_duplicate_instances
  audit_n_edges <- .edges$n_edges
  nofollow_col <- .edges$nofollow_col
  robots_blocked_urls <- .edges$robots_blocked_urls
  noindex_urls <- .edges$noindex_urls
  used_waste_sink <- .edges$used_waste_sink

  # --- 4. Handle isolates + inject unmatched prior URLs (opt-in) ---
  # See .build_vertex_set below: with drop_isolates_flag = TRUE only nodes on a
  # complete edge survive; otherwise the full known universe (including
  # synthetic sink / robots self-loop nodes) is kept, and
  # prior_inject_unmatched surfaces authoritative prior URLs that fold onto no
  # vertex as edge-less isolates.
  vertices_for_pagerank_df <- .build_vertex_set(
    edge_list_df = current_edge_list,
    all_vertex_universe = all_vertex_universe,
    drop_isolates_flag = drop_isolates_flag,
    folded_prior_df = folded_prior_df,
    prior_inject_unmatched = prior_inject_unmatched,
    prior_url_col = prior_url_col,
    node_col_name = temp_node_col_name,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col
  )

  # The synthetic waste and leak sinks never receive teleport. By default
  # (`prior_exclude_waste = TRUE`) the collect-but-cannot-pass class
  # (noindex / robots-blocked / response-dead) is excluded from teleport too:
  # class members keep the authority that flows to them via inlinks but are no
  # longer paid the uniform teleport share for merely existing (PAGE-bcpacnfm).
  prior_exclude_nodes <- .prior_exclude_nodes(
    used_waste_sink = used_waste_sink,
    waste_sink_name = waste_sink_name,
    used_leak_sink = used_leak_sink,
    leak_sink_name = leak_sink_name,
    prior_exclude_waste = prior_exclude_waste,
    noindex_urls = noindex_urls,
    robots_blocked_urls = robots_blocked_urls,
    status_dead_urls = status_dead_urls
  )

  # --- 5. Compute PageRank ---
  pagerank_results <- compute_pagerank(
    edge_list_df = current_edge_list,
    vertices_df = vertices_for_pagerank_df,
    from_col = edge_from_col,
    to_col = edge_to_col,
    vertex_col_name = temp_node_col_name,
    reverse = reverse,
    damping = damping,
    weight_col = effective_weight_col,
    prior_df = folded_prior_df,
    prior_url_col = prior_url_col,
    prior_weight_col = prior_weight_col,
    prior_transform = prior_transform,
    prior_alpha = prior_alpha,
    prior_exclude_nodes = prior_exclude_nodes,
    prior_verbose = prior_verbose,
    ...
  )

  # Capture the convergence diagnostic before the subsetting below, which (like
  # any `[.data.frame`) drops non-standard attributes; re-attached in step 7.
  convergence <- attr(pagerank_results, "convergence")

  # --- 6. Post-processing: remove internal nodes from results ---
  #
  # The stationary vector computed in step 5 spans EVERY node — including the
  # shared waste sink and any robots-blocked nodes that the caller asked to
  # vanish — so it sums to 1 by construction. The visible result only carries
  # real, reported pages, so its scores can sum to < 1. Before dropping the
  # internal nodes we measure how much stationary mass each carried away, so the
  # difference is fully accounted for rather than written off as
  # undifferentiated "leakage":
  #   * evaporated mass = stationary mass parked on the waste sink (what the
  #     whole class — noindex, robots-blocked, 4xx/5xx — and every real
  #     nofollowed outlink passed on but could not deliver);
  #   * leaked mass     = stationary mass parked on the leak sink (the authority
  #     that flowed into out-of-scope-folded sources under
  #     `out_of_scope_fold = "leak"`, treated like an external redirect);
  #   * hidden mass      = the own stationary mass of robots-blocked nodes that
  #     were removed under `robots_blocked_action = "vanish"` (their
  #     pass-through still routed to the waste sink, so it is counted as
  #     evaporated, not hidden).
  # Captured here, fed into the transition audit's mass$ fields in step 7.
  .stripped <- .strip_internal_nodes(
    pagerank_results = pagerank_results,
    used_waste_sink = used_waste_sink,
    waste_sink_name = waste_sink_name,
    used_leak_sink = used_leak_sink,
    leak_sink_name = leak_sink_name,
    robots_blocked_action = robots_blocked_action,
    robots_blocked_urls = robots_blocked_urls
  )
  pagerank_results <- .stripped$pagerank_results
  mass_evaporated <- .stripped$mass_evaporated
  mass_leaked <- .stripped$mass_leaked
  mass_hidden <- .stripped$mass_hidden

  # Per-URL waste attribution: tag every reported page with its page_state
  # (live / noindex / robots_blocked / response_dead) and wasted_mass (the
  # authority it collected and routed to the black-hole sink). Present only when
  # indexability_df or status_df was supplied, mirroring how prior_weight
  # appears only with prior_df. Added after the internal nodes are stripped so
  # the sink and vanished robots pages are never tagged.
  pagerank_results <- .attach_page_state(
    pagerank_results = pagerank_results,
    indexability_df = indexability_df,
    status_df = status_df,
    noindex_urls = noindex_urls,
    robots_blocked_urls = robots_blocked_urls,
    status_dead_urls = status_dead_urls,
    damping = damping
  )

  # --- 7. Transition audit / provenance object ---
  # Assembled from the counts captured along the path above and attached to the
  # result as an attribute (attr(result, "transition_audit")). The attribute
  # approach is backward-compatible: the return value is still the same data
  # frame with the same columns, so existing callers and tests are unaffected,
  # while reproducibility metadata travels alongside the result.

  transition_audit <- .build_transition_audit(
    pagerank_results = pagerank_results,
    current_edge_list = current_edge_list,
    folded_prior_df = folded_prior_df,
    vertices_for_pagerank_df = vertices_for_pagerank_df,
    node_col_name = temp_node_col_name,
    prior_url_col = prior_url_col,
    effective_weight_col = effective_weight_col,
    weight_col = weight_col,
    n_input_rows = audit_n_input_rows,
    n_edges = audit_n_edges,
    duplicate_edge_policy = duplicate_edge_policy,
    instance_count_col = audit_instance_count_col,
    n_duplicate_instances = audit_n_duplicate_instances,
    duplicate_edges = audit_duplicate_edges,
    n_rows_na = audit_n_rows_na,
    n_rows_duplicate = audit_n_rows_duplicate,
    n_self_loops = audit_n_self_loops,
    robots_blocked_urls = robots_blocked_urls,
    status_dead_urls = status_dead_urls,
    mass_evaporated = mass_evaporated,
    mass_leaked = mass_leaked,
    mass_hidden = mass_hidden,
    out_of_scope_fold = out_of_scope_fold,
    oos_sources = audit_oos_sources,
    oos_targets = audit_oos_targets,
    oos_signals = audit_oos_signals,
    oos_applied = audit_oos_applied,
    collisions_df = audit_collisions_df,
    self_loops = self_loops,
    drop_isolates_flag = drop_isolates_flag,
    reverse = reverse,
    nofollow_col = nofollow_col,
    nofollow_action = nofollow_action,
    robots_blocked_action = robots_blocked_action,
    prior_alpha = prior_alpha,
    prior_transform = prior_transform,
    prior_inject_unmatched = prior_inject_unmatched,
    prior_exclude_waste = prior_exclude_waste,
    has_redirects = audit_has_redirects,
    has_canonicals = audit_has_canonicals,
    indexability_df = indexability_df,
    status_df = status_df,
    preset = preset,
    placement = .placement$provenance,
    boilerplate = .boilerplate$provenance,
    position = .position$provenance
  )

  attr(pagerank_results, "transition_audit") <- transition_audit
  attr(pagerank_results, "convergence") <- convergence

  pagerank_results
}

#' Clean pagerank() inputs and snapshot the pre-fold namespace.
#'
#' Resolves the canonicalization profile once (via .resolve_rurl_params, user
#' `rurl_params` overriding per key) and cleans the edge / redirect / canonical
#' URL columns through it -- one resolved profile that also drives fold
#' collision
#' detection, domain filtering, and prior prep so node identities never drift.
#' Also captures the raw input-row count and the crawled node snapshot taken
#' BEFORE any fold (cleaned edge endpoints only; indexability URLs are not part
#' of scope) that the domain filter uses to detect folded-away filter values.
#' @keywords internal
#' @noRd
.pagerank_clean_inputs <- function(
  edge_list_df,
  redirects_df,
  canonicals_df,
  edge_from_col,
  edge_to_col,
  redirect_from_col,
  redirect_to_col,
  canonical_from_col,
  canonical_to_col,
  clean_edge_urls,
  clean_redirect_urls,
  clean_canonical_urls,
  rurl_params
) {
  n_input_rows <- nrow(edge_list_df)
  effective_rurl_params <- .resolve_rurl_params(rurl_params)
  cleaned <- .clean_pipeline_urls(
    edge_list_df = edge_list_df,
    redirects_df = redirects_df,
    canonicals_df = canonicals_df,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    redirect_from_col = redirect_from_col,
    redirect_to_col = redirect_to_col,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    clean_edge_urls = clean_edge_urls,
    clean_redirect_urls = clean_redirect_urls,
    clean_canonical_urls = clean_canonical_urls,
    effective_rurl_params = effective_rurl_params
  )
  edge_list_df <- cleaned$edge_list_df
  sf_prefold_nodes <- unique(stats::na.omit(c(
    as.character(edge_list_df[[edge_from_col]]),
    as.character(edge_list_df[[edge_to_col]])
  )))
  list(
    edge_list_df = edge_list_df,
    redirects_df = cleaned$redirects_df,
    canonicals_df = cleaned$canonicals_df,
    effective_rurl_params = effective_rurl_params,
    sf_prefold_nodes = sf_prefold_nodes,
    n_input_rows = n_input_rows
  )
}

#' Scope the post-fold namespace and fold the TIPR prior into it.
#'
#' Applies domain/host filtering (which runs AFTER the fold, and warns when an
#' out-of-scope fold rewrote a crawled filter value away), snapshots the full
#' vertex universe before NA rows are stripped, routes out-of-scope-folded
#' sources onto the leak sink (retargeting their inbound equity and dropping
#' their outbound edges), and canonicalizes + folds the prior into the same
#' namespace. Returns the scoped edge list, vertex universe, and folded prior.
#' @keywords internal
#' @noRd
.pagerank_scope_and_prior <- function(
  edge_list_df,
  sf_prefold_nodes,
  keep_domains,
  exclude_domains,
  keep_hosts,
  exclude_hosts,
  edge_from_col,
  edge_to_col,
  effective_rurl_params,
  used_leak_sink,
  leak_sources,
  leak_sink_name,
  prior_df,
  prior_url_col,
  prior_weight_col,
  clean_edge_urls,
  fold_map
) {
  edge_list_df <- .apply_domain_host_filter(
    edge_list_df = edge_list_df,
    prefold_nodes = sf_prefold_nodes,
    keep_domains = keep_domains,
    exclude_domains = exclude_domains,
    keep_hosts = keep_hosts,
    exclude_hosts = exclude_hosts,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    effective_rurl_params = effective_rurl_params
  )
  all_vertex_universe <- unique(stats::na.omit(c(
    as.character(edge_list_df[[edge_from_col]]),
    as.character(edge_list_df[[edge_to_col]])
  )))
  leaked <- .route_leak_sources(
    edge_list_df = edge_list_df,
    all_vertex_universe = all_vertex_universe,
    used_leak_sink = used_leak_sink,
    leak_sources = leak_sources,
    leak_sink_name = leak_sink_name,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col
  )
  folded_prior_df <- .prepare_prior(
    prior_df = prior_df,
    prior_url_col = prior_url_col,
    prior_weight_col = prior_weight_col,
    clean_edge_urls = clean_edge_urls,
    effective_rurl_params = effective_rurl_params,
    fold_map = fold_map,
    used_leak_sink = used_leak_sink,
    leak_sources = leak_sources,
    leak_sink_name = leak_sink_name
  )
  list(
    edge_list_df = leaked$edge_list_df,
    all_vertex_universe = leaked$all_vertex_universe,
    folded_prior_df = folded_prior_df
  )
}

#' Apply pagerank() edge policies to the scoped edge list.
#'
#' Dedups per `duplicate_edge_policy` (fixing the scored-edge count before any
#' synthetic rows are added), evaporates real nofollowed edges per
#' `nofollow_action`, routes the whole "collects PageRank but cannot pass it"
#' class (noindex, robots-blocked, 4xx/5xx) through the shared waste sink via a
#' single member -> sink edge each, adds one absorbing sink self-loop when the
#' sink was used, and adds the leak-sink self-loop. All mutations happen after
#' dedup so `self_loops = "drop"` cannot strip the synthetic rows. Returns the
#' mutated edge list plus the dedup / class audit and column metadata.
#' @keywords internal
#' @noRd
.pagerank_apply_edge_policies <- function(
  edge_list_df,
  duplicate_edge_policy,
  self_loops,
  weight_col,
  edge_from_col,
  edge_to_col,
  indexability_df,
  indexability_url_col,
  indexability_status_col,
  status_dead_urls,
  nofollow_col,
  all_vertex_universe,
  nofollow_action,
  waste_sink_name,
  used_leak_sink,
  leak_sink_name
) {
  dup <- .apply_duplicate_policy_audited(
    edge_list_df = edge_list_df,
    duplicate_edge_policy = duplicate_edge_policy,
    self_loops = self_loops,
    weight_col = weight_col,
    from_col = edge_from_col,
    to_col = edge_to_col
  )
  edge_list_df <- dup$edge_list_df
  n_edges <- nrow(edge_list_df)

  # Classify indexability directives (detection only; flow handled below).
  idx <- .classify_indexability(
    indexability_df = indexability_df,
    indexability_url_col = indexability_url_col,
    indexability_status_col = indexability_status_col
  )

  # Real rel=nofollow edges evaporate to (or drop before reaching) the shared
  # waste sink per nofollow_action; noindex is decoupled from this and routed
  # below with the rest of the class.
  nf <- .apply_nofollow(
    edge_list_df = edge_list_df,
    nofollow_col = nofollow_col,
    nofollow_action = nofollow_action,
    waste_sink_name = waste_sink_name,
    from_col = edge_from_col,
    to_col = edge_to_col
  )
  edge_list_df <- nf$edge_list_df

  # Route the whole class through the sink: strip each member's outedges and add
  # exactly one member -> sink edge (the "adds one" that stops a no-outlink 404
  # from dangling into teleport).
  class_urls <- unique(c(
    idx$noindex_urls,
    idx$robots_blocked_urls,
    status_dead_urls
  ))
  wc <- .route_waste_class(
    edge_list_df = edge_list_df,
    class_urls = class_urls,
    all_vertex_universe = all_vertex_universe,
    waste_sink_name = waste_sink_name,
    from_col = edge_from_col,
    to_col = edge_to_col
  )
  edge_list_df <- wc$edge_list_df

  # Exactly one absorbing sink self-loop if either channel used the sink.
  used_waste_sink <- isTRUE(nf$used_waste_sink) || isTRUE(wc$used_sink)
  if (used_waste_sink) {
    edge_list_df <- rbind(
      edge_list_df,
      .make_sink_rows(edge_list_df, edge_from_col, edge_to_col, waste_sink_name)
    )
  }

  edge_list_df <- .add_leak_sink_selfloop(
    edge_list_df = edge_list_df,
    used_leak_sink = used_leak_sink,
    leak_sink_name = leak_sink_name,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col
  )
  list(
    edge_list_df = edge_list_df,
    instance_count_col = dup$instance_count_col,
    effective_weight_col = dup$effective_weight_col,
    duplicate_edges = dup$duplicate_edges,
    n_duplicate_instances = dup$n_duplicate_instances,
    n_edges = n_edges,
    nofollow_col = nofollow_col,
    robots_blocked_urls = idx$robots_blocked_urls,
    noindex_urls = idx$noindex_urls,
    used_waste_sink = used_waste_sink
  )
}

#' Detect keep/exclude filter values folded out of scope.
#'
#' Returns the subset of the supplied domain / host filter values that
#' classified one or more crawled (pre-fold) nodes but no surviving post-fold
#' node -- i.e. an out-of-scope canonical/redirect fold rewrote the crawled
#' domain/host away before the domain filter ran. Presence is tested by
#' delegating to [filter_links_by_domain()] in `return_report` mode on a
#' self-loop edge list of the node set: a self-loop row survives a single-value
#' keep filter iff that node is classified on the named domain/host, so this
#' reuses the filter's own extraction + resolution (same rurl profile, same
#' PSL) rather than re-parsing hosts here.
#' @keywords internal
#' @noRd
.sf_folded_away_filter_values <- function(
  prefold_nodes,
  postfold_nodes,
  domain_values,
  host_values,
  rurl_params
) {
  domain_values <- .sf_faf_keep_present(domain_values)
  host_values <- .sf_faf_keep_present(host_values)

  folded_away <- c(
    .sf_faf_collect(
      domain_values,
      prefold_nodes,
      postfold_nodes,
      "domain",
      rurl_params
    ),
    .sf_faf_collect(
      host_values,
      prefold_nodes,
      postfold_nodes,
      "host",
      rurl_params
    )
  )
  unique(folded_away)
}

#' Drop NA/empty filter values and de-duplicate.
#' @keywords internal
#' @noRd
.sf_faf_keep_present <- function(values) {
  values <- values[!is.na(values) & nzchar(values)]
  unique(values)
}

#' Test whether a domain/host filter value classifies any of `nodes`.
#'
#' Probes [filter_links_by_domain()] in `return_report` mode on a self-loop
#' edge list of the node set: a self-loop row survives a single-value keep
#' filter iff that node is classified on the named domain/host.
#' @keywords internal
#' @noRd
.sf_faf_present <- function(nodes, value, type, rurl_params) {
  if (length(nodes) == 0) {
    return(FALSE)
  }
  probe_df <- data.frame(
    from = nodes,
    to = nodes,
    stringsAsFactors = FALSE
  )
  args <- list(
    edge_list_df = probe_df,
    return_report = TRUE,
    rurl_params = rurl_params
  )
  if (identical(type, "domain")) {
    args$keep_domains <- value
  } else {
    args$keep_hosts <- value
  }
  report <- do.call(filter_links_by_domain, args)$report
  isTRUE(report$rows_after > 0)
}

#' Collect filter values present pre-fold but folded away post-fold.
#' @keywords internal
#' @noRd
.sf_faf_collect <- function(
  values,
  prefold_nodes,
  postfold_nodes,
  type,
  rurl_params
) {
  folded_away <- character(0)
  for (value in values) {
    if (!.sf_faf_present(prefold_nodes, value, type, rurl_params)) {
      next
    }
    if (.sf_faf_present(postfold_nodes, value, type, rurl_params)) {
      next
    }
    folded_away <- c(folded_away, value)
  }
  folded_away
}

#' Apply pagerank() duplicate-edge policy.
#' @keywords internal
#' @noRd
.apply_duplicate_edge_policy <- function(
  edge_list_df,
  policy,
  self_loops,
  from_col,
  to_col
) {
  if (policy == "collapse") {
    return(get_unique_edges(
      edge_list_df = edge_list_df,
      self_loops = self_loops,
      from_col = from_col,
      to_col = to_col
    ))
  }

  if (
    policy == "count_instances" &&
      nrow(edge_list_df) > 0 &&
      all(c(from_col, to_col) %in% names(edge_list_df))
  ) {
    from <- as.character(edge_list_df[[from_col]])
    to <- as.character(edge_list_df[[to_col]])
    valid <- !is.na(from) & !is.na(to)
    if (self_loops == "drop") {
      valid <- valid & from != to
    }

    instance_count <- integer(nrow(edge_list_df))
    instance_count[valid] <- 1L
    edge_list_df[["__pr_instance_count__"]] <- instance_count
  }

  aggregate_edges(
    edge_list_df = edge_list_df,
    self_loops = self_loops,
    from_col = from_col,
    to_col = to_col
  )
}

#' Build compact duplicate-edge audit rows for counted mode.
#' @keywords internal
#' @noRd
.duplicate_edge_audit_rows <- function(
  edge_list_df,
  from_col,
  to_col,
  instance_count_col,
  weight_col
) {
  if (
    is.null(instance_count_col) ||
      !(instance_count_col %in% names(edge_list_df)) ||
      nrow(edge_list_df) == 0
  ) {
    return(NULL)
  }

  instance_count <- suppressWarnings(
    as.integer(edge_list_df[[instance_count_col]])
  )
  duplicated_edges <- !is.na(instance_count) & instance_count > 1L
  if (!any(duplicated_edges)) {
    return(data.frame(
      from = character(0),
      to = character(0),
      instance_count = integer(0)
    ))
  }

  audit <- data.frame(
    from = as.character(edge_list_df[[from_col]][duplicated_edges]),
    to = as.character(edge_list_df[[to_col]][duplicated_edges]),
    instance_count = instance_count[duplicated_edges]
  )

  if (!is.null(weight_col) && weight_col %in% names(edge_list_df)) {
    audit[["effective_weight"]] <- suppressWarnings(
      as.numeric(edge_list_df[[weight_col]][duplicated_edges])
    )
  }

  audit
}

#' Validate the arguments passed to pagerank().
#'
#' Front-loads all of `pagerank()`'s argument checks (type / length / range,
#' required columns, and the reverse-mode compatibility guards). Errors carry
#' `call. = FALSE`, so their messages are identical to when the checks lived
#' inline. Returns `invisible(NULL)`; called purely for its side effect of
#' erroring on invalid input. `nofollow_action` is expected already
#' `match.arg()`-normalized by the caller.
#' @keywords internal
#' @noRd
.validate_pagerank_args <- function(
  edge_list_df,
  redirects_df,
  canonicals_df,
  clean_canonical_urls,
  canonical_from_col,
  canonical_to_col,
  clean_edge_urls,
  clean_redirect_urls,
  rurl_params,
  drop_isolates_flag,
  reverse,
  damping,
  weight_col,
  nofollow_col,
  nofollow_action,
  indexability_df,
  indexability_url_col,
  indexability_status_col,
  status_df,
  status_url_col,
  status_col,
  prior_df,
  prior_url_col,
  prior_weight_col,
  prior_alpha,
  prior_inject_unmatched,
  prior_exclude_waste
) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  # Further column checks within functions called.
  .assert_df_or_null(redirects_df, "redirects_df")
  .assert_df_or_null(canonicals_df, "canonicals_df")
  .assert_flag(clean_canonical_urls, "clean_canonical_urls")
  .validate_canonical_cols(canonicals_df, canonical_from_col, canonical_to_col)
  .assert_flag(clean_edge_urls, "clean_edge_urls")
  .assert_flag(clean_redirect_urls, "clean_redirect_urls")
  if (!is.list(rurl_params)) {
    stop("`rurl_params` must be a list.", call. = FALSE)
  }
  .assert_flag(drop_isolates_flag, "drop_isolates_flag")
  .assert_flag(reverse, "reverse", allow_na = FALSE)
  .assert_unit_interval(damping, "damping", "numeric value")

  .validate_reverse_guards(
    reverse,
    indexability_df,
    nofollow_col,
    nofollow_action,
    status_df
  )

  .assert_col_or_null(weight_col, "weight_col", edge_list_df)
  .assert_col_or_null(nofollow_col, "nofollow_col", edge_list_df)
  .validate_indexability_df(
    indexability_df,
    indexability_url_col,
    indexability_status_col
  )
  .validate_status_df(status_df, status_url_col, status_col)
  .validate_prior_df(prior_df, prior_url_col, prior_weight_col)
  .assert_unit_interval(prior_alpha, "prior_alpha", "number")
  .assert_flag(prior_inject_unmatched, "prior_inject_unmatched")
  .assert_flag(prior_exclude_waste, "prior_exclude_waste", allow_na = FALSE)

  invisible(NULL)
}

#' Error unless `x` is a single logical value (NA allowed unless `allow_na`).
#' @keywords internal
#' @noRd
.assert_flag <- function(x, name, allow_na = TRUE) {
  if (!is.logical(x) || length(x) != 1 || (!allow_na && is.na(x))) {
    stop("`", name, "` must be a single logical value.", call. = FALSE)
  }
  invisible(NULL)
}

#' Error unless `x` is NULL or a data frame.
#' @keywords internal
#' @noRd
.assert_df_or_null <- function(x, name) {
  if (!is.null(x) && !is.data.frame(x)) {
    stop("`", name, "` must be a data frame or NULL.", call. = FALSE)
  }
  invisible(NULL)
}

#' Error unless `x` is a single number in `[0, 1]`. `word` tunes the message
#' ("numeric value" for `damping`, "number" for `prior_alpha`).
#' @keywords internal
#' @noRd
.assert_unit_interval <- function(x, name, word) {
  # Sequential checks (rather than one `||` chain) keep the short-circuit order
  # and message identical while staying low-complexity.
  bad <- function() {
    stop(
      "`",
      name,
      "` must be a single ",
      word,
      " between 0 and 1.",
      call. = FALSE
    )
  }
  if (!is.numeric(x)) {
    bad()
  }
  if (length(x) != 1) {
    bad()
  }
  if (is.na(x)) {
    bad()
  }
  if (x < 0) {
    bad()
  }
  if (x > 1) {
    bad()
  }
  invisible(NULL)
}

#' Error unless `x` is NULL or a single character string naming a column present
#' in `edge_list_df` (the latter only when the frame has rows).
#' @keywords internal
#' @noRd
.assert_col_or_null <- function(x, name, edge_list_df) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (!is.character(x) || length(x) != 1) {
    stop(
      "`",
      name,
      "` must be a single character string or NULL.",
      call. = FALSE
    )
  }
  if (nrow(edge_list_df) > 0 && !(x %in% names(edge_list_df))) {
    stop("`", name, "` '", x, "' not found in `edge_list_df`.", call. = FALSE)
  }
  invisible(NULL)
}

#' Error unless `canonicals_df` (when non-empty) has the declared from/to cols.
#' @keywords internal
#' @noRd
.validate_canonical_cols <- function(
  canonicals_df,
  canonical_from_col,
  canonical_to_col
) {
  canonical_cols <- c(canonical_from_col, canonical_to_col)
  if (
    !is.null(canonicals_df) &&
      nrow(canonicals_df) > 0 &&
      !all(canonical_cols %in% names(canonicals_df))
  ) {
    stop(
      "`canonicals_df` must have '",
      canonical_from_col,
      "' and '",
      canonical_to_col,
      "' columns.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Reverse / inverse PageRank (CheiRank) compatibility guards.
#'
#' Reversal transposes only the link graph. Features whose semantics depend on
#' the *direction of authority flow* do not transpose cleanly and are rejected
#' rather than silently producing misleading scores:
#'   * nofollow "evaporate": the sink device models the SOURCE wasting its
#'     outgoing budget (a forward concept); reversed, the sink would inject rank
#'     instead. Use "drop" (the correct CheiRank treatment: a nofollowed link
#'     funnels no authority outward) or "keep".
#'   * indexability: noindex and robots-blocked both route the page's outgoing
#'     budget to the waste sink, encoding forward crawl/index behavior with no
#'     meaningful transpose.
#' Direction-agnostic features (cleaning, redirect folding, dedup, weights,
#' domain/host filtering, TIPR prior) remain fully supported under reverse.
#' @keywords internal
#' @noRd
.validate_reverse_guards <- function(
  reverse,
  indexability_df,
  nofollow_col,
  nofollow_action,
  status_df = NULL
) {
  if (!isTRUE(reverse)) {
    return(invisible(NULL))
  }
  if (!is.null(indexability_df) && nrow(indexability_df) > 0) {
    stop(
      "`indexability_df` is not supported with `reverse = TRUE`: noindex ",
      "and robots.txt handling encode forward crawl semantics that do not ",
      "transpose. Drop `indexability_df` or set `reverse = FALSE`.",
      call. = FALSE
    )
  }
  if (!is.null(status_df) && nrow(status_df) > 0) {
    stop(
      "`status_df` is not supported with `reverse = TRUE`: a response-dead ",
      "page collecting authority it cannot pass on encodes forward crawl ",
      "semantics that do not transpose. Drop `status_df` or set ",
      "`reverse = FALSE`.",
      call. = FALSE
    )
  }
  if (!is.null(nofollow_col) && nofollow_action == "evaporate") {
    stop(
      "`nofollow_action = \"evaporate\"` is not supported with ",
      "`reverse = TRUE`: the evaporation sink models a source wasting its ",
      "outgoing budget and does not transpose. Use `nofollow_action = ",
      "\"drop\"` (a nofollowed link funnels no authority outward) or ",
      "\"keep\", or set `reverse = FALSE`.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Validate `indexability_df` and its url/status column names.
#' @keywords internal
#' @noRd
.validate_indexability_df <- function(
  indexability_df,
  indexability_url_col,
  indexability_status_col
) {
  if (is.null(indexability_df)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(indexability_df)) {
    stop("`indexability_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (nrow(indexability_df) > 0) {
    if (!(indexability_url_col %in% names(indexability_df))) {
      stop(
        "`indexability_url_col` '",
        indexability_url_col,
        "' not found in `indexability_df`.",
        call. = FALSE
      )
    }
    if (!(indexability_status_col %in% names(indexability_df))) {
      stop(
        "`indexability_status_col` '",
        indexability_status_col,
        "' not found in `indexability_df`.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Validate `status_df` and its url/status column names.
#'
#' Mirrors [.validate_indexability_df]: shape check plus presence of the two
#' named columns. The status column is not coerced here — parsing to integer
#' and 400:599 class membership happen in [.classify_status_dead]; a column of
#' non-numeric strings is a data problem surfaced there, not a contract error.
#' @keywords internal
#' @noRd
.validate_status_df <- function(status_df, status_url_col, status_col) {
  if (is.null(status_df)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(status_df)) {
    stop("`status_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (nrow(status_df) > 0) {
    if (!(status_url_col %in% names(status_df))) {
      stop(
        "`status_url_col` '",
        status_url_col,
        "' not found in `status_df`.",
        call. = FALSE
      )
    }
    if (!(status_col %in% names(status_df))) {
      stop(
        "`status_col` '",
        status_col,
        "' not found in `status_df`.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Identify response-dead URLs (HTTP 4xx/5xx) present in the graph.
#'
#' Parses `status_df[[status_col]]` to integer and returns the subset of
#' `status_df[[status_url_col]]` whose code is in `400:599` AND that appears as
#' a vertex in `vertex_universe`. Restricting to actual vertices keeps phantom
#' rows (URLs listed in the status export but never linked in the graph) out of
#' the count and out of any downstream flow treatment. 4xx and 5xx are one
#' class (no split); 3xx and codes below 400 are not dead; codes that cannot be
#' parsed as an integer are treated as live.
#' @keywords internal
#' @noRd
.classify_status_dead <- function(
  status_df,
  status_url_col,
  status_col,
  vertex_universe
) {
  if (is.null(status_df) || nrow(status_df) == 0) {
    return(character(0))
  }
  codes <- suppressWarnings(as.integer(as.character(status_df[[status_col]])))
  urls <- as.character(status_df[[status_url_col]])
  is_dead <- !is.na(codes) & codes >= 400L & codes <= 599L
  dead_urls <- unique(urls[is_dead])
  intersect(dead_urls, vertex_universe)
}

#' Validate the TIPR `prior_df` and its url/weight column names.
#' @keywords internal
#' @noRd
.validate_prior_df <- function(prior_df, prior_url_col, prior_weight_col) {
  if (is.null(prior_df)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(prior_df)) {
    stop("`prior_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (nrow(prior_df) > 0) {
    if (!(prior_url_col %in% names(prior_df))) {
      stop(
        "`prior_url_col` '",
        prior_url_col,
        "' not found in `prior_df`.",
        call. = FALSE
      )
    }
    if (!(prior_weight_col %in% names(prior_df))) {
      stop(
        "`prior_weight_col` '",
        prior_weight_col,
        "' not found in `prior_df`.",
        call. = FALSE
      )
    }
    if (!is.numeric(prior_df[[prior_weight_col]])) {
      stop(
        "`prior_weight_col` '",
        prior_weight_col,
        "' must be a numeric column.",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Build synthetic self-loop rows for a sink node.
#'
#' Constructs one `from == to == node` row per element of `nodes` (via
#' [.make_synthetic_rows]), matching the column structure of `edge_list_df`.
#' Used for the absorbing waste-sink and leak-sink self-loops.
#' @keywords internal
#' @noRd
.make_sink_rows <- function(edge_list_df, from_col, to_col, nodes) {
  .make_synthetic_rows(edge_list_df, from_col, to_col, nodes, nodes)
}

#' Build synthetic edge rows with an explicit from/to pairing.
#'
#' Generalizes [.make_sink_rows]: constructs one row per `from_nodes[i] ->
#' to_nodes[i]` pair (recycled the usual R way), matching the column structure
#' of `edge_list_df`. Extra columns beyond the from/to pair are filled with
#' type-appropriate neutral defaults (`FALSE` for logical, `1` for numeric, `NA`
#' otherwise) so the rows `rbind()` cleanly. Used both for self-loop sink rows
#' (`to_nodes == from_nodes`) and for the class members' single edge to the
#' waste sink (`to_nodes == waste_sink_name`).
#' @keywords internal
#' @noRd
.make_synthetic_rows <- function(edge_list_df, from_col, to_col,
                                 from_nodes, to_nodes) {
  rows <- stats::setNames(
    data.frame(from_nodes, to_nodes, stringsAsFactors = FALSE),
    c(from_col, to_col)
  )
  extra_cols <- setdiff(names(edge_list_df), c(from_col, to_col))
  for (col in extra_cols) {
    rows[[col]] <- if (is.logical(edge_list_df[[col]])) {
      FALSE
    } else if (is.numeric(edge_list_df[[col]])) {
      1
    } else {
      NA
    }
  }
  rows
}

#' Detect fold-target collisions and warn (SF-scope / PAGE-rjrduvmy).
#'
#' Computed on the PRE-fold edge endpoints (`edge_list_df`), using the fold map
#' exactly as it will be applied. A fold entry `source -> target` COLLIDES when
#' the relabel silently merges the crawled source's node with a node that
#' already carries genuine, INDEPENDENT inbound links to `target`, inflating its
#' PageRank invisibly:
#'   (1) `target` is a pure link-target -- it appears ONLY as a `to`, never as a
#'       `from`, AND is NOT a known crawled URL (absent from the crawl table
#'       `indexability_df`). This second clause separates the harmful case (an
#'       UNCRAWLED canonical/redirect target, e.g. a production URL a staged
#'       page folds onto) from a benign fold onto a genuinely crawled LEAF
#'       page (a real 200 page that simply has no outlinks -- it appears only
#'       as a `to` too, but IS in the crawl table, so it is not flagged); AND
#'   (2) `target` is independently referenced -- it is the `to` of >=1 edge
#'       whose `from` is NOT itself a source folding onto `target`; AND
#'   (3) >=1 source folding onto `target` is an actual pre-fold edge endpoint,
#'       so the relabel genuinely merges a crawled node onto it.
#' A normal redirect/canonical onto a genuinely crawled target (a `from`, or a
#' leaf listed in `indexability_df`) is NOT flagged -- correct merge.
#'
#' b2 fallback: this diagnostic REQUIRES crawl-URL knowledge. Without
#' `indexability_df` there is no way to tell a crawled leaf page from an
#' uncrawled fold target (they are identical in the edge list), so detection is
#' skipped entirely and the function returns NULL.
#'
#' @return A data frame of colliding targets (`target`, `n_independent_refs`,
#'   `source`) and emits a `warning()`, or `NULL` when there are none / the
#'   diagnostic is unavailable.
#' @keywords internal
#' @noRd
.detect_fold_collisions <- function(
  fold_map,
  edge_list_df,
  prefold_nodes,
  indexability_df,
  indexability_url_col,
  clean_edge_urls,
  effective_rurl_params,
  from_col,
  to_col
) {
  have_crawl_urls <- !is.null(indexability_df) && nrow(indexability_df) > 0
  if (!have_crawl_urls) {
    return(NULL)
  }

  # Known crawled URLs, canonicalized into the SAME namespace as the edges /
  # fold targets (see .detect_crawl_urls).
  crawl_urls <- .detect_crawl_urls(
    indexability_df = indexability_df,
    indexability_url_col = indexability_url_col,
    clean_edge_urls = clean_edge_urls,
    effective_rurl_params = effective_rurl_params
  )

  prefold_from <- as.character(edge_list_df[[from_col]])
  prefold_to <- as.character(edge_list_df[[to_col]])

  # Scan fold targets for silent merges (see .collect_fold_collisions).
  coll <- .collect_fold_collisions(
    fold_map = fold_map,
    prefold_from = prefold_from,
    prefold_to = prefold_to,
    prefold_nodes = prefold_nodes,
    crawl_urls = crawl_urls
  )

  if (length(coll$targets) == 0) {
    return(NULL)
  }

  warning(
    "Fold-target collision: canonical/redirect folding relabeled ",
    "crawled page(s) onto uncrawled URL(s) that are ALSO ",
    "independently linked, silently merging their inbound link equity ",
    "and inflating PageRank: ",
    paste0("`", coll$targets, "`", collapse = ", "),
    ". Inspect `attr(result, \"transition_audit\")$fold$collisions`.",
    call. = FALSE
  )
  data.frame(
    target = coll$targets,
    n_independent_refs = coll$nrefs,
    source = coll$sources,
    stringsAsFactors = FALSE
  )
}

#' Canonicalize known crawled URLs into the edge namespace.
#'
#' Indexability URLs are not cleaned elsewhere, so clean them here through the
#' same resolved rurl profile when edge cleaning is on, then drop NA/duplicates.
#' @keywords internal
#' @noRd
.detect_crawl_urls <- function(
  indexability_df,
  indexability_url_col,
  clean_edge_urls,
  effective_rurl_params
) {
  crawl_urls <- as.character(indexability_df[[indexability_url_col]])
  if (clean_edge_urls) {
    idx_tmp <- data.frame(u = crawl_urls, stringsAsFactors = FALSE)
    idx_tmp <- do.call(
      clean_url_columns,
      c(list(data_frame = idx_tmp, columns = "u"), effective_rurl_params)
    )
    crawl_urls <- as.character(idx_tmp$u)
  }
  unique(stats::na.omit(crawl_urls))
}

#' Scan fold targets for silent inbound-equity merges.
#'
#' A collision is an out-of-crawl fold target (never a `from`, not a crawled
#' URL) that at least one real crawled source folds onto AND that is also
#' independently linked from a page outside that source set. Returns parallel
#' `targets` / `nrefs` / `sources` vectors (empty when there are none).
#' @keywords internal
#' @noRd
.collect_fold_collisions <- function(
  fold_map,
  prefold_from,
  prefold_to,
  prefold_nodes,
  crawl_urls
) {
  fold_sources <- names(fold_map)
  fold_targets <- unname(fold_map)

  targets <- character(0)
  nrefs <- integer(0)
  sources <- character(0)

  # Candidate targets: pure link-targets (never a `from`) that are ALSO not
  # known crawled URLs.
  cand_targets <- unique(fold_targets[
    !(fold_targets %in% prefold_from) & !(fold_targets %in% crawl_urls)
  ])
  for (tgt in cand_targets) {
    srcs <- fold_sources[fold_targets == tgt]
    # A merge only happens if >=1 folding source is a real crawled node.
    if (!any(srcs %in% prefold_nodes)) {
      next
    }
    # Independent inbound references: edges to `tgt` from a page that is not one
    # of the sources folding onto `tgt`.
    indep <- prefold_to == tgt & !(prefold_from %in% srcs)
    n_indep <- sum(indep, na.rm = TRUE)
    if (n_indep > 0L) {
      targets <- c(targets, tgt)
      nrefs <- c(nrefs, as.integer(n_indep))
      sources <- c(
        sources,
        toString(unique(srcs[srcs %in% prefold_nodes]))
      )
    }
  }

  list(targets = targets, nrefs = nrefs, sources = sources)
}

#' Classify indexability directives into noindex / robots-blocked URL sets.
#'
#' Detection only: parses `indexability_df` statuses and returns the URLs
#' declared noindex or robots.txt-blocked, with robots.txt taking priority (a
#' page that is both is treated as robots-blocked, never noindex). Flow
#' treatment — routing the whole "collects PR but cannot pass it" class through
#' the waste sink — happens uniformly in [.route_waste_class], so this no longer
#' mutates edges or the nofollow column.
#'
#' @return A list with `noindex_urls` and `robots_blocked_urls` character
#'   vectors (both empty when no `indexability_df` was supplied).
#' @keywords internal
#' @noRd
.classify_indexability <- function(
  indexability_df,
  indexability_url_col,
  indexability_status_col
) {
  if (is.null(indexability_df) || nrow(indexability_df) == 0) {
    return(list(
      noindex_urls = character(0),
      robots_blocked_urls = character(0)
    ))
  }
  statuses <- as.character(indexability_df[[indexability_status_col]])
  urls <- as.character(indexability_df[[indexability_url_col]])

  # robots.txt takes priority over noindex.
  is_robots_blocked <- grepl("Blocked by robots.txt", statuses, fixed = TRUE)
  is_noindex <- grepl("noindex", statuses, ignore.case = TRUE) &
    !is_robots_blocked

  list(
    noindex_urls = unique(urls[is_noindex]),
    robots_blocked_urls = unique(urls[is_robots_blocked])
  )
}

#' Route the whole waste class through a single edge to the shared sink.
#'
#' Every member of the "collects PageRank but cannot pass it" class (noindex,
#' robots-blocked, response-dead 4xx/5xx) loses ALL of its outgoing edges and
#' gains EXACTLY ONE `member -> sink` edge. For a page with real outlinks
#' (noindex, robots-blocked) this REPLACES them; for a page with none (a 404)
#' it ADDS one, which is what stops a no-outlink dead page from dangling and
#' recycling its inbound authority to every page via teleport. Only members
#' still present as a real node (an edge endpoint after the strip, or in
#' `all_vertex_universe`) get the sink edge, so a class URL that never appears
#' in the graph introduces no phantom node. The absorbing sink self-loop is
#' added once by the caller.
#'
#' @return A list with `edge_list_df` (mutated) and `used_sink` (whether any
#'   member -> sink edge was added).
#' @keywords internal
#' @noRd
.route_waste_class <- function(
  edge_list_df,
  class_urls,
  all_vertex_universe,
  waste_sink_name,
  from_col,
  to_col
) {
  if (length(class_urls) == 0) {
    return(list(edge_list_df = edge_list_df, used_sink = FALSE))
  }
  # Strip every outgoing edge from a class member.
  if (nrow(edge_list_df) > 0) {
    from_in_class <- edge_list_df[[from_col]] %in% class_urls
    if (any(from_in_class)) {
      edge_list_df <- edge_list_df[!from_in_class, , drop = FALSE]
    }
  }
  # Exactly one member -> sink edge per class member that is a real node.
  members_present <- unique(class_urls[
    class_urls %in% edge_list_df[[from_col]] |
      class_urls %in% edge_list_df[[to_col]] |
      class_urls %in% all_vertex_universe
  ])
  if (length(members_present) == 0) {
    return(list(edge_list_df = edge_list_df, used_sink = FALSE))
  }
  sink_edges <- .make_synthetic_rows(
    edge_list_df,
    from_col,
    to_col,
    from_nodes = members_present,
    to_nodes = rep(waste_sink_name, length(members_present))
  )
  list(
    edge_list_df = rbind(edge_list_df, sink_edges),
    used_sink = TRUE
  )
}

#' Tag each reported page with its page_state and wasted_mass (per-URL waste
#' attribution).
#'
#' Appends two columns to the visible results, present only when
#' `indexability_df` or `status_df` was supplied (mirroring how `prior_weight`
#' appears only with `prior_df`):
#'
#' * `page_state` ∈ {`live`, `noindex`, `robots_blocked`, `response_dead`} —
#'   the page's health/indexability state. Precedence, highest first:
#'   robots_blocked > response_dead > noindex > live.
#' * `wasted_mass` — the share of the shared waste sink's stationary mass this
#'   page is responsible for: the authority it collected and black-holed. A
#'   waste-class member routes its entire throughput through one edge to the
#'   absorbing sink, so at convergence the mass it evaporates is
#'   `damping / (1 - damping)` times its own reported score — larger than, and
#'   distinct from, that score. Summed over the class this equals the evaporated
#'   mass in the transition audit (`mass$sink`). A live page routes nothing to
#'   the sink, so its `wasted_mass` is `0`.
#'
#' Called after the internal / vanished nodes are stripped, so the sink and any
#' vanished robots pages are never tagged.
#' @keywords internal
#' @noRd
.attach_page_state <- function(
  pagerank_results,
  indexability_df,
  status_df,
  noindex_urls,
  robots_blocked_urls,
  status_dead_urls,
  damping
) {
  has_idx <- !is.null(indexability_df) && nrow(indexability_df) > 0
  has_status <- !is.null(status_df) && nrow(status_df) > 0
  if ((!has_idx && !has_status) || nrow(pagerank_results) == 0) {
    return(pagerank_results)
  }
  nodes <- pagerank_results[[1]]
  score <- pagerank_results[[2]]
  state <- rep("live", length(nodes))
  # Lowest precedence first so higher-precedence assignments win.
  state[nodes %in% noindex_urls] <- "noindex"
  state[nodes %in% status_dead_urls] <- "response_dead"
  state[nodes %in% robots_blocked_urls] <- "robots_blocked"
  pagerank_results[["page_state"]] <- state

  # Per-URL wasted mass. `damping == 1` is degenerate (no teleport); the
  # attribution factor is undefined there, so the class is reported as NA.
  waste_factor <- if (damping < 1) damping / (1 - damping) else NA_real_
  is_waste <- state %in% c("noindex", "robots_blocked", "response_dead")
  wasted <- rep(0, length(nodes))
  wasted[is_waste] <- waste_factor * score[is_waste]
  pagerank_results[["wasted_mass"]] <- wasted

  pagerank_results
}

#' Apply nofollow handling to real rel=nofollow edges.
#'
#' Drops, evaporates (retargets onto the shared waste sink), or keeps nofollowed
#' edges per `nofollow_action`. The nofollow column is coerced to logical (0/1
#' numeric accepted); `NA` is treated as not-nofollow. The absorbing sink
#' self-loop is added once by the caller, so this only signals whether the sink
#' was used.
#'
#' @return A list with `edge_list_df` (mutated) and `used_waste_sink`
#'   (whether any edge was retargeted to the sink).
#' @keywords internal
#' @noRd
.apply_nofollow <- function(
  edge_list_df,
  nofollow_col,
  nofollow_action,
  waste_sink_name,
  from_col,
  to_col
) {
  used_waste_sink <- FALSE

  # No-op when there is no usable nofollow column.
  if (
    is.null(nofollow_col) ||
      !(nofollow_col %in% names(edge_list_df)) ||
      nrow(edge_list_df) == 0
  ) {
    return(list(edge_list_df = edge_list_df, used_waste_sink = FALSE))
  }

  # Coerce nofollow column to logical
  nf_vals <- edge_list_df[[nofollow_col]]
  if (is.numeric(nf_vals)) {
    nf_vals <- as.logical(nf_vals)
  }
  nf_mask <- !is.na(nf_vals) & nf_vals

  if (any(nf_mask)) {
    if (nofollow_action == "drop") {
      # Simply remove nofollow edges
      edge_list_df <- edge_list_df[!nf_mask, , drop = FALSE]
    } else if (nofollow_action == "evaporate") {
      # Retarget nofollow edges to the shared waste sink.
      edge_list_df[[to_col]][nf_mask] <- waste_sink_name
      used_waste_sink <- TRUE
    }
    # nofollow_action == "keep": do nothing
  }

  list(edge_list_df = edge_list_df, used_waste_sink = used_waste_sink)
}

#' Clean the edge / redirect / canonical URL columns through one rurl profile.
#'
#' `rurl::get_clean_url` memoizes parses internally and the cache is shared
#' across calls, so URLs common to the edge and redirect/canonical lists are
#' canonicalized once per unique string without any local memoizer. Canonicals
#' are cleaned through the SAME resolved profile so the composed fold map
#' operates in one node namespace. When edge cleaning is disabled and edge URLs
#' still contain query parameters, emits the same advisory warning as before --
#' but only when `warn_uncleaned_edges` is `TRUE` (pagerank()); hits()/salsa()
#' share this spine and suppress the advisory to preserve their behavior.
#'
#' @return A list with the (possibly cleaned) `edge_list_df`, `redirects_df`,
#'   and `canonicals_df`.
#' @keywords internal
#' @noRd
.clean_pipeline_urls <- function(
  edge_list_df,
  redirects_df,
  canonicals_df,
  edge_from_col,
  edge_to_col,
  redirect_from_col,
  redirect_to_col,
  canonical_from_col,
  canonical_to_col,
  clean_edge_urls,
  clean_redirect_urls,
  clean_canonical_urls,
  effective_rurl_params,
  warn_uncleaned_edges = TRUE
) {
  # Determine edge, redirect, and canonical URL columns for cleaning
  edge_url_cols <- intersect(c(edge_from_col, edge_to_col), names(edge_list_df))
  redirect_url_cols <- .url_cols_of(
    redirects_df,
    redirect_from_col,
    redirect_to_col
  )
  canonical_url_cols <- .url_cols_of(
    canonicals_df,
    canonical_from_col,
    canonical_to_col
  )

  # Edge cleaning has no data-frame emptiness guard (edge_list_df is always a
  # data frame); redirect/canonical cleaning additionally require a non-empty
  # frame, matching the original inline conditions.
  edge_list_df <- .clean_cols_if(
    edge_list_df,
    edge_url_cols,
    clean_edge_urls,
    effective_rurl_params
  )
  redirects_df <- .clean_cols_if(
    redirects_df,
    redirect_url_cols,
    clean_redirect_urls && .df_has_rows(redirects_df),
    effective_rurl_params
  )
  canonicals_df <- .clean_cols_if(
    canonicals_df,
    canonical_url_cols,
    clean_canonical_urls && .df_has_rows(canonicals_df),
    effective_rurl_params
  )

  # The uncleaned-edge advisory is a pagerank() diagnostic; hits()/salsa() share
  # the cleaning spine but have never emitted it, so they pass FALSE to preserve
  # their behavior.
  if (warn_uncleaned_edges) {
    .warn_uncleaned_query_params(edge_list_df, edge_url_cols, clean_edge_urls)
  }

  list(
    edge_list_df = edge_list_df,
    redirects_df = redirects_df,
    canonicals_df = canonicals_df
  )
}

#' URL columns present in `df` among the declared from/to pair (empty if NULL).
#' @keywords internal
#' @noRd
.url_cols_of <- function(df, from_col, to_col) {
  if (is.null(df)) {
    return(character(0))
  }
  intersect(c(from_col, to_col), names(df))
}

#' TRUE when `df` is a non-NULL data frame with at least one row.
#' @keywords internal
#' @noRd
.df_has_rows <- function(df) {
  !is.null(df) && nrow(df) > 0
}

#' Clean `cols` of `df` through the resolved rurl profile when `should` and the
#' columns are present. Returns `df` unchanged otherwise.
#' @keywords internal
#' @noRd
.clean_cols_if <- function(df, cols, should, effective_rurl_params) {
  if (should && length(cols) > 0) {
    df <- do.call(
      clean_url_columns,
      c(list(data_frame = df, columns = cols), effective_rurl_params)
    )
  }
  df
}

#' Warn when edge cleaning is disabled but edge URLs still carry query params.
#' @keywords internal
#' @noRd
.warn_uncleaned_query_params <- function(
  edge_list_df,
  edge_url_cols,
  clean_edge_urls
) {
  if (
    !clean_edge_urls &&
      length(edge_url_cols) > 0 &&
      .urls_contain_query_params(edge_list_df, columns = edge_url_cols)
  ) {
    warning(
      "URLs in `edge_list_df` may contain query parameters ",
      "(e.g. '?' or '&'). Consider setting `clean_edge_urls = TRUE` ",
      "for consistent PageRank calculation, using `rurl_params` to ",
      "control `rurl::clean_url` behavior if needed.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Build and apply the composed redirect + canonical fold map.
#'
#' Composes the single fold map from both signals (empty when neither is
#' supplied; the plain redirect map when canonicals are absent), records which
#' signal materially folded an edge, classifies out-of-scope folds (targets that
#' are not crawled nodes) and applies the `out_of_scope_fold` policy (`relabel`
#' keeps the full map; `keep` and `leak` drop the out-of-scope entries, and
#' `leak` records the sources so the caller can route them onto the leak sink),
#' runs fold-target collision detection, then applies the map to the edge
#' endpoints. `fold_map` (after any keep/leak dropping) remains the source of
#' truth for TIPR prior folding.
#'
#' @return A list with the folded `edge_list_df`, the applied `fold_map`, the
#'   `audit_has_redirects` / `audit_has_canonicals` flags, the out-of-scope
#'   `audit_oos_sources` / `_targets` / `_signals`, `audit_collisions_df`, and
#'   `leak_sources` / `used_leak_sink`.
#' @keywords internal
#' @noRd
.resolve_fold_and_apply <- function(
  edge_list_df,
  redirects_df,
  canonicals_df,
  edge_from_col,
  edge_to_col,
  redirect_from_col,
  redirect_to_col,
  canonical_from_col,
  canonical_to_col,
  duplicate_from_policy,
  loop_handling,
  canonical_duplicate_from_policy,
  canonical_loop_handling,
  canonical_conflict_policy,
  out_of_scope_fold,
  indexability_df,
  indexability_url_col,
  clean_edge_urls,
  effective_rurl_params
) {
  has_redirects <- .df_has_rows(redirects_df)
  has_canonicals <- .df_has_rows(canonicals_df)

  out <- .fold_result_skeleton(edge_list_df)
  if (!has_redirects && !has_canonicals) {
    return(out)
  }

  fold <- .compose_fold_map(
    redirects_df = if (has_redirects) redirects_df else NULL,
    canonicals_df = if (has_canonicals) canonicals_df else NULL,
    redirect_from_col = redirect_from_col,
    redirect_to_col = redirect_to_col,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    duplicate_from_policy = duplicate_from_policy,
    loop_handling = loop_handling,
    canonical_duplicate_from_policy = canonical_duplicate_from_policy,
    canonical_loop_handling = canonical_loop_handling,
    canonical_conflict_policy = canonical_conflict_policy
  )
  fold_map <- fold$map
  out$fold_map <- fold_map

  if (length(fold$signal) > 0) {
    out$audit_has_redirects <- any(fold$signal == "redirect")
    out$audit_has_canonicals <- any(fold$signal == "canonical")
  }

  # Classify out-of-scope folds, detect collisions, and apply the composed map
  # to the edges (see .apply_composed_fold; skipped when the map is empty).
  if (length(fold_map) > 0) {
    out <- .apply_composed_fold(
      out = out,
      fold_map = fold_map,
      fold_signal = fold$signal,
      edge_list_df = edge_list_df,
      out_of_scope_fold = out_of_scope_fold,
      indexability_df = indexability_df,
      indexability_url_col = indexability_url_col,
      clean_edge_urls = clean_edge_urls,
      effective_rurl_params = effective_rurl_params,
      edge_from_col = edge_from_col,
      edge_to_col = edge_to_col
    )
  }
  out
}

#' Empty fold-resolution result skeleton.
#'
#' The default result returned when no redirects/canonicals apply, and the base
#' object that .resolve_fold_and_apply mutates. `edge_list_df` starts as the
#' unmodified input.
#' @keywords internal
#' @noRd
.fold_result_skeleton <- function(edge_list_df) {
  list(
    edge_list_df = edge_list_df,
    fold_map = character(0),
    audit_has_redirects = FALSE,
    audit_has_canonicals = FALSE,
    audit_oos_sources = character(0),
    audit_oos_targets = character(0),
    audit_oos_signals = character(0),
    audit_collisions_df = NULL,
    leak_sources = character(0),
    used_leak_sink = FALSE
  )
}

#' Classify out-of-scope folds, detect collisions, and apply the fold map.
#'
#' Runs on a non-empty composed fold map: captures the pre-fold crawled node
#' set, applies the out_of_scope_fold policy (via .classify_oos_folds), and --
#' when a non-empty map survives -- records fold-target collisions and relabels
#' the edge endpoints through the map. Mutates and returns `out`.
#' @keywords internal
#' @noRd
.apply_composed_fold <- function(
  out,
  fold_map,
  fold_signal,
  edge_list_df,
  out_of_scope_fold,
  indexability_df,
  indexability_url_col,
  clean_edge_urls,
  effective_rurl_params,
  edge_from_col,
  edge_to_col
) {
  # Pre-fold crawled node set: unique, non-NA edge endpoints captured
  # IMMEDIATELY BEFORE the fold is applied. Indexability URLs are NOT part of
  # scope -- the crawled set is edge endpoints only.
  prefold_nodes <- unique(stats::na.omit(c(
    as.character(edge_list_df[[edge_from_col]]),
    as.character(edge_list_df[[edge_to_col]])
  )))

  # Classify out-of-scope entries (fold targets that are not crawled nodes)
  # and apply the out_of_scope_fold policy. See .classify_oos_folds.
  .oos <- .classify_oos_folds(
    fold_map = fold_map,
    fold_signal = fold_signal,
    prefold_nodes = prefold_nodes,
    out_of_scope_fold = out_of_scope_fold
  )
  fold_map <- .oos$fold_map
  out$audit_oos_sources <- .oos$oos_sources
  out$audit_oos_targets <- .oos$oos_targets
  out$audit_oos_signals <- .oos$oos_signals
  out$leak_sources <- .oos$leak_sources
  out$used_leak_sink <- .oos$used_leak_sink

  if (length(fold_map) > 0) {
    out$audit_collisions_df <- .detect_fold_collisions(
      fold_map = fold_map,
      edge_list_df = edge_list_df,
      prefold_nodes = prefold_nodes,
      indexability_df = indexability_df,
      indexability_url_col = indexability_url_col,
      clean_edge_urls = clean_edge_urls,
      effective_rurl_params = effective_rurl_params,
      from_col = edge_from_col,
      to_col = edge_to_col
    )

    for (col_name in c(edge_from_col, edge_to_col)) {
      if (col_name %in% names(edge_list_df)) {
        edge_list_df[[col_name]] <- .apply_fold_map(
          edge_list_df[[col_name]],
          fold_map
        )
      }
    }
  }

  out$edge_list_df <- edge_list_df
  out$fold_map <- fold_map
  out
}

#' Classify out-of-scope fold entries and apply the out_of_scope_fold policy.
#'
#' An out-of-scope entry is a fold-map `source -> target` whose target is not a
#' crawled node (folding it invents a phantom vertex). `relabel` (default) keeps
#' the full map; `keep` drops the out-of-scope entries so crawled sources retain
#' their as-crawled identity; `leak` drops them too but records the sources so
#' the caller can route them onto the leak sink.
#'
#' @return A list with the (possibly trimmed) `fold_map`, the out-of-scope
#'   `oos_sources` / `oos_targets` / `oos_signals`, and `leak_sources` /
#'   `used_leak_sink`.
#' @keywords internal
#' @noRd
.classify_oos_folds <- function(
  fold_map,
  fold_signal,
  prefold_nodes,
  out_of_scope_fold
) {
  out <- list(
    fold_map = fold_map,
    oos_sources = character(0),
    oos_targets = character(0),
    oos_signals = character(0),
    leak_sources = character(0),
    used_leak_sink = FALSE
  )
  oos_mask <- !(unname(fold_map) %in% prefold_nodes)
  if (!any(oos_mask)) {
    return(out)
  }
  out$oos_sources <- names(fold_map)[oos_mask]
  out$oos_targets <- unname(fold_map)[oos_mask]
  out$oos_signals <- unname(fold_signal[out$oos_sources])

  if (identical(out_of_scope_fold, "keep")) {
    out$fold_map <- fold_map[!oos_mask]
  } else if (identical(out_of_scope_fold, "leak")) {
    out$fold_map <- fold_map[!oos_mask]
    out$leak_sources <- out$oos_sources
    out$used_leak_sink <- TRUE
  }
  out
}

#' Apply post-fold domain / host filtering, warning on folded-away values.
#'
#' No-op when no keep/exclude domain or host values are supplied. Otherwise
#' warns when an out-of-scope canonical/redirect fold rewrote a crawled filter
#' value onto a different domain/host (so filtering on the crawled value now
#' matches zero post-fold nodes), then delegates to [filter_links_by_domain()]
#' using the same resolved rurl profile as cleaning.
#'
#' @return The (possibly filtered) edge list data frame.
#' @keywords internal
#' @noRd
.apply_domain_host_filter <- function(
  edge_list_df,
  prefold_nodes,
  keep_domains,
  exclude_domains,
  keep_hosts,
  exclude_hosts,
  edge_from_col,
  edge_to_col,
  effective_rurl_params
) {
  if (
    is.null(keep_domains) &&
      is.null(exclude_domains) &&
      is.null(keep_hosts) &&
      is.null(exclude_hosts)
  ) {
    return(edge_list_df)
  }

  # The post-fold node namespace the filter actually sees (folded edge
  # endpoints, before the filter drops anything). Compared against the pre-fold
  # snapshot so the fold -- not the filter -- is isolated as the cause of a
  # folded-away value.
  postfold_nodes <- unique(stats::na.omit(c(
    as.character(edge_list_df[[edge_from_col]]),
    as.character(edge_list_df[[edge_to_col]])
  )))

  folded_away <- .sf_folded_away_filter_values(
    prefold_nodes = prefold_nodes,
    postfold_nodes = postfold_nodes,
    domain_values = c(keep_domains, exclude_domains),
    host_values = c(keep_hosts, exclude_hosts),
    rurl_params = effective_rurl_params
  )
  if (length(folded_away) > 0) {
    warning(
      "Domain/host filter value(s) ",
      paste0("`", folded_away, "`", collapse = ", "),
      " matched the crawled input but no node after canonical/redirect ",
      "folding. An out-of-scope canonical/redirect fold rewrote the crawled ",
      "node(s) onto a different domain/host BEFORE filtering, so filtering ",
      "on the crawled value now matches zero nodes. Filtering happens AFTER ",
      "folding: to scope the crawled input, run `filter_links_by_domain()` ",
      "on the edge list before `pagerank()`; to scope the folded graph, ",
      "filter on the post-fold (canonical) domain/host instead.",
      call. = FALSE
    )
  }

  filter_links_by_domain(
    edge_list_df = edge_list_df,
    from_col = edge_from_col,
    to_col = edge_to_col,
    keep_domains = keep_domains,
    ignore_domains = exclude_domains,
    keep_hosts = keep_hosts,
    ignore_hosts = exclude_hosts,
    rurl_params = effective_rurl_params
  )
}

#' Route out-of-scope-folded sources onto the leak sink.
#'
#' Under `out_of_scope_fold = "leak"`, a crawled page whose canonical/redirect
#' folds OUT of scope is treated like an external redirect: inbound edges are
#' retargeted onto the leak sink (so their equity reaches the sink and later
#' evaporates), and the source's outbound edges are dropped. The leaked source
#' is removed from the vertex universe (it must not linger as an isolate).
#'
#' No-op unless `used_leak_sink` and at least one leak source.
#' @return A list with the mutated `edge_list_df` and `all_vertex_universe`.
#' @keywords internal
#' @noRd
.route_leak_sources <- function(
  edge_list_df,
  all_vertex_universe,
  used_leak_sink,
  leak_sources,
  leak_sink_name,
  edge_from_col,
  edge_to_col
) {
  if (!used_leak_sink || length(leak_sources) == 0) {
    return(list(
      edge_list_df = edge_list_df,
      all_vertex_universe = all_vertex_universe
    ))
  }
  if (nrow(edge_list_df) > 0) {
    if (edge_to_col %in% names(edge_list_df)) {
      to_leak_mask <- edge_list_df[[edge_to_col]] %in% leak_sources
      edge_list_df[[edge_to_col]][to_leak_mask] <- leak_sink_name
    }
    if (edge_from_col %in% names(edge_list_df)) {
      from_leak_mask <- edge_list_df[[edge_from_col]] %in% leak_sources
      edge_list_df <- edge_list_df[!from_leak_mask, , drop = FALSE]
    }
  }
  all_vertex_universe <- setdiff(all_vertex_universe, leak_sources)
  list(edge_list_df = edge_list_df, all_vertex_universe = all_vertex_universe)
}

#' Prepare the TIPR authority prior in the final vertex namespace.
#'
#' Canonicalizes prior URLs with the SAME rurl profile as the edges (only when
#' edges were cleaned), folds them through the SAME composed map so they land on
#' the same representatives as the vertices, and under `leak` routes the prior
#' of a leaking source onto the leak sink. Summing of coalesced weights happens
#' later in [align_prior_to_vertices()].
#'
#' @return The folded prior data frame, or `NULL` when no prior was supplied.
#' @keywords internal
#' @noRd
.prepare_prior <- function(
  prior_df,
  prior_url_col,
  prior_weight_col,
  clean_edge_urls,
  effective_rurl_params,
  fold_map,
  used_leak_sink,
  leak_sources,
  leak_sink_name
) {
  if (is.null(prior_df) || nrow(prior_df) == 0) {
    return(NULL)
  }

  folded_prior_df <- prior_df[,
    c(prior_url_col, prior_weight_col),
    drop = FALSE
  ]
  folded_prior_df[[prior_url_col]] <-
    as.character(folded_prior_df[[prior_url_col]])

  if (clean_edge_urls) {
    folded_prior_df <- do.call(
      clean_url_columns,
      c(
        list(data_frame = folded_prior_df, columns = prior_url_col),
        effective_rurl_params
      )
    )
  }

  if (length(fold_map) > 0) {
    folded_prior_df[[prior_url_col]] <- .apply_fold_map(
      folded_prior_df[[prior_url_col]],
      fold_map
    )
  }

  if (used_leak_sink && length(leak_sources) > 0) {
    prior_leak_mask <- folded_prior_df[[prior_url_col]] %in% leak_sources
    folded_prior_df[[prior_url_col]][prior_leak_mask] <- leak_sink_name
  }

  folded_prior_df
}

#' Count edge rows dropped at deduplication (NA endpoints / self-loops / dups).
#'
#' Measured against the post-fold/post-filter edge list, mirroring exactly what
#' [get_unique_edges()] removes, so the audit reflects the data that actually
#' reached the deduplication step.
#'
#' @return A list with `n_rows_na`, `n_self_loops`, and `n_rows_duplicate`.
#' @keywords internal
#' @noRd
.count_dropped_edge_rows <- function(
  edge_list_df,
  self_loops,
  edge_from_col,
  edge_to_col
) {
  n_rows_na <- 0L
  n_self_loops <- 0L
  n_rows_duplicate <- 0L
  if (
    nrow(edge_list_df) > 0 &&
      all(c(edge_from_col, edge_to_col) %in% names(edge_list_df))
  ) {
    pre_from <- as.character(edge_list_df[[edge_from_col]])
    pre_to <- as.character(edge_list_df[[edge_to_col]])
    na_mask <- is.na(pre_from) | is.na(pre_to)
    n_rows_na <- sum(na_mask)
    nn_from <- pre_from[!na_mask]
    nn_to <- pre_to[!na_mask]
    self_mask <- nn_from == nn_to
    if (self_loops == "drop") {
      n_self_loops <- sum(self_mask)
      nn_from <- nn_from[!self_mask]
      nn_to <- nn_to[!self_mask]
    }
    if (length(nn_from) > 0) {
      dup_mask <- duplicated(paste0(nn_from, "\t", nn_to))
      n_rows_duplicate <- sum(dup_mask)
    }
  }
  list(
    n_rows_na = n_rows_na,
    n_self_loops = n_self_loops,
    n_rows_duplicate = n_rows_duplicate
  )
}

#' Build the final vertex set (isolate handling + optional prior injection).
#'
#' With `drop_isolates_flag = TRUE` only nodes on a complete edge survive;
#' otherwise the full known universe (original vertices plus any synthetic
#' sink / robots self-loop nodes introduced upstream) is kept. When
#' `prior_inject_unmatched` is TRUE, authoritative prior URLs that fold onto no
#' vertex are surfaced as edge-less isolates carrying their teleport prior.
#'
#' @return A one-column data frame of vertex names, or `NULL` when there are no
#'   vertices.
#' @keywords internal
#' @noRd
.build_vertex_set <- function(
  edge_list_df,
  all_vertex_universe,
  drop_isolates_flag,
  folded_prior_df,
  prior_inject_unmatched,
  prior_url_col,
  node_col_name,
  edge_from_col,
  edge_to_col
) {
  current_edge_nodes <- unique(c(
    as.character(edge_list_df[[edge_from_col]]),
    as.character(edge_list_df[[edge_to_col]])
  ))
  current_edge_nodes <- current_edge_nodes[!is.na(current_edge_nodes)]

  vertices_for_pagerank_df <- NULL
  if (drop_isolates_flag) {
    # Only keep nodes that participate in at least one complete edge.
    if (length(current_edge_nodes) > 0) {
      vertices_for_pagerank_df <- stats::setNames(
        data.frame(sort(current_edge_nodes)),
        node_col_name
      )
    }
  } else {
    # Keep all known nodes: original universe PLUS nodes introduced upstream.
    full_universe <- unique(c(all_vertex_universe, current_edge_nodes))
    if (length(full_universe) > 0) {
      vertices_for_pagerank_df <- stats::setNames(
        data.frame(sort(full_universe)),
        node_col_name
      )
    }
  }

  if (
    !is.null(folded_prior_df) &&
      prior_inject_unmatched &&
      !is.null(vertices_for_pagerank_df)
  ) {
    existing_nodes <- vertices_for_pagerank_df[[node_col_name]]
    prior_dests <- unique(stats::na.omit(folded_prior_df[[prior_url_col]]))
    to_add <- setdiff(prior_dests, existing_nodes)
    if (length(to_add) > 0) {
      vertices_for_pagerank_df <- stats::setNames(
        data.frame(sort(c(existing_nodes, to_add))),
        node_col_name
      )
    }
  }
  vertices_for_pagerank_df
}

#' Remove synthetic / hidden nodes from results, measuring their mass first.
#'
#' The stationary vector spans EVERY node (it sums to 1 by construction),
#' including the shared waste sink, the leak sink, and any robots-blocked nodes
#' the caller asked to vanish. This measures the mass each carried away —
#' evaporated (waste sink), leaked (leak sink), and hidden (the own mass of
#' robots-blocked nodes under `robots_blocked_action = "vanish"`; their
#' pass-through already routed to the waste sink) — then drops those rows so the
#' visible result carries only real, reported pages.
#'
#' @return A list with the trimmed `pagerank_results` and the `mass_evaporated`
#'   / `mass_leaked` / `mass_hidden` scalars.
#' @keywords internal
#' @noRd
.strip_internal_nodes <- function(
  pagerank_results,
  used_waste_sink,
  waste_sink_name,
  used_leak_sink,
  leak_sink_name,
  robots_blocked_action,
  robots_blocked_urls
) {
  mass_evaporated <- 0
  mass_leaked <- 0
  mass_hidden <- 0
  if (nrow(pagerank_results) > 0) {
    pr_node_col <- names(pagerank_results)[1]
    pr_value_col <- names(pagerank_results)[2]

    if (used_waste_sink) {
      sink_mask <- pagerank_results[[pr_node_col]] == waste_sink_name
      mass_evaporated <- sum(
        pagerank_results[[pr_value_col]][sink_mask],
        na.rm = TRUE
      )
      pagerank_results <- pagerank_results[!sink_mask, , drop = FALSE]
    }

    if (used_leak_sink) {
      leak_sink_mask <- pagerank_results[[pr_node_col]] == leak_sink_name
      mass_leaked <- sum(
        pagerank_results[[pr_value_col]][leak_sink_mask],
        na.rm = TRUE
      )
      pagerank_results <- pagerank_results[!leak_sink_mask, , drop = FALSE]
    }

    if (robots_blocked_action == "vanish" && length(robots_blocked_urls) > 0) {
      hidden_mask <- pagerank_results[[pr_node_col]] %in% robots_blocked_urls
      mass_hidden <- sum(
        pagerank_results[[pr_value_col]][hidden_mask],
        na.rm = TRUE
      )
      pagerank_results <- pagerank_results[!hidden_mask, , drop = FALSE]
    }

    row.names(pagerank_results) <- NULL
  }
  list(
    pagerank_results = pagerank_results,
    mass_evaporated = mass_evaporated,
    mass_leaked = mass_leaked,
    mass_hidden = mass_hidden
  )
}

#' Assemble the transition_audit object attached to a pagerank() result.
#'
#' Derives the remaining audit scalars (behavioral-weight coverage, unmatched
#' prior count, reported PageRank total, out-of-scope fold list) and constructs
#' the [transition_audit] via [new_transition_audit()]. Kept out of `pagerank()`
#' so the orchestrator stays readable; behavior is unchanged.
#' @return A `transition_audit` object.
#' @keywords internal
#' @noRd
.build_transition_audit <- function(
  pagerank_results,
  current_edge_list,
  folded_prior_df,
  vertices_for_pagerank_df,
  node_col_name,
  prior_url_col,
  effective_weight_col,
  weight_col,
  n_input_rows,
  n_edges,
  duplicate_edge_policy,
  instance_count_col,
  n_duplicate_instances,
  duplicate_edges,
  n_rows_na,
  n_rows_duplicate,
  n_self_loops,
  robots_blocked_urls,
  status_dead_urls = character(0),
  mass_evaporated,
  mass_leaked,
  mass_hidden,
  out_of_scope_fold,
  oos_sources,
  oos_targets,
  oos_signals,
  oos_applied,
  collisions_df,
  self_loops,
  drop_isolates_flag,
  reverse,
  nofollow_col,
  nofollow_action,
  robots_blocked_action,
  prior_alpha,
  prior_transform,
  prior_inject_unmatched,
  prior_exclude_waste,
  has_redirects,
  has_canonicals,
  indexability_df,
  status_df = NULL,
  preset = NULL,
  placement = NULL,
  boilerplate = NULL,
  position = NULL
) {
  # Derived scalars, config snapshot, and construction live in helpers below.
  metrics <- .transition_audit_metrics(
    pagerank_results = pagerank_results,
    current_edge_list = current_edge_list,
    folded_prior_df = folded_prior_df,
    vertices_for_pagerank_df = vertices_for_pagerank_df,
    node_col_name = node_col_name,
    prior_url_col = prior_url_col,
    effective_weight_col = effective_weight_col,
    oos_sources = oos_sources,
    oos_targets = oos_targets,
    oos_signals = oos_signals,
    robots_blocked_urls = robots_blocked_urls,
    status_dead_urls = status_dead_urls
  )
  .assemble_transition_audit(
    metrics = metrics,
    n_input_rows = n_input_rows,
    n_edges = n_edges,
    duplicate_edge_policy = duplicate_edge_policy,
    instance_count_col = instance_count_col,
    n_duplicate_instances = n_duplicate_instances,
    duplicate_edges = duplicate_edges,
    n_rows_na = n_rows_na,
    n_rows_duplicate = n_rows_duplicate,
    n_self_loops = n_self_loops,
    mass_evaporated = mass_evaporated,
    mass_leaked = mass_leaked,
    mass_hidden = mass_hidden,
    out_of_scope_fold = out_of_scope_fold,
    oos_applied = oos_applied,
    collisions_df = collisions_df,
    self_loops = self_loops,
    drop_isolates_flag = drop_isolates_flag,
    reverse = reverse,
    weight_col = weight_col,
    effective_weight_col = effective_weight_col,
    nofollow_col = nofollow_col,
    nofollow_action = nofollow_action,
    robots_blocked_action = robots_blocked_action,
    prior_alpha = prior_alpha,
    prior_transform = prior_transform,
    prior_inject_unmatched = prior_inject_unmatched,
    prior_exclude_waste = prior_exclude_waste,
    has_redirects = has_redirects,
    has_canonicals = has_canonicals,
    indexability_df = indexability_df,
    status_df = status_df,
    folded_prior_df = folded_prior_df,
    preset = preset,
    placement = placement,
    boilerplate = boilerplate,
    position = position
  )
}

#' Derive the scalar metrics carried by a transition_audit.
#'
#' Computes behavioral-weight coverage, the unmatched authority-prior count, the
#' reported PageRank total, the out-of-scope fold list, and the vertex / robots
#' / oos counts. Returns a named list consumed by [.assemble_transition_audit].
#' @keywords internal
#' @noRd
.transition_audit_metrics <- function(
  pagerank_results,
  current_edge_list,
  folded_prior_df,
  vertices_for_pagerank_df,
  node_col_name,
  prior_url_col,
  effective_weight_col,
  oos_sources,
  oos_targets,
  oos_signals,
  robots_blocked_urls,
  status_dead_urls = character(0)
) {
  # Behavioral-weight coverage: how many scored edges carry a usable weight.
  weighted <- !is.null(effective_weight_col) &&
    effective_weight_col %in% names(current_edge_list)
  n_edges_weighted <- 0L
  if (weighted && nrow(current_edge_list) > 0) {
    w <- suppressWarnings(as.numeric(current_edge_list[[effective_weight_col]]))
    n_edges_weighted <- sum(!is.na(w) & is.finite(w) & w > 0)
  }

  # Authority-prior URLs that never folded onto a vertex (unmatched).
  n_prior_unmatched <- NA_integer_
  if (!is.null(folded_prior_df) && !is.null(vertices_for_pagerank_df)) {
    final_nodes <- vertices_for_pagerank_df[[node_col_name]]
    prior_dests <- unique(stats::na.omit(folded_prior_df[[prior_url_col]]))
    n_prior_unmatched <- length(setdiff(prior_dests, final_nodes))
  }

  pagerank_total <- if (
    nrow(pagerank_results) > 0 &&
      ncol(pagerank_results) >= 2
  ) {
    sum(pagerank_results[[2]], na.rm = TRUE)
  } else {
    NA_real_
  }

  # Out-of-scope fold list (source, target, signal); NULL when there were none.
  oos_fold_df <- if (length(oos_sources) > 0) {
    data.frame(
      source = oos_sources,
      target = oos_targets,
      signal = oos_signals,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  list(
    weighted = weighted,
    weight_col = if (weighted) effective_weight_col else NULL,
    n_edges_weighted = n_edges_weighted,
    n_prior_unmatched = n_prior_unmatched,
    pagerank_total = pagerank_total,
    oos_fold_df = oos_fold_df,
    n_vertices = nrow(pagerank_results),
    n_robots_blocked = length(robots_blocked_urls),
    n_status_dead = length(status_dead_urls),
    n_out_of_scope_folds = length(oos_sources)
  )
}

#' Snapshot the pagerank() configuration into the transition_audit config list.
#' @keywords internal
#' @noRd
.transition_audit_config <- function(
  self_loops,
  drop_isolates_flag,
  reverse,
  weight_col,
  effective_weight_col,
  duplicate_edge_policy,
  nofollow_col,
  nofollow_action,
  robots_blocked_action,
  prior_alpha,
  prior_transform,
  prior_inject_unmatched,
  prior_exclude_waste,
  has_redirects,
  has_canonicals,
  indexability_df,
  status_df = NULL,
  folded_prior_df,
  preset = NULL,
  placement = NULL,
  boilerplate = NULL,
  position = NULL
) {
  # noindex no longer synthesizes a nofollow column, so the config reports the
  # caller-supplied nofollow_col verbatim (NULL when none was given).
  config_nofollow_col <- nofollow_col
  list(
    preset = .pr_preset_label(preset),
    placement = placement,
    boilerplate = boilerplate,
    position = position,
    self_loops = self_loops,
    drop_isolates_flag = drop_isolates_flag,
    reverse = reverse,
    weight_col = weight_col,
    effective_weight_col = effective_weight_col,
    duplicate_edge_policy = duplicate_edge_policy,
    nofollow_col = config_nofollow_col,
    nofollow_action = nofollow_action,
    robots_blocked_action = robots_blocked_action,
    prior_alpha = prior_alpha,
    prior_transform = prior_transform,
    prior_inject_unmatched = prior_inject_unmatched,
    prior_exclude_waste = isTRUE(prior_exclude_waste),
    has_redirects = isTRUE(has_redirects),
    has_canonicals = isTRUE(has_canonicals),
    has_indexability = !is.null(indexability_df) &&
      nrow(indexability_df) > 0,
    has_status = !is.null(status_df) &&
      nrow(status_df) > 0,
    has_prior = !is.null(folded_prior_df)
  )
}

#' Construct the transition_audit object from precomputed metrics + raw counts.
#'
#' Packages the config snapshot (via [.transition_audit_config]) and forwards
#' every field to [new_transition_audit()]. Split out so the derived-metric and
#' config concerns live in their own helpers; behavior is unchanged.
#' @keywords internal
#' @noRd
.assemble_transition_audit <- function(
  metrics,
  n_input_rows,
  n_edges,
  duplicate_edge_policy,
  instance_count_col,
  n_duplicate_instances,
  duplicate_edges,
  n_rows_na,
  n_rows_duplicate,
  n_self_loops,
  mass_evaporated,
  mass_leaked,
  mass_hidden,
  out_of_scope_fold,
  oos_applied,
  collisions_df,
  self_loops,
  drop_isolates_flag,
  reverse,
  weight_col,
  effective_weight_col,
  nofollow_col,
  nofollow_action,
  robots_blocked_action,
  prior_alpha,
  prior_transform,
  prior_inject_unmatched,
  prior_exclude_waste,
  has_redirects,
  has_canonicals,
  indexability_df,
  status_df = NULL,
  folded_prior_df,
  preset = NULL,
  placement = NULL,
  boilerplate = NULL,
  position = NULL
) {
  new_transition_audit(
    n_input_rows = n_input_rows,
    n_edges = n_edges,
    n_vertices = metrics$n_vertices,
    weighted = metrics$weighted,
    weight_col = metrics$weight_col,
    n_edges_weighted = metrics$n_edges_weighted,
    duplicate_edge_policy = duplicate_edge_policy,
    instance_count_col = instance_count_col,
    n_duplicate_instances = n_duplicate_instances,
    duplicate_edges = duplicate_edges,
    n_rows_na = n_rows_na,
    n_rows_duplicate = n_rows_duplicate,
    n_self_loops = n_self_loops,
    n_prior_unmatched = metrics$n_prior_unmatched,
    n_robots_blocked = metrics$n_robots_blocked,
    n_status_dead = metrics$n_status_dead,
    pagerank_total = metrics$pagerank_total,
    mass_reported = metrics$pagerank_total,
    mass_evaporated = mass_evaporated,
    mass_leaked = mass_leaked,
    mass_hidden = mass_hidden,
    out_of_scope_fold = out_of_scope_fold,
    n_out_of_scope_folds = metrics$n_out_of_scope_folds,
    out_of_scope_folds_applied = oos_applied,
    out_of_scope_fold_list = metrics$oos_fold_df,
    fold_collisions = collisions_df,
    config = .transition_audit_config(
      self_loops = self_loops,
      drop_isolates_flag = drop_isolates_flag,
      reverse = reverse,
      weight_col = weight_col,
      effective_weight_col = effective_weight_col,
      duplicate_edge_policy = duplicate_edge_policy,
      nofollow_col = nofollow_col,
      nofollow_action = nofollow_action,
      robots_blocked_action = robots_blocked_action,
      prior_alpha = prior_alpha,
      prior_transform = prior_transform,
      prior_inject_unmatched = prior_inject_unmatched,
      prior_exclude_waste = prior_exclude_waste,
      has_redirects = has_redirects,
      has_canonicals = has_canonicals,
      indexability_df = indexability_df,
      status_df = status_df,
      folded_prior_df = folded_prior_df,
      preset = preset,
      placement = placement,
      boilerplate = boilerplate,
      position = position
    )
  )
}

#' Apply the duplicate-edge policy and collect its audit metadata.
#'
#' Dedups the edge list per `duplicate_edge_policy` (see
#' [.apply_duplicate_edge_policy]); under `"count_instances"` also records the
#' synthetic instance-count column, the effective weight column (the
#' instance-count column when no `weight_col` was supplied), the per-edge
#' duplicate audit rows, and the total duplicate-instance count.
#'
#' @return A list with `edge_list_df`, `instance_count_col`,
#'   `effective_weight_col`, `duplicate_edges`, and `n_duplicate_instances`.
#' @keywords internal
#' @noRd
.apply_duplicate_policy_audited <- function(
  edge_list_df,
  duplicate_edge_policy,
  self_loops,
  weight_col,
  from_col,
  to_col
) {
  edge_list_df <- .apply_duplicate_edge_policy(
    edge_list_df = edge_list_df,
    policy = duplicate_edge_policy,
    self_loops = self_loops,
    from_col = from_col,
    to_col = to_col
  )

  instance_count_col <- NULL
  effective_weight_col <- weight_col
  duplicate_edges <- NULL
  n_duplicate_instances <- 0L

  if (duplicate_edge_policy == "count_instances") {
    instance_count_col <- "__pr_instance_count__"
    if (is.null(weight_col)) {
      effective_weight_col <- instance_count_col
    }
    duplicate_edges <- .duplicate_edge_audit_rows(
      edge_list_df = edge_list_df,
      from_col = from_col,
      to_col = to_col,
      instance_count_col = instance_count_col,
      weight_col = effective_weight_col
    )
    if (is.data.frame(duplicate_edges) && nrow(duplicate_edges) > 0) {
      n_duplicate_instances <- sum(duplicate_edges$instance_count, na.rm = TRUE)
    }
  }

  list(
    edge_list_df = edge_list_df,
    instance_count_col = instance_count_col,
    effective_weight_col = effective_weight_col,
    duplicate_edges = duplicate_edges,
    n_duplicate_instances = n_duplicate_instances
  )
}

#' Add the leak-sink self-loop (no-op unless the leak sink was used).
#'
#' Added after deduplication so `self_loops = "drop"` cannot strip it, mirroring
#' the nofollow sink. Keeps the sink from being a dangling node so the equity
#' routed to it stays trapped and later evaporates.
#' @return The (possibly extended) edge list data frame.
#' @keywords internal
#' @noRd
.add_leak_sink_selfloop <- function(
  edge_list_df,
  used_leak_sink,
  leak_sink_name,
  edge_from_col,
  edge_to_col
) {
  if (used_leak_sink && nrow(edge_list_df) > 0) {
    leak_sink_row <- .make_sink_rows(
      edge_list_df,
      edge_from_col,
      edge_to_col,
      leak_sink_name
    )
    edge_list_df <- rbind(edge_list_df, leak_sink_row)
  }
  edge_list_df
}

#' Nodes excluded from the teleport prior (synthetic waste / leak sinks).
#'
#' Class members (noindex / robots-blocked / response-dead) are real pages and
#' keep their authority, so they are NOT excluded here (excluding them is
#' PAGE-bcpacnfm, a separate change).
#' @return A character vector of node names to exclude from teleport.
#' @keywords internal
#' @noRd
.prior_exclude_nodes <- function(
  used_waste_sink,
  waste_sink_name,
  used_leak_sink,
  leak_sink_name,
  prior_exclude_waste = FALSE,
  noindex_urls = character(0),
  robots_blocked_urls = character(0),
  status_dead_urls = character(0)
) {
  # The synthetic sinks never receive teleport: they are not real pages, so
  # paying them the uniform teleport share would inflate the accounting sink.
  # When `prior_exclude_waste` is on, the collect-but-cannot-pass class
  # (noindex / robots-blocked / 4xx-5xx) is zeroed too, so a hub cannot
  # manufacture authority by linking to dead ends (PAGE-bcpacnfm). Class members
  # keep the rank that reaches them via inlinks; only their teleport is removed.
  nodes <- character(0)
  if (used_waste_sink) {
    nodes <- c(nodes, waste_sink_name)
  }
  if (used_leak_sink) {
    nodes <- c(nodes, leak_sink_name)
  }
  if (isTRUE(prior_exclude_waste)) {
    nodes <- c(nodes, noindex_urls, robots_blocked_urls, status_dead_urls)
  }
  unique(nodes)
}
