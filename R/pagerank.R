#' @title Master PageRank Calculation Wrapper
#' @description Orchestrates the complete PageRank calculation workflow,
#' including URL cleaning, redirect resolution, edge deduplication,
#' indexability handling, nofollow handling, isolate handling, and
#' PageRank computation.
#' @name pagerank
#'
#' @param edge_list_df A data frame representing the edge list, typically with
#'   columns like "from" and "to".
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
#' @param robots_blocked_action How to present robots.txt-blocked pages in
#'   results. One of:
#'   \describe{
#'     \item{`"trap"`}{(default) Blocked pages appear in results showing their
#'       accumulated (trapped) PageRank, useful for seeing wasted PR.}
#'     \item{`"vanish"`}{Blocked pages are removed from results. Their PR
#'       disappears.}
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
#' @param prior_verbose Logical, whether to emit prior-alignment coverage
#'   diagnostics. Default `TRUE`. Only relevant when `prior_df` is supplied.
#' @param ... Additional arguments passed to [compute_pagerank()] and
#'   subsequently to `igraph::page_rank()`. Besides `damping`, the recognized
#'   convergence controls `algo` (`"prpack"` / `"arpack"`), `eps`, and `niter`
#'   are forwarded here; see the "Convergence controls" section below.
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
#' ## Indexability handling
#'
#' When `indexability_df` is provided, two types of pages receive special
#' treatment:
#'
#' **Indexed-corpus assumption and noindex pages:** `pagerankr` models the
#' ranked corpus as the set of indexed documents. Under this package assumption,
#' a noindex page is outside that corpus: it may receive authority through
#' inlinks, but it cannot redistribute that authority within the indexed graph.
#' Its outgoing links are therefore treated as nofollow for propagation and
#' processed according to `nofollow_action`: `"evaporate"` leaves them as
#' slot-consuming links whose shares reach the sink, `"drop"` removes them
#' before shares are allocated, and `"keep"` follows them normally. This is a
#' PageRank modeling choice in `pagerankr`; it does not assert that Google
#' defines noindex as a nofollow directive. Noindex pages may still appear in
#' the returned results so their received authority remains auditable. Hiding
#' them would be an optional reporting choice, not a propagation correction.
#'
#' **robots.txt-blocked pages:** Google cannot access the page content, so
#' there are no visible outgoing links. All outgoing edges are removed and a
#' self-loop is added to trap inbound PageRank. The `robots_blocked_action`
#' parameter controls whether these pages appear in results (`"trap"`) or
#' are removed (`"vanish"`).
#'
#' **Priority rule:** robots.txt always takes precedence over noindex. If a
#' page is both robots-blocked and noindex, it is treated as robots-blocked.
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
#'   \item{`indexability_df`}{Errors. noindex (outlinks-as-nofollow) and
#'     robots.txt blocking (drop outlinks + trap self-loop) encode forward
#'     crawl/index behavior with no meaningful transpose.}
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
#'   nofollow evaporation, indexability handling, or `robots_blocked_action =
#'   "vanish"` is active, the returned scores may sum to less than 1. The
#'   difference is not undifferentiated "leakage": it is decomposed into
#'   **evaporated mass** (authority sent to the nofollow sink), **leaked mass**
#'   (authority sent to the leak sink under `out_of_scope_fold = "leak"`), and
#'   **hidden mass** (authority trapped on robots-blocked nodes removed from the
#'   results). The full breakdown — reported / evaporated (sink) / leaked /
#'   hidden / total (= 1) — is recorded in the `mass` field of the transition
#'   audit (see below).
#'
#'   The data frame additionally carries a `"transition_audit"` attribute (a
#'   [transition_audit] object) recording how the transition graph was built:
#'   row/edge counts, behavioral-weight coverage, normalization totals, the
#'   page-mass decomposition (reported / evaporated / leaked / hidden / total),
#'   dropped
#'   data (rows lost to NA / dedup / self-loops, unmatched prior URLs), and the
#'   model configuration used. Retrieve it with
#'   `attr(result, "transition_audit")`. This attribute is backward-compatible:
#'   the data frame itself (its columns and rows) is unchanged.
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
pagerank <- function(edge_list_df,
                     redirects_df = NULL,
                     clean_edge_urls = TRUE,
                     clean_redirect_urls = TRUE,
                     rurl_params = list(),
                     self_loops = c("drop", "keep"),
                     drop_isolates_flag = TRUE,
                     reverse = FALSE,
                     weight_col = NULL,
                     duplicate_edge_policy = c(
                       "collapse", "aggregate", "count_instances"
                     ),
                     nofollow_col = NULL,
                     nofollow_action = c("evaporate", "drop", "keep"),
                     indexability_df = NULL,
                     indexability_url_col = "url",
                     indexability_status_col = "indexability_status",
                     robots_blocked_action = c("trap", "vanish"),
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
                       "none", "log", "percentile",
                       "minmax", "zipf", "rank_linear"
                     ),
                     prior_alpha = 0,
                     prior_inject_unmatched = FALSE,
                     prior_verbose = TRUE,
                     damping = 0.85,
                     ...) {
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

  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  # Further column checks within functions called.

  if (!is.null(redirects_df) && !is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.null(canonicals_df) && !is.data.frame(canonicals_df)) {
    stop("`canonicals_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (!is.logical(clean_canonical_urls) || length(clean_canonical_urls) != 1) {
    stop(
      "`clean_canonical_urls` must be a single logical value.",
      call. = FALSE
    )
  }
  canonical_cols <- c(canonical_from_col, canonical_to_col)
  if (!is.null(canonicals_df) && nrow(canonicals_df) > 0 &&
        !all(canonical_cols %in% names(canonicals_df))) {
    stop("`canonicals_df` must have '", canonical_from_col, "' and '",
      canonical_to_col, "' columns.",
      call. = FALSE
    )
  }
  if (!is.logical(clean_edge_urls) || length(clean_edge_urls) != 1) {
    stop("`clean_edge_urls` must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(clean_redirect_urls) || length(clean_redirect_urls) != 1) {
    stop("`clean_redirect_urls` must be a single logical value.", call. = FALSE)
  }
  if (!is.list(rurl_params)) {
    stop("`rurl_params` must be a list.", call. = FALSE)
  }
  if (!is.logical(drop_isolates_flag) || length(drop_isolates_flag) != 1) {
    stop("`drop_isolates_flag` must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(reverse) || length(reverse) != 1 || is.na(reverse)) {
    stop("`reverse` must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(damping) || length(damping) != 1 ||
        is.na(damping) || damping < 0 || damping > 1) {
    stop(
      "`damping` must be a single numeric value between 0 and 1.",
      call. = FALSE
    )
  }

  # --- Reverse / inverse PageRank (CheiRank) compatibility guards ---
  # Reversal transposes only the link graph. Features whose semantics depend on
  # the *direction of authority flow* do not transpose cleanly and are rejected
  # rather than silently producing misleading scores:
  #   * nofollow "evaporate": the sink device models the SOURCE wasting its
  #     outgoing budget (a forward concept); reversed, the sink would inject
  #     rank instead. Use "drop" (the correct CheiRank treatment: a nofollowed
  #     link funnels no authority outward) or "keep".
  #   * indexability: noindex => outlinks-as-nofollow and robots-blocked =>
  #     drop-outlinks + trap-self-loop both encode forward crawl/index
  #     behavior with no meaningful transpose.
  # Direction-agnostic features (cleaning, redirect folding, dedup, weights,
  # domain/host filtering, TIPR prior) remain fully supported under reverse.
  if (isTRUE(reverse)) {
    if (!is.null(indexability_df) && nrow(indexability_df) > 0) {
      stop(
        "`indexability_df` is not supported with `reverse = TRUE`: noindex ",
        "and robots.txt handling encode forward crawl semantics that do not ",
        "transpose. Drop `indexability_df` or set `reverse = FALSE`.",
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
  }

  # Validate weight_col
  if (!is.null(weight_col)) {
    if (!is.character(weight_col) || length(weight_col) != 1) {
      stop(
        "`weight_col` must be a single character string or NULL.",
        call. = FALSE
      )
    }
    if (nrow(edge_list_df) > 0 && !(weight_col %in% names(edge_list_df))) {
      stop(
        "`weight_col` '", weight_col, "' not found in `edge_list_df`.",
        call. = FALSE
      )
    }
  }

  # Validate nofollow_col
  if (!is.null(nofollow_col)) {
    if (!is.character(nofollow_col) || length(nofollow_col) != 1) {
      stop(
        "`nofollow_col` must be a single character string or NULL.",
        call. = FALSE
      )
    }
    if (nrow(edge_list_df) > 0 && !(nofollow_col %in% names(edge_list_df))) {
      stop(
        "`nofollow_col` '", nofollow_col, "' not found in `edge_list_df`.",
        call. = FALSE
      )
    }
  }

  # Validate indexability_df
  if (!is.null(indexability_df)) {
    if (!is.data.frame(indexability_df)) {
      stop("`indexability_df` must be a data frame or NULL.", call. = FALSE)
    }
    if (nrow(indexability_df) > 0) {
      if (!(indexability_url_col %in% names(indexability_df))) {
        stop("`indexability_url_col` '", indexability_url_col,
          "' not found in `indexability_df`.",
          call. = FALSE
        )
      }
      if (!(indexability_status_col %in% names(indexability_df))) {
        stop("`indexability_status_col` '", indexability_status_col,
          "' not found in `indexability_df`.",
          call. = FALSE
        )
      }
    }
  }

  # Validate prior_df (TIPR personalization)
  if (!is.null(prior_df)) {
    if (!is.data.frame(prior_df)) {
      stop("`prior_df` must be a data frame or NULL.", call. = FALSE)
    }
    if (nrow(prior_df) > 0) {
      if (!(prior_url_col %in% names(prior_df))) {
        stop("`prior_url_col` '", prior_url_col,
          "' not found in `prior_df`.",
          call. = FALSE
        )
      }
      if (!(prior_weight_col %in% names(prior_df))) {
        stop("`prior_weight_col` '", prior_weight_col,
          "' not found in `prior_df`.",
          call. = FALSE
        )
      }
      if (!is.numeric(prior_df[[prior_weight_col]])) {
        stop("`prior_weight_col` '", prior_weight_col,
          "' must be a numeric column.",
          call. = FALSE
        )
      }
    }
  }
  if (!is.numeric(prior_alpha) || length(prior_alpha) != 1 ||
        is.na(prior_alpha) || prior_alpha < 0 || prior_alpha > 1) {
    stop(
      "`prior_alpha` must be a single number between 0 and 1.",
      call. = FALSE
    )
  }
  if (!is.logical(prior_inject_unmatched) ||
        length(prior_inject_unmatched) != 1) {
    stop(
      "`prior_inject_unmatched` must be a single logical value.",
      call. = FALSE
    )
  }

  # Dots for igraph params are handled by compute_pagerank directly.

  # --- Initialize working copies of data frames ---
  current_edge_list <- edge_list_df
  current_redirects_list <- redirects_df
  current_canonicals_list <- canonicals_df

  # --- Transition audit / provenance: capture the raw input size up front. ---
  # Counts accumulated along the aggregation / validation / cleaning path are
  # assembled into a `transition_audit` object at the end and attached to the
  # result (see R/transition_audit.R).
  audit_n_input_rows <- nrow(edge_list_df)

  # --- 1. URL Cleaning (Potentially Shared Memoization) ---
  # As per Spec: "ensures that all unique URLs from both the edge list and
  # redirect list are canonicalized *once* per unique string using a shared
  # memoized `rurl::clean_url` instance"

  # Determine edge and redirect columns for cleaning
  edge_url_cols <- intersect(
    c(edge_from_col, edge_to_col),
    names(current_edge_list)
  )
  redirect_url_cols <- if (!is.null(current_redirects_list)) {
    intersect(
      c(redirect_from_col, redirect_to_col),
      names(current_redirects_list)
    )
  } else {
    character(0)
  }
  canonical_url_cols <- if (!is.null(current_canonicals_list)) {
    intersect(
      c(canonical_from_col, canonical_to_col),
      names(current_canonicals_list)
    )
  } else {
    character(0)
  }

  # Resolve the full canonicalization profile once: every rurl knob pinned
  # explicitly (see .canonical_profile()), with user `rurl_params` overriding
  # per key. This single resolved profile drives BOTH the cleaning path below
  # and the domain-filtering step (2.7), so the two cannot drift apart, and
  # node identities never depend on rurl's own (version-dependent) defaults.
  # It mirrors the cross-project canonicalization contract (semantic FR-05):
  #   - case_handling = "lower_host": scheme + host fold case-insensitively
  #     (RFC 3986); the path keeps its case.
  #   - protocol_handling = "keep": add a scheme to scheme-less URLs but never
  #     rewrite an existing one (an http->https redirect folds via the redirect
  #     map, not by rewriting the scheme).
  effective_rurl_params <- .resolve_rurl_params(rurl_params)

  # rurl::get_clean_url memoizes parses internally and the cache is shared
  # across calls, so edge and redirect URLs that overlap are canonicalized once
  # per unique string without any local memoizer.
  if (clean_edge_urls && length(edge_url_cols) > 0) {
    current_edge_list <- do.call(
      clean_url_columns,
      c(list(
        data_frame = current_edge_list,
        columns = edge_url_cols
      ), effective_rurl_params)
    )
  }
  if (clean_redirect_urls && !is.null(current_redirects_list) &&
        nrow(current_redirects_list) > 0 && length(redirect_url_cols) > 0) {
    current_redirects_list <- do.call(
      clean_url_columns,
      c(list(
        data_frame = current_redirects_list,
        columns = redirect_url_cols
      ), effective_rurl_params)
    )
  }
  # Canonicals are cleaned through the SAME resolved profile as edges and
  # redirects, so the composed fold map operates in one node namespace.
  if (clean_canonical_urls && !is.null(current_canonicals_list) &&
        nrow(current_canonicals_list) > 0 && length(canonical_url_cols) > 0) {
    current_canonicals_list <- do.call(
      clean_url_columns,
      c(list(
        data_frame = current_canonicals_list,
        columns = canonical_url_cols
      ), effective_rurl_params)
    )
  }

  # --- Warning for Uncleaned Edge URLs with Query Parameters ---
  # Per spec: when edge URL cleaning is disabled and edge URLs still contain
  # query parameters, the user should be warned.
  if (!clean_edge_urls && length(edge_url_cols) > 0) {
    has_query_params <- .urls_contain_query_params(
      current_edge_list,
      columns = edge_url_cols
    )
    if (has_query_params) {
      warning(
        "URLs in `edge_list_df` may contain query parameters ",
        "(e.g. '?' or '&'). Consider setting `clean_edge_urls = TRUE` ",
        "for consistent PageRank calculation, using `rurl_params` to ",
        "control `rurl::clean_url` behavior if needed.",
        call. = FALSE
      )
    }
  }

  # Snapshot of the crawled node namespace as it stands BEFORE any redirect /
  # canonical fold is applied (cleaned edge endpoints only; indexability URLs
  # are not part of scope). Captured here so the domain filter (step 2.7) can
  # tell whether a keep/exclude value that matches zero surviving nodes did so
  # because an out-of-scope fold rewrote the crawled domain/host away. Kept
  # local and independent of the fold block's own `prefold_nodes` (which is
  # scoped to that block and only computed when the map is non-empty).
  sf_prefold_nodes <- unique(stats::na.omit(c(
    as.character(current_edge_list[[edge_from_col]]),
    as.character(current_edge_list[[edge_to_col]])
  )))

  # --- 2. Redirect + canonical resolution (one composed fold map) ---
  # Build the single composed fold map ONCE from both signals (it is the empty
  # map when neither is supplied, and the plain redirect map when canonicals are
  # absent -- preserving prior behavior). This same map is the source of truth
  # for BOTH edge folding here and TIPR prior folding (step 2.8), and matches
  # what build_fold_map() exports downstream.
  has_redirects <- !is.null(current_redirects_list) &&
    nrow(current_redirects_list) > 0
  has_canonicals <- !is.null(current_canonicals_list) &&
    nrow(current_canonicals_list) > 0

  # Audit: whether each signal *materially* folded an edge (contributed an
  # entry to the composed fold map). Mere presence of a no-op signal (e.g. a
  # self-canonical) leaves the graph -- and these flags -- untouched, so the
  # audit of an effective no-op matches a call with no signal at all.
  audit_has_redirects <- FALSE
  audit_has_canonicals <- FALSE

  # Out-of-scope fold diagnostics (populated regardless of `out_of_scope_fold`;
  # wired into the transition_audit `fold` section at the constructor below).
  # An out-of-scope fold is a composed fold-map entry whose TARGET is not a
  # crawled node -- folding it invents a phantom vertex.
  audit_oos_sources <- character(0)
  audit_oos_targets <- character(0)
  audit_oos_signals <- character(0)
  # Fold-target collisions (SF-scope / PAGE-rjrduvmy). Populated on the pre-fold
  # edge list when a fold relabels a crawled source onto an uncrawled URL that
  # is ALSO independently linked, silently merging their inbound equity. A data
  # frame of the merged targets, or NULL when none. See the collision-detection
  # block at the fold-application site below.
  audit_collisions_df <- NULL
  # `relabel` applies the out-of-scope entries; `keep` skips them; `leak`
  # routes the crawled source onto the leak sink. `applied` is TRUE when the
  # out-of-scope folds were acted upon (relabeled through, or routed to the
  # leak sink) and FALSE when they were skipped / kept as crawled.
  audit_oos_applied <- out_of_scope_fold %in% c("relabel", "leak")

  # Leak sink: a synthetic node, distinct from the nofollow sink, that absorbs
  # the equity flowing into out-of-scope-folded sources under
  # `out_of_scope_fold = "leak"`, so it evaporates out of the measured graph.
  # Sources to leak are recorded here and routed to the sink after domain
  # filtering (mirroring the nofollow sink, so the synthetic node is never
  # subject to host/domain rules).
  leak_sink_name <- "__pr_leak_sink__"
  used_leak_sink <- FALSE
  leak_sources <- character(0)

  fold_map <- character(0)
  if (has_redirects || has_canonicals) {
    fold <- .compose_fold_map(
      redirects_df = if (has_redirects) current_redirects_list else NULL,
      canonicals_df = if (has_canonicals) current_canonicals_list else NULL,
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

    if (length(fold$signal) > 0) {
      audit_has_redirects <- any(fold$signal == "redirect")
      audit_has_canonicals <- any(fold$signal == "canonical")
    }

    if (length(fold_map) > 0) {
      # Pre-fold crawled node set: unique, non-NA edge endpoints captured
      # IMMEDIATELY BEFORE the fold is applied. Indexability URLs are NOT part
      # of scope -- the crawled set is edge endpoints only.
      prefold_nodes <- unique(stats::na.omit(c(
        as.character(current_edge_list[[edge_from_col]]),
        as.character(current_edge_list[[edge_to_col]])
      )))

      # Out-of-scope entries: fold targets that are not crawled nodes.
      oos_mask <- !(unname(fold_map) %in% prefold_nodes)
      if (any(oos_mask)) {
        audit_oos_sources <- names(fold_map)[oos_mask]
        audit_oos_targets <- unname(fold_map)[oos_mask]
        audit_oos_signals <- unname(fold$signal[audit_oos_sources])
      }

      # Under `keep`, drop the out-of-scope entries before applying the map to
      # edges (and, below, the TIPR prior) so crawled sources retain their
      # as-crawled identity and edges + prior stay in one namespace. `relabel`
      # (default) applies the full map unchanged. `leak` also drops the entries
      # from the fold map (so the source keeps its as-crawled identity through
      # folding + domain filtering) but records the sources so they can be
      # routed onto the leak sink afterwards.
      if (any(oos_mask)) {
        if (identical(out_of_scope_fold, "keep")) {
          fold_map <- fold_map[!oos_mask]
        } else if (identical(out_of_scope_fold, "leak")) {
          fold_map <- fold_map[!oos_mask]
          leak_sources <- audit_oos_sources
          used_leak_sink <- TRUE
        }
      }
    }

    if (length(fold_map) > 0) {
      # --- Fold-target collision detection (SF-scope / PAGE-rjrduvmy). ---
      # Computed on the PRE-fold edge endpoints, using the fold map exactly as
      # it will be applied (after any keep/leak dropping above). A fold entry
      # `source -> target` COLLIDES when the relabel silently merges the crawled
      # source's node with a node that already carries genuine, INDEPENDENT
      # inbound links to `target`, inflating its PageRank invisibly:
      #   (1) `target` is a pure link-target -- it appears ONLY as a `to`, never
      #       as a `from`, AND is NOT a known crawled URL (absent from the crawl
      #       table `indexability_df`). This second clause is what separates the
      #       harmful case (an UNCRAWLED canonical/redirect target, e.g. a
      #       production URL a staged page folds onto) from a benign fold onto a
      #       genuinely crawled LEAF page (a real 200 page that simply has no
      #       outlinks -- it appears only as a `to` too, but IS in the crawl
      #       table, so it is not flagged); AND
      #   (2) `target` is independently referenced -- it is the `to` of >=1 edge
      #       whose `from` is NOT itself a source folding onto `target` (i.e.
      #       not the folding sources' own edges to their canonical target); AND
      #   (3) >=1 source folding onto `target` is an actual pre-fold edge
      #       endpoint, so the relabel genuinely merges a crawled node onto it.
      # A normal redirect/canonical onto a genuinely crawled target (a `from`,
      # or a leaf listed in `indexability_df`) is NOT flagged -- correct merge.
      #
      # b2 fallback: this diagnostic REQUIRES crawl-URL knowledge. Without
      # `indexability_df` there is no way to tell a crawled leaf page from an
      # uncrawled fold target (they are identical in the edge list), so
      # detection is skipped entirely and `collisions` stays NULL.
      have_crawl_urls <- !is.null(indexability_df) && nrow(indexability_df) > 0
      if (have_crawl_urls) {
        # Known crawled URLs, canonicalized into the SAME namespace as the edges
        # / fold targets (indexability URLs are not cleaned elsewhere, so clean
        # them here through the same resolved profile when edge cleaning is on).
        crawl_urls <- as.character(indexability_df[[indexability_url_col]])
        if (clean_edge_urls) {
          idx_tmp <- data.frame(u = crawl_urls, stringsAsFactors = FALSE)
          idx_tmp <- do.call(
            clean_url_columns,
            c(list(data_frame = idx_tmp, columns = "u"), effective_rurl_params)
          )
          crawl_urls <- as.character(idx_tmp$u)
        }
        crawl_urls <- unique(stats::na.omit(crawl_urls))

        prefold_from <- as.character(current_edge_list[[edge_from_col]])
        prefold_to <- as.character(current_edge_list[[edge_to_col]])
        fold_sources <- names(fold_map)
        fold_targets <- unname(fold_map)

        coll_targets <- character(0)
        coll_nrefs <- integer(0)
        coll_sources <- character(0)

        # Candidate targets: pure link-targets (never a `from`) that are ALSO
        # not known crawled URLs.
        cand_targets <- unique(fold_targets[
          !(fold_targets %in% prefold_from) & !(fold_targets %in% crawl_urls)
        ])
        for (tgt in cand_targets) {
          srcs <- fold_sources[fold_targets == tgt]
          # A merge only happens if >=1 folding source is a real crawled node.
          if (!any(srcs %in% prefold_nodes)) {
            next
          }
          # Independent inbound references: edges to `tgt` from a page that is
          # not one of the sources folding onto `tgt`.
          indep <- prefold_to == tgt & !(prefold_from %in% srcs)
          n_indep <- sum(indep, na.rm = TRUE)
          if (n_indep > 0L) {
            coll_targets <- c(coll_targets, tgt)
            coll_nrefs <- c(coll_nrefs, as.integer(n_indep))
            coll_sources <- c(
              coll_sources, paste(unique(srcs[srcs %in% prefold_nodes]),
                collapse = ", "
              )
            )
          }
        }

        if (length(coll_targets) > 0) {
          audit_collisions_df <- data.frame(
            target = coll_targets,
            n_independent_refs = coll_nrefs,
            source = coll_sources,
            stringsAsFactors = FALSE
          )
          warning(
            "Fold-target collision: canonical/redirect folding relabeled ",
            "crawled page(s) onto uncrawled URL(s) that are ALSO ",
            "independently linked, silently merging their inbound link equity ",
            "and inflating PageRank: ",
            paste0("`", coll_targets, "`", collapse = ", "),
            ". Inspect `attr(result, \"transition_audit\")$fold$collisions`.",
            call. = FALSE
          )
        }
      }

      for (col_name in c(edge_from_col, edge_to_col)) {
        if (col_name %in% names(current_edge_list)) {
          current_edge_list[[col_name]] <- .apply_fold_map(
            current_edge_list[[col_name]], fold_map
          )
        }
      }
    }
  }

  # --- 2.7. Domain / host filtering ---
  # NOTE ON ORDERING: filtering runs AFTER the fold above, so it scopes the
  # post-fold (canonical) namespace, not the crawled input. When an
  # out-of-scope canonical/redirect rewrites the crawled domain/host onto a
  # different one, a filter naming the crawled value silently matches nothing.
  # We detect that case and warn (below). To scope the INPUT you crawled, run
  # filter_links_by_domain() on the edge list BEFORE calling pagerank().
  if (!is.null(keep_domains) || !is.null(exclude_domains) ||
        !is.null(keep_hosts) || !is.null(exclude_hosts)) {
    # The post-fold node namespace the filter actually sees (folded edge
    # endpoints, before the filter drops anything). Compared against the
    # pre-fold snapshot so the fold -- not the filter -- is isolated as the
    # cause of a folded-away value.
    sf_postfold_nodes <- unique(stats::na.omit(c(
      as.character(current_edge_list[[edge_from_col]]),
      as.character(current_edge_list[[edge_to_col]])
    )))

    # Identify keep/exclude value(s) that classified one or more crawled
    # (pre-fold) nodes but no surviving post-fold node -- i.e. folded out of
    # scope. Reuses filter_links_by_domain()'s own extraction + resolution
    # (same rurl profile, same PSL) so pre/post comparison keys match the
    # filter exactly.
    folded_away <- .sf_folded_away_filter_values(
      prefold_nodes = sf_prefold_nodes,
      postfold_nodes = sf_postfold_nodes,
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

    # Forward the same resolved canonicalization profile used for cleaning, so
    # the filter extracts hosts/domains exactly as the (already cleaned) node
    # keys were derived (host_encoding, www_handling, subdomain levels, etc.).
    current_edge_list <- filter_links_by_domain(
      edge_list_df = current_edge_list,
      from_col = edge_from_col,
      to_col = edge_to_col,
      keep_domains = keep_domains,
      ignore_domains = exclude_domains,
      keep_hosts = keep_hosts,
      ignore_hosts = exclude_hosts,
      rurl_params = effective_rurl_params
    )
  }

  # --- 2.5. Extract full vertex universe (before NA rows are stripped) ---
  # This must happen before get_unique_edges() which drops rows with NAs.
  # When drop_isolates_flag = FALSE, nodes from partial rows (one NA column)
  # represent known URLs that should be included in the graph as isolates.
  # Standardized name for intermediate vertex data
  temp_node_col_name <- "node_name"
  all_vertex_universe <- unique(stats::na.omit(c(
    as.character(current_edge_list[[edge_from_col]]),
    as.character(current_edge_list[[edge_to_col]])
  )))

  # --- 2.75. Leak out-of-scope-folded sources to the leak sink ---
  # Under out_of_scope_fold = "leak", a crawled page whose canonical / redirect
  # folds OUT of scope is treated like an external redirect: its RECEIVED equity
  # should leave the measured graph. Inbound edges (... -> source) are
  # retargeted onto the dedicated leak sink so that equity reaches the sink (a
  # pure trap via its self-loop) and evaporates; the source's OUTBOUND edges are
  # dropped, since an external-redirect target contributes no links to the
  # measured graph and the leaked equity must not flow back to any surviving
  # page. Done AFTER domain filtering (so the synthetic sink node is never
  # subject to host/domain rules).
  # The leaked source is removed from the vertex universe (it must not linger as
  # an isolate), but its out-targets stay in scope. The sink self-loop is added
  # later, after deduplication, so `self_loops = "drop"` cannot strip it.
  if (used_leak_sink && length(leak_sources) > 0) {
    if (nrow(current_edge_list) > 0) {
      if (edge_to_col %in% names(current_edge_list)) {
        to_leak_mask <- current_edge_list[[edge_to_col]] %in% leak_sources
        current_edge_list[[edge_to_col]][to_leak_mask] <- leak_sink_name
      }
      if (edge_from_col %in% names(current_edge_list)) {
        from_leak_mask <- current_edge_list[[edge_from_col]] %in% leak_sources
        current_edge_list <- current_edge_list[!from_leak_mask, , drop = FALSE]
      }
    }
    all_vertex_universe <- setdiff(all_vertex_universe, leak_sources)
  }

  # --- 2.8. Prior preparation (TIPR) ---
  # Canonicalize the prior URLs with the SAME rurl settings as the edges and
  # fold them through the SAME redirect map, summing happens later in
  # align_prior_to_vertices(). This puts the prior into the final vertex
  # namespace before the graph is built.
  folded_prior_df <- NULL
  if (!is.null(prior_df) && nrow(prior_df) > 0) {
    folded_prior_df <- prior_df[, c(prior_url_col, prior_weight_col),
      drop = FALSE
    ]
    folded_prior_df[[prior_url_col]] <-
      as.character(folded_prior_df[[prior_url_col]])

    # Canonicalize to match the edge namespace (only when edges were cleaned).
    if (clean_edge_urls) {
      folded_prior_df <- do.call(
        clean_url_columns,
        c(
          list(data_frame = folded_prior_df, columns = prior_url_col),
          effective_rurl_params
        )
      )
    }

    # Fold through the SAME composed map (redirects + canonicals) used for the
    # edges above -- single source of truth, so prior URLs land on the same
    # representatives as the vertices.
    if (length(fold_map) > 0) {
      folded_prior_df[[prior_url_col]] <- .apply_fold_map(
        folded_prior_df[[prior_url_col]], fold_map
      )
    }

    # Under `leak`, route the prior on a leaking source onto the leak sink too,
    # so the source's own teleport equity leaves the measured graph alongside
    # its received equity (the sink is excluded from teleport below).
    if (used_leak_sink && length(leak_sources) > 0) {
      prior_leak_mask <- folded_prior_df[[prior_url_col]] %in% leak_sources
      folded_prior_df[[prior_url_col]][prior_leak_mask] <- leak_sink_name
    }
  }

  # --- Transition audit: account for rows dropped (dedup / NA / self-loops) ---
  # Measured against the post-fold/post-filter edge list, mirroring exactly what
  # get_unique_edges() removes, so the audit reflects the data that actually
  # reached the deduplication step.
  audit_n_rows_na <- 0L
  audit_n_self_loops <- 0L
  audit_n_rows_duplicate <- 0L
  if (nrow(current_edge_list) > 0 &&
        all(c(edge_from_col, edge_to_col) %in% names(current_edge_list))) {
    .pre_from <- as.character(current_edge_list[[edge_from_col]])
    .pre_to <- as.character(current_edge_list[[edge_to_col]])
    .na_mask <- is.na(.pre_from) | is.na(.pre_to)
    audit_n_rows_na <- sum(.na_mask)
    .nn_from <- .pre_from[!.na_mask]
    .nn_to <- .pre_to[!.na_mask]
    .self_mask <- .nn_from == .nn_to
    if (self_loops == "drop") {
      audit_n_self_loops <- sum(.self_mask)
      .nn_from <- .nn_from[!.self_mask]
      .nn_to <- .nn_to[!.self_mask]
    }
    if (length(.nn_from) > 0) {
      .dup_mask <- duplicated(paste0(.nn_from, "\t", .nn_to))
      audit_n_rows_duplicate <- sum(.dup_mask)
    }
  }

  audit_instance_count_col <- NULL
  effective_weight_col <- weight_col
  audit_duplicate_edges <- NULL
  audit_n_duplicate_instances <- 0L

  # --- 3. Apply duplicate-edge policy (handles self-loops) ---
  current_edge_list <- .apply_duplicate_edge_policy(
    edge_list_df = current_edge_list,
    policy = duplicate_edge_policy,
    self_loops = self_loops,
    from_col = edge_from_col,
    to_col = edge_to_col
  )
  if (duplicate_edge_policy == "count_instances") {
    audit_instance_count_col <- "__pr_instance_count__"
    if (is.null(weight_col)) {
      effective_weight_col <- audit_instance_count_col
    }
    audit_duplicate_edges <- .duplicate_edge_audit_rows(
      edge_list_df = current_edge_list,
      from_col = edge_from_col,
      to_col = edge_to_col,
      instance_count_col = audit_instance_count_col,
      weight_col = effective_weight_col
    )
    if (is.data.frame(audit_duplicate_edges) &&
          nrow(audit_duplicate_edges) > 0) {
      audit_n_duplicate_instances <- sum(
        audit_duplicate_edges$instance_count,
        na.rm = TRUE
      )
    }
  }

  # Edges actually scored (after folding, dedup and self-loop handling).
  # Synthetic rows added later (nofollow sink, robots-blocked self-loops) are
  # graph-construction devices, not input edges, so n_edges is fixed here.
  audit_n_edges <- nrow(current_edge_list)

  # --- 3.5. Indexability handling ---
  # Must come after dedup (step 3) but before nofollow (step 3.6) so that

  # noindex-derived nofollow edges are picked up by the nofollow mechanism.
  robots_blocked_urls <- character(0)
  if (!is.null(indexability_df) && nrow(indexability_df) > 0 &&
        nrow(current_edge_list) > 0) {
    statuses <- as.character(indexability_df[[indexability_status_col]])
    urls <- as.character(indexability_df[[indexability_url_col]])

    # Parse statuses: robots.txt takes priority over noindex
    is_robots_blocked <- grepl("Blocked by robots.txt", statuses, fixed = TRUE)
    is_noindex <- grepl("noindex", statuses, ignore.case = TRUE) &
      !is_robots_blocked

    noindex_urls <- urls[is_noindex]
    robots_blocked_urls <- urls[is_robots_blocked]

    # --- noindex pages: mark all outgoing edges as nofollow ---
    if (length(noindex_urls) > 0) {
      from_is_noindex <- current_edge_list[[edge_from_col]] %in% noindex_urls
      if (any(from_is_noindex)) {
        # Create nofollow column if it doesn't exist
        if (is.null(nofollow_col)) {
          nofollow_col <- "__pr_nofollow__"
          current_edge_list[[nofollow_col]] <- FALSE
        } else if (!(nofollow_col %in% names(current_edge_list))) {
          current_edge_list[[nofollow_col]] <- FALSE
        }
        current_edge_list[[nofollow_col]][from_is_noindex] <- TRUE
      }
    }

    # --- robots-blocked pages: remove outgoing edges, add self-loop ---
    if (length(robots_blocked_urls) > 0) {
      from_is_blocked <-
        current_edge_list[[edge_from_col]] %in% robots_blocked_urls
      if (any(from_is_blocked)) {
        # Remove all outgoing edges from blocked pages
        current_edge_list <- current_edge_list[!from_is_blocked, , drop = FALSE]

        # Add self-loops for each blocked URL that exists as a source
        blocked_with_edges <- unique(robots_blocked_urls[
          robots_blocked_urls %in% current_edge_list[[edge_from_col]] |
            robots_blocked_urls %in% current_edge_list[[edge_to_col]] |
            robots_blocked_urls %in% all_vertex_universe
        ])
        if (length(blocked_with_edges) > 0) {
          # Build self-loop rows matching the edge list structure
          self_loop_df <- stats::setNames(
            data.frame(blocked_with_edges, blocked_with_edges
            ),
            c(edge_from_col, edge_to_col)
          )
          # Fill extra columns with appropriate defaults
          extra_cols <- setdiff(
            names(current_edge_list),
            c(edge_from_col, edge_to_col)
          )
          for (col in extra_cols) {
            if (is.logical(current_edge_list[[col]])) {
              self_loop_df[[col]] <- FALSE
            } else if (is.numeric(current_edge_list[[col]])) {
              self_loop_df[[col]] <- 1
            } else {
              self_loop_df[[col]] <- NA
            }
          }
          current_edge_list <- rbind(current_edge_list, self_loop_df)
        }
      }
    }
  }

  # --- 3.6. Nofollow handling ---
  nofollow_sink_name <- "__pr_nofollow_sink__"
  used_nofollow_sink <- FALSE

  if (!is.null(nofollow_col) && nofollow_col %in% names(current_edge_list) &&
        nrow(current_edge_list) > 0) {
    # Coerce nofollow column to logical
    nf_vals <- current_edge_list[[nofollow_col]]
    if (is.numeric(nf_vals)) nf_vals <- as.logical(nf_vals)
    nf_mask <- !is.na(nf_vals) & nf_vals

    if (any(nf_mask)) {
      if (nofollow_action == "drop") {
        # Simply remove nofollow edges
        current_edge_list <- current_edge_list[!nf_mask, , drop = FALSE]
      } else if (nofollow_action == "evaporate") {
        # Redirect nofollow edge targets to the sink node
        current_edge_list[[edge_to_col]][nf_mask] <- nofollow_sink_name

        # Add a self-loop on the sink so it isn't a dangling node
        sink_row <- stats::setNames(
          data.frame(nofollow_sink_name, nofollow_sink_name
          ),
          c(edge_from_col, edge_to_col)
        )
        # Fill extra columns
        extra_cols <- setdiff(
          names(current_edge_list),
          c(edge_from_col, edge_to_col)
        )
        for (col in extra_cols) {
          if (is.logical(current_edge_list[[col]])) {
            sink_row[[col]] <- FALSE
          } else if (is.numeric(current_edge_list[[col]])) {
            sink_row[[col]] <- 1
          } else {
            sink_row[[col]] <- NA
          }
        }
        current_edge_list <- rbind(current_edge_list, sink_row)
        used_nofollow_sink <- TRUE
      }
      # nofollow_action == "keep": do nothing
    }
  }

  # --- 3.7. Leak sink self-loop ---
  # When `out_of_scope_fold = "leak"` routed at least one source onto the leak
  # sink (in step 2.75), add a self-loop so the sink is not a dangling node and
  # the equity that reached it stays trapped (and evaporates when the sink is
  # removed from the results). Added here, after deduplication, so
  # `self_loops = "drop"` cannot strip it -- mirroring the nofollow sink.
  if (used_leak_sink && nrow(current_edge_list) > 0) {
    leak_sink_row <- stats::setNames(
      data.frame(leak_sink_name, leak_sink_name),
      c(edge_from_col, edge_to_col)
    )
    extra_cols <- setdiff(
      names(current_edge_list),
      c(edge_from_col, edge_to_col)
    )
    for (col in extra_cols) {
      if (is.logical(current_edge_list[[col]])) {
        leak_sink_row[[col]] <- FALSE
      } else if (is.numeric(current_edge_list[[col]])) {
        leak_sink_row[[col]] <- 1
      } else {
        leak_sink_row[[col]] <- NA
      }
    }
    current_edge_list <- rbind(current_edge_list, leak_sink_row)
  }

  # --- 4. Handle Isolates ---
  # After nofollow/indexability steps, the edge list may contain new nodes

  # (e.g., __pr_nofollow_sink__, robots-blocked self-loops). Include them.
  current_edge_nodes <- unique(c(
    as.character(current_edge_list[[edge_from_col]]),
    as.character(current_edge_list[[edge_to_col]])
  ))
  current_edge_nodes <- current_edge_nodes[!is.na(current_edge_nodes)]

  vertices_for_pagerank_df <- NULL

  if (drop_isolates_flag) {
    # Only keep nodes that participate in at least one complete edge.
    if (length(current_edge_nodes) > 0) {
      vertices_for_pagerank_df <- stats::setNames(
        data.frame(sort(current_edge_nodes)),
        temp_node_col_name
      )
    }
  } else {
    # Keep all known nodes: original vertex universe PLUS any nodes
    # introduced by nofollow/indexability steps (sink node, etc.)
    full_universe <- unique(c(all_vertex_universe, current_edge_nodes))
    if (length(full_universe) > 0) {
      vertices_for_pagerank_df <- stats::setNames(
        data.frame(sort(full_universe)),
        temp_node_col_name
      )
    }
  }

  # --- 4.5. Inject unmatched prior URLs as isolates (opt-in) ---
  # Authoritative URLs that don't fold onto any existing vertex are surfaced as
  # edge-less vertices (carrying their teleport prior, distributing nothing).
  if (!is.null(folded_prior_df) && prior_inject_unmatched &&
        !is.null(vertices_for_pagerank_df)) {
    existing_nodes <- vertices_for_pagerank_df[[temp_node_col_name]]
    prior_dests <- unique(stats::na.omit(folded_prior_df[[prior_url_col]]))
    to_add <- setdiff(prior_dests, existing_nodes)
    if (length(to_add) > 0) {
      vertices_for_pagerank_df <- stats::setNames(
        data.frame(sort(c(existing_nodes, to_add))),
        temp_node_col_name
      )
    }
  }

  # The synthetic nofollow and leak sinks are excluded from teleport; robots/404
  # self-loop nodes are real pages and keep their authority.
  prior_exclude_nodes <- character(0)
  if (used_nofollow_sink) {
    prior_exclude_nodes <- c(prior_exclude_nodes, nofollow_sink_name)
  }
  if (used_leak_sink) {
    prior_exclude_nodes <- c(prior_exclude_nodes, leak_sink_name)
  }

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
  # synthetic nofollow-evaporation sink and any robots-blocked nodes that the
  # caller asked to vanish — so it sums to 1 by construction. The visible
  # result only carries real, reported pages, so its scores can sum to < 1.
  # Before dropping the internal nodes we measure how much stationary mass
  # each carried away, so the difference is fully accounted for rather than
  # written off as undifferentiated "leakage":
  #   * evaporated mass = stationary mass parked on the nofollow sink (the
  #     authority a source wasted on nofollowed outlinks);
  #   * leaked mass     = stationary mass parked on the leak sink (the authority
  #     that flowed into out-of-scope-folded sources under
  #     `out_of_scope_fold = "leak"`, treated like an external redirect);
  #   * hidden mass      = stationary mass trapped on robots-blocked nodes that
  #     were removed under `robots_blocked_action = "vanish"`.
  # Captured here, fed into the transition audit's mass$ fields in step 7.
  mass_evaporated <- 0
  mass_leaked <- 0
  mass_hidden <- 0
  if (nrow(pagerank_results) > 0) {
    pr_node_col <- names(pagerank_results)[1]
    pr_value_col <- names(pagerank_results)[2]

    # Remove nofollow sink node (measure its evaporated mass first)
    if (used_nofollow_sink) {
      sink_mask <- pagerank_results[[pr_node_col]] == nofollow_sink_name
      mass_evaporated <- sum(pagerank_results[[pr_value_col]][sink_mask],
        na.rm = TRUE
      )
      pagerank_results <- pagerank_results[!sink_mask, , drop = FALSE]
    }

    # Remove leak sink node (measure its leaked mass first)
    if (used_leak_sink) {
      leak_sink_mask <- pagerank_results[[pr_node_col]] == leak_sink_name
      mass_leaked <- sum(pagerank_results[[pr_value_col]][leak_sink_mask],
        na.rm = TRUE
      )
      pagerank_results <- pagerank_results[!leak_sink_mask, , drop = FALSE]
    }

    # Remove robots-blocked nodes if vanish action (measure hidden mass first)
    if (robots_blocked_action == "vanish" && length(robots_blocked_urls) > 0) {
      hidden_mask <- pagerank_results[[pr_node_col]] %in% robots_blocked_urls
      mass_hidden <- sum(pagerank_results[[pr_value_col]][hidden_mask],
        na.rm = TRUE
      )
      pagerank_results <- pagerank_results[!hidden_mask, , drop = FALSE]
    }

    row.names(pagerank_results) <- NULL
  }

  # --- 7. Transition audit / provenance object ---
  # Assembled from the counts captured along the path above and attached to the
  # result as an attribute (attr(result, "transition_audit")). The attribute
  # approach is backward-compatible: the return value is still the same data
  # frame with the same columns, so existing callers and tests are unaffected,
  # while reproducibility metadata travels alongside the result.

  # Behavioral-weight coverage: how many scored edges carry a usable weight.
  audit_weighted <- !is.null(effective_weight_col) &&
    effective_weight_col %in% names(current_edge_list)
  audit_n_edges_weighted <- 0L
  if (audit_weighted && nrow(current_edge_list) > 0) {
    .w <- suppressWarnings(
      as.numeric(current_edge_list[[effective_weight_col]])
    )
    audit_n_edges_weighted <- sum(!is.na(.w) & is.finite(.w) & .w > 0)
  }

  # Authority-prior URLs that never folded onto a vertex (unmatched).
  audit_n_prior_unmatched <- NA_integer_
  if (!is.null(folded_prior_df) && !is.null(vertices_for_pagerank_df)) {
    .final_nodes <- vertices_for_pagerank_df[[temp_node_col_name]]
    .prior_dests <- unique(stats::na.omit(folded_prior_df[[prior_url_col]]))
    audit_n_prior_unmatched <- length(setdiff(.prior_dests, .final_nodes))
  }

  audit_pagerank_total <- if (nrow(pagerank_results) > 0 &&
                                ncol(pagerank_results) >= 2) {
    sum(pagerank_results[[2]], na.rm = TRUE)
  } else {
    NA_real_
  }

  # Out-of-scope fold list (source, target, signal) for the audit `fold`
  # section; NULL when there were no out-of-scope folds.
  audit_oos_fold_df <- if (length(audit_oos_sources) > 0) {
    data.frame(
      source = audit_oos_sources,
      target = audit_oos_targets,
      signal = audit_oos_signals,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  transition_audit <- new_transition_audit(
    n_input_rows = audit_n_input_rows,
    n_edges = audit_n_edges,
    n_vertices = nrow(pagerank_results),
    weighted = audit_weighted,
    weight_col = if (audit_weighted) effective_weight_col else NULL,
    n_edges_weighted = audit_n_edges_weighted,
    duplicate_edge_policy = duplicate_edge_policy,
    instance_count_col = audit_instance_count_col,
    n_duplicate_instances = audit_n_duplicate_instances,
    duplicate_edges = audit_duplicate_edges,
    n_rows_na = audit_n_rows_na,
    n_rows_duplicate = audit_n_rows_duplicate,
    n_self_loops = audit_n_self_loops,
    n_prior_unmatched = audit_n_prior_unmatched,
    n_robots_blocked = length(robots_blocked_urls),
    pagerank_total = audit_pagerank_total,
    mass_reported = audit_pagerank_total,
    mass_evaporated = mass_evaporated,
    mass_leaked = mass_leaked,
    mass_hidden = mass_hidden,
    out_of_scope_fold = out_of_scope_fold,
    n_out_of_scope_folds = length(audit_oos_sources),
    out_of_scope_folds_applied = audit_oos_applied,
    out_of_scope_fold_list = audit_oos_fold_df,
    fold_collisions = audit_collisions_df,
    config = list(
      self_loops = self_loops,
      drop_isolates_flag = drop_isolates_flag,
      reverse = reverse,
      weight_col = weight_col,
      effective_weight_col = effective_weight_col,
      duplicate_edge_policy = duplicate_edge_policy,
      nofollow_col = if (identical(nofollow_col, "__pr_nofollow__")) {
        NULL
      } else {
        nofollow_col
      },
      nofollow_action = nofollow_action,
      robots_blocked_action = robots_blocked_action,
      prior_alpha = prior_alpha,
      prior_transform = prior_transform,
      prior_inject_unmatched = prior_inject_unmatched,
      has_redirects = isTRUE(audit_has_redirects),
      has_canonicals = isTRUE(audit_has_canonicals),
      has_indexability = !is.null(indexability_df) &&
        nrow(indexability_df) > 0,
      has_prior = !is.null(folded_prior_df)
    )
  )

  attr(pagerank_results, "transition_audit") <- transition_audit
  attr(pagerank_results, "convergence") <- convergence

  pagerank_results
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
.sf_folded_away_filter_values <- function(prefold_nodes,
                                          postfold_nodes,
                                          domain_values,
                                          host_values,
                                          rurl_params) {
  present <- function(nodes, value, type) {
    if (length(nodes) == 0) {
      return(FALSE)
    }
    probe_df <- data.frame(
      from = nodes, to = nodes, stringsAsFactors = FALSE
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

  keep_present <- function(values) {
    values <- values[!is.na(values) & nzchar(values)]
    unique(values)
  }
  domain_values <- keep_present(domain_values)
  host_values <- keep_present(host_values)

  folded_away <- character(0)
  for (value in domain_values) {
    if (present(prefold_nodes, value, "domain") &&
          !present(postfold_nodes, value, "domain")) {
      folded_away <- c(folded_away, value)
    }
  }
  for (value in host_values) {
    if (present(prefold_nodes, value, "host") &&
          !present(postfold_nodes, value, "host")) {
      folded_away <- c(folded_away, value)
    }
  }
  unique(folded_away)
}

#' Apply pagerank() duplicate-edge policy.
#' @keywords internal
#' @noRd
.apply_duplicate_edge_policy <- function(edge_list_df,
                                         policy,
                                         self_loops,
                                         from_col,
                                         to_col) {
  if (policy == "collapse") {
    return(get_unique_edges(
      edge_list_df = edge_list_df,
      self_loops = self_loops,
      from_col = from_col,
      to_col = to_col
    ))
  }

  if (policy == "count_instances" &&
        nrow(edge_list_df) > 0 &&
        all(c(from_col, to_col) %in% names(edge_list_df))) {
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
.duplicate_edge_audit_rows <- function(edge_list_df,
                                       from_col,
                                       to_col,
                                       instance_count_col,
                                       weight_col) {
  if (is.null(instance_count_col) ||
        !(instance_count_col %in% names(edge_list_df)) ||
        nrow(edge_list_df) == 0) {
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
