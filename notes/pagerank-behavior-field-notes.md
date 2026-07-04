# PageRank behavior: field notes

Empirical and conceptual findings about how PageRank behaves on real crawls,
gathered while exercising `pagerankr` against production Screaming Frog exports.
These notes are raw material for a promotional / applied paper (see the
"Paper: pagerankr" fp issue) and for future vignettes. Numbers below come from a
live case study unless stated otherwise.

## Case study: `tidioreviews` (a 67-page reviews microsite)

- Input: Screaming Frog **Internal: All** (92 rows) + **All Inlinks** (~4,440
  rows), crawled on the staging host `tidioreviews.pages.dev`.
- After the bundle adapters: 3,820 hyperlink edges, ~2,900 scored edges over
  67 internal pages.
- The crawl carried a wrinkle that turned into the most instructive finding:
  every page's `rel=canonical` pointed to the **production** host
  `tidioreviews.com`, which was never crawled.

---

## 1. A cross-domain canonical fold can relabel the whole graph — and the result looks real by happenstance

**What happened.** `pagerankr` models both 3xx redirects and `rel=canonical`
through the *same* fold-map engine (`.compose_fold_map` →
`.apply_map_to_edge_list`): a canonical is a URL rewrite applied to *both*
endpoints of every edge. The 67 canonicals formed a clean 1:1
`pages.dev → .com` bijection with matching paths, so folding was a graph
**isomorphism**: PageRank was computed on the `pages.dev` topology and every
vertex was *renamed* to its `.com` twin.

**The proof it was happenstance.** Replacing the real cross-domain canonicals
with self-canonicals (`pages.dev → pages.dev`) yields **bit-identical**
PageRank (max |Δ| = 0.000e+00, identical ranking); only the host label changes.
Nothing about `.com` was measured — no `.com` page was crawled or had its link
graph observed. The reported `.com` PageRank was the `pages.dev` structure
wearing a `.com` nametag.

**Why it matters (design gap).** The fold engine is **scope-blind**: it folds
to targets outside the crawl scope, silently synthesizing phantom nodes. A
cross-domain canonical to an uncrawled site is semantically *not* a redirect
("this URL is gone") — it is advisory ("this 200-returning page defers to a
page I cannot see"). Modeling it as a hard node-identity merge is only correct
if the crawled host is a faithful mirror of the target, which the tool *assumes*
rather than establishes.

**Verified failure modes** (repro in the `SF-scope` fp issues):
- **Crawled-domain erasure.** After folding, filtering on the domain you
  actually crawled (`keep_domains = "tidioreviews.pages.dev"`) returns **0
  nodes**; you must filter on a domain never present in the input.
- **Collision → real corruption.** If the crawl contains any genuine link to
  the canonical *target* domain, that external link **merges into** the
  relabeled internal node (no new node created). Forced repro: 5 real links to
  prod `/website/` raised the *internal* node's PR **+9.3%**. Did not bite this
  crawl only because it links to `www.tidio.com`, never `tidioreviews.com`.
- **Ruled out:** partial canonicalization does *not* duplicate a page into two
  nodes — the fold is a consistent per-URL rewrite, so each page keeps one
  identity (mixed hostnames only).

**Takeaway.** Redirect and canonical are not the same signal. A redirect means
the URL moved; a cross-domain canonical to an out-of-scope target is a hint, and
folding it is a modeling choice that should be explicit, scoped, and warned
about — not silent.

---

## 2. On a real crawl, PageRank mostly measures the navigation template

The full-graph PageRank of `tidioreviews` was nearly **flat**: the top ~55
pages all sat at ~2.22% with a Gini of 0.31 and an entropy ratio of 0.94
(near-uniform). The cause is structural, not editorial:

| Placement | Internal edges | Share |
|-----------|---------------:|------:|
| nav       | 3,204          | 86%   |
| content   | 463            | 12%   |
| header    | 67             | 2%    |

Every page casts the same ~46 nav votes, so PageRank cannot differentiate them.
**A naive full-crawl PageRank is largely a readout of the site template.** This
is the single most important caveat for anyone using internal PageRank for SEO:
the boilerplate dominates.

---

## 3. Placement weighting (a reasonable-surfer proxy) recovers editorial signal — and *reduces* concentration

Downweighting nav (`placement_weights = c(content=1, nav=0.1, header=0.1)`)
reshuffles the ranking hard. Pages strong in body copy but absent from the nav
climb (e.g. `/alternatives/zendesk/` #40 → #9); pages propped only by the menu
fall (`/alternatives/tawk/` #9 → #38).

Counterintuitively, **Gini drops** (0.305 → 0.264). The nav was *manufacturing*
artificial concentration on its ~46 favored pages; editorial linking is spread
more evenly. Lesson: the template can make a site look more (or less)
"focused" than its actual content architecture is.

---

## 4. Editorial (content-only) PageRank is hyper-concentrated — because in-content boilerplate is a second nav

Recomputing PageRank on **content edges only** inverts the flatness into extreme
concentration: two pages, `/about/methodology/` (30.7%) and
`/about/affiliate-disclosure/` (23.7%), hold **54%** of all editorial authority.

The reason is a *second* layer of boilerplate hiding inside article bodies: on a
reviews site, every review links to "our methodology" and the affiliate
disclosure (an FTC-compliance link). Both pages have editorial **in-degree 67
(every page), out-degree 2–3** — near-perfect sinks.

**Lesson.** Stripping the template nav is not enough to see genuine editorial
intent. Sitewide *in-content* links (compliance, methodology, "contact us")
behave exactly like nav and must also be excluded to find the truly
discretionary link graph.

---

## 5. CheiRank surfaces disconnected hubs — but high CheiRank ≠ valuable

CheiRank (PageRank on the reversed graph, `reverse = TRUE`) ranks pages by
*outflow*. On `tidioreviews` the top CheiRank pages were also the **lowest**
PageRank pages: `/website/`, `/company/headquarters/`, `/download/app/` — pages
that link out generously but receive almost nothing.

The trap: CheiRank rewards outbound *volume*, and every page ships the same
footer, so a big chunk of every page's CheiRank is boilerplate it carries by
template. High CheiRank on a thin utility page is close to noise.

**The right metric for "should this page exist" is editorial in-degree** — how
many pages *chose*, in body content, to link to it. `/company/headquarters/`
had editorial in-degree **2** (near-floor PR, out-degree 11): it references
others generously but almost nothing references it. That is the signal that a
page earns no editorial interest — not its PageRank, and certainly not its
CheiRank.

**Seeded feeders need a clean graph.** `topic_feeder_pagerank()` (seeded reverse
PageRank, "what feeds this cluster") returned the *global* hubs when run on all
edges — the nav is so uniform that every page feeds every cluster. Only on
**content-only** edges did the genuine topical feeders appear (for the AI-Agent
cluster: the product overview, pricing, and sibling feature pages). Topic-feeder
analysis is only meaningful after boilerplate is removed.

---

## 6. Sinks hoard; conduits recirculate; nofollow no longer redistributes

A page hoards PageRank when it has many inlinks and few outlinks (high in/out
ratio). `/about/affiliate-disclosure/`: in=67, out=2, ratio 33.5. The fix for a
hoarder is to turn the sink into a conduit (add relevant outlinks), *not* to cut
inlinks — and often you *can't* cut them (compliance).

Note for practitioners: **`nofollow` will not redistribute equity to money
pages.** Since Google's 2019 change, nofollow is a hint and the equity
*evaporates* rather than flowing to a page's other links — classic PageRank
sculpting is dead. In modeling terms this is `nofollow_action = "evaporate"`,
and it shows up in the mass-accounting report as sink/hidden mass, not as a
transfer.

---

## 7. Low PageRank on a low-value page is the system working, not a leak

An important framing correction. It is tempting to call every low-PR page an
"under-linked" problem, but conversion/utility pages (`/website/signup/`,
`/website/login/`, `/download/*`) *should* have low PageRank — you do not want
them ranking. The graph declining to spend equity on them is correct.

The actionable cases are the **gap** between what the graph does and what intent
says it should do:
- **High intent, low PR** → a money page the internal graph is starving. Fix it.
- **Low intent, low PR** → correct; leave it (and if it is also thin with
  near-zero editorial in-degree, consider consolidation — a content decision,
  not a PageRank one).

**PageRank tells you what the graph *does*; only intent tells you what it
*should* do. Act on the gap.**

---

## 8. "What-if" modeling is cheap and quantifiable

`simulate_changes()` (add/remove edges, add redirects) lets you model an
architecture change on the stored graph before touching the site. Example: on
`tidioreviews`, adding **3 body-content links** to each of the 4 most-orphaned
pages raised their PR by **+28% to +40%** while costing the donor hubs
**−0.38% each** — near-free authority redistribution. This is the honest way to
justify (or reject) an internal-linking recommendation: show the redistribution,
do not assert it.

---

## 9. PageRank is a lens, not the deliverable

Every finding above is scoped to a *view* of the graph (full, content-only,
reversed, weighted). None of them is what Google computes — Google sees the full
graph plus external signals. The `/about/affiliate-disclosure/` "dominance"
(23.7% of editorial PR) shrinks to full-graph **#40** in reality. And on a
67-page site, internal PageRank sculpting is low-leverage regardless.

Use PageRank views as **diagnostics that pose questions** ("why is this
money page starving?", "is this thin page earning any editorial interest?"), and
resolve those questions with intent, content quality, and — where it matters —
a modeled `simulate_changes()` before/after. The value of the toolkit is that it
makes each of these views a one-liner and keeps the provenance/mass-accounting
honest.

---

## Paper hooks (candidate angles)

- **The canonical-vs-redirect distinction in link-graph modeling**, with the
  "happenstance PageRank" cautionary tale — scope-aware folding as a
  contribution (ties `pagerankr` to the `rurl` URL-canonicalization stack).
- **"Your PageRank is mostly your navigation"** — placement-aware PageRank and
  the reasonable-surfer proxy as the fix; the double-boilerplate problem
  (template nav + in-content compliance links).
- **A reproducible internal-link audit workflow in R** — Screaming Frog bundle →
  placement-weighted PageRank + CheiRank + `simulate_changes`, with mass
  accounting and provenance, on a real site.
