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
- **Two crawls now exist**, straddling a shipped internal-linking intervention:
  a **pre-intervention** crawl (the one the numbers throughout §1–§9 were
  computed from) and a **post-intervention** crawl taken after a de-sink /
  conduit epic that carried out the recommendations in §6 and §8. The measured
  before/after is written up in §10. (Provenance note: the pre-intervention
  crawl CSVs were recoverable from the site repo's git history even after the
  working copies were overwritten — crawl exports stored under fixed filenames
  should be committed or archived per-date so a baseline is never lost.)

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
inlinks — and often you *can't* cut them (compliance). **This recommendation was
later shipped and re-measured — see §10.**

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
do not assert it. **The follow-up test of this method — did a shipped
intervention move the metrics the way the model predicted? — is §10.**

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

## 10. Closed loop: the modeled de-sink, shipped and re-measured

Every finding above is a *snapshot*. This one is a *diff across an
intervention*: the site shipped the de-sink recommended in §6 (turn the two
compliance sinks into conduits) plus the starved-page feeding modeled in §8
(section-index links into commercial pages), and a second crawl was taken. This
is the rare closed loop — diagnose → model → ship → **re-measure on a real
crawl** — that separates a method from an anecdote.

**The recovered baseline reproduces the notes exactly.** The pre-intervention
crawl scored `/about/methodology/` at editorial PR **0.307** and
`/about/affiliate-disclosure/` at **0.237** — bit-for-bit the §4 figures. So the
before/after below is measured on the same instrument that produced §1–§9, not a
re-derivation.

**Editorial (content-only) graph — the sinks drained, the money pages filled.**

| page | editorial PR: before → after | Δ | rank |
|------|------------------------------:|----:|------|
| `/about/methodology/`          | 0.307 → 0.007 | **−97.7%** | #1 → #40 |
| `/about/affiliate-disclosure/` | 0.237 → 0.010 | −95.9% | #2 → #27 |
| `/about/contact/`              | 0.138 → 0.006 | −95.4% | #3 → #48 |
| `/about/faq/`                  | 0.042 → 0.010 | −75.9% | #4 → #26 |
| `/pricing/`                    | 0.015 → 0.064 | **+325%**  | #6 → #2 |
| `/pricing/free-plan/`          | 0.005 → 0.061 | +1096% | #17 → #3 |
| `/features/ai-agent/`          | 0.006 → 0.045 | +705%  | #13 → #5 |
| `/pricing/free-trial/`         | 0.004 → 0.041 | +996%  | #26 → #7 |
| homepage `/en-us/`             | 0.003 → 0.011 | +270%  | #48 → #23 |

Concentration collapsed exactly as a de-sink predicts: editorial **Gini
0.763 → 0.472**, **top-5 share 76.2% → 31.1%**, entropy 2.53 → 3.75. Authority
that four sitewide in-content hubs were hoarding got spread across the discretionary
graph, landing disproportionately on commercial pages. Every money page gained;
none regressed.

**Mechanism — the byline reclassification (a placement gotcha worth its own
line).** The single biggest lever was *not* new links but **re-placement**: the
sitewide methodology/disclosure byline links were moved into a semantic
`<nav aria-label="Editorial standards">`, so Screaming Frog reclassifies them
from `Content` to `Navigation`. They simply **leave the editorial graph**
(content edges fell 339 → 232). Corollary to §4: in-content boilerplate is a
second nav — and the fix can be to *make the HTML say so*, which is both correct
semantics and the cheapest possible de-sink.

**Full graph — insensitive, as §2/§9 warned.** The full graph barely twitched:
Gini 0.305 → 0.273, Pearson(before, after) = 0.9999, every commercial page
within **+0.69% to +0.76%** (numerical noise on a near-uniform vector). The
entire intervention is an *editorial-graph* phenomenon, invisible to the
boilerplate-dominated full graph. This is the clearest single confirmation of §9:
the lens you choose decides whether you can even see the change.

**Honest confounds (state these in any writeup).** This is a two-crawl natural
experiment, not a controlled `simulate_changes()` on one fixed graph, so:
- The node set changed — 4 pages were retired/merged during the epic
  (`/submit/`, `/company/funding/`, `/company/headquarters/`, `/tidio/`), and
  total editorial edges fell (339 → 232). **Absolute editorial PR levels are
  therefore not directly comparable across crawls; read rank shifts,
  concentration metrics (Gini / top-share / entropy), and relative deltas** — the
  same "relative before/after lens" discipline §9 argues for.
- The effect blends two moves: (a) the byline content→nav reclassification and
  (b) genuine new conduit/feeder links. The re-measure captures the *net*, which
  is what shipped; attributing shares between them needs the per-unit models
  (which existed and pointed the same direction).

**Lesson.** A `simulate_changes()` projection and a post-ship re-crawl agreed in
*direction and rough magnitude* — the model was decision-useful, not just
decorative. That agreement, on a real site, is the strongest claim the toolkit
can make: **the views are diagnostic, the models are predictive, and the loop
closes.**

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
- **Close the loop: does the model predict the re-crawl?** — the strongest hook.
  Diagnose sinks → model a fix with `simulate_changes` → ship it → re-crawl →
  show the projection and the measured before/after agree (§10). Positions
  `pagerankr` as *predictive*, not just descriptive, and gives the "living /
  multi-site" paper its template: one section per site, each an intervention
  with a modeled projection and a re-measured outcome. Also seeds a
  **placement-as-intervention** sub-angle (the byline content→nav
  reclassification as the cheapest de-sink) and a **methods caveat** on
  two-crawl natural experiments (read rank/concentration/relative deltas, not
  absolute PR).

---

## 11. Uniform teleport pays pages for existing — dead pages can capture 95% of a site

*Synthetic experiment, not a field observation. Reproduce with
`notes/experiments/teleport-dead-pages.R`.*

Setup: a 21-page site (hub + 20 interlinked pages), plus **K fake dead URLs**
each discovered via exactly one link from the hub — the realistic shape, since a
crawler only finds a URL because something links to it. The dead pages have no
outlinks, which is what a 404 actually looks like.

Under the **uniform teleport vector** (current behavior), dead pages are ordinary
dangling nodes:

| K dead URLs | dead pages hold | real content holds | hub |
|---|---|---|---|
| 0 | 0% | **69.7%** | 0.303 |
| 10 | 16.6% | 57.8% | 0.255 |
| 100 | 66.5% | 23.1% | 0.104 |
| 1000 | **95.2%** | **3.3%** | 0.015 |

A thousand fake broken URLs capture **95% of the site's PageRank** and collapse
real content 21x. Nobody linked to them except one hub link each.

**Excluding dead pages from the teleport vector** makes it plateau instead of
running away:

| K | dead pages hold | real content holds | teleport's share of a dead page's score |
|---|---|---|---|
| 10 | 8.0% | 63.9% | 52.0% |
| 100 | 18.0% | 56.5% | 72.9% |
| 1000 | **20.6%** | **54.6%** | **78.3%** |

At K=1000, **78% of a dead page's score is mass it received for existing.**

Note the residual decline under exclusion (69.7% -> 54.6%): that part is
**correct and should be reported**. The hub really is spending its outgoing link
budget on 1000 broken links. What exclusion removes is the manufactured collapse
layered on top of it.

**Convergence with zeroed teleport entries** — the one thing the literature does
not address directly — checks out at all three sizes: scores sum to exactly 1,
no `NA`, no negatives, minimum score strictly positive.

### Why this is not a deviation from PageRank

Page & Brin (1998, §6) treat the teleport vector `E` as a free parameter and
criticize the uniform choice in these words: *"This is a very democratic choice
for E since all web pages are valued simply because they exist."* They test `E`
concentrated on a **single page**. Sparse teleport is authorial intent, not a
departure.

Bianchini, Gori & Scarselli, *Inside PageRank* (2005) formalize the inflation:
a group of pages carries a **"default energy" equal to its page count**, and
their stated "golden rule" is that *"the same content divided into many small
pages yields a higher score than the same content into a single large page."*
Adding K junk URLs harvests K units of default energy. Langville & Meyer report
Google itself tinkered with teleport-vector elements to annihilate link farms.

### Dangling nodes: every option invents something

A page with no outlinks is undefined in the formula — its mass is divided among
zero destinations. There is no invention-free choice, only three inventions:

| Treatment | What it claims about the site |
|---|---|
| **Dangle** (today) | authority flowing into 404s is spread evenly across every page |
| **Self-loop** | the 404 links to itself forever, compounding |
| **Sink** | authority flows into 404s and stops there |

Only the third is true. Langville & Meyer name the second: a self-absorbing
dangling node is a **rank sink / absorbing state** that *"keeps accumulating more
and more PageRank at each iteration."* Measured in `pagerankr`: identical graph,
page X under `noindex` (evaporate) scores **0.1065**; under robots-blocked
(trap/self-loop) it scores **0.8875** — 8.3x, holding 89% of the graph.

See fp `PAGE-qzskzcfd` for the resulting policy decisions.
