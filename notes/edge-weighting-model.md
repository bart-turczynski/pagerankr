# Edge weighting model: placement, boilerplate, position

Design notes from the 2026-07-20 session. Companion to
`notes/pagerank-behavior-field-notes.md` — the field notes record *what a real crawl did*,
this file records *the model we think explains it* and the decisions that follow.

Status: design agreed and validated against three real crawls (§10). **§4 (placement) is
implemented** — PRs #127 and #128, `PAGE-ktatjtta` done. §5 (boilerplate detection) and the
position axis are still design only.
Tickets: see "Where this lives" at the end.

---

## 1. The framing problem: three things we had been treating as one

Most of the confusion in this area came from one conflation. We had been using
"content", "editorial", and "organic" more or less interchangeably, and separately
we had started thinking about link order. Pulling them apart:

| Axis | Question it answers | Observable from |
|---|---|---|
| **Placement / region** | Where on the page does the link sit? | Markup region (`nav`, `header`, `footer`, `aside`, else content) |
| **Boilerplate** | Is this link a template artifact or a discretionary choice? | Recurrence of the same source context across pages |
| **Position** | Where in the reading order does the link sit? | Order within the page |

The historical accident: the first time we discussed in-content links we called them
"editorial", meaning simply "not nav". That stuck, and it quietly imported a claim —
*editorial* means **discretionary**, and a link being in the content region does not
make it discretionary. A recycled CTA in a blog post template sits in the content
region and is pure boilerplate. We did not notice this until the test site surfaced it.

**Placement does not determine editorial intent.** That is the whole reason the
boilerplate axis has to exist separately.

---

## 2. Correction: boilerplate and placement are ONE axis, not two

The first instinct was to multiply three factors: `placement × boilerplate × position`.
That is wrong, because placement and boilerplate are not independent. A nav link is
boilerplate *by construction* — under our own definition, boilerplate **must** rely on
HTML placement and **may** additionally include repetitive faux-editorial links.
Multiplying both discounts the same link twice for the same fact (`0.1 × 0.5 = 0.05`)
and produces a number nobody can explain.

So boilerplate is a **single graded axis fed by two detectors**:

| Edge | Detected via | Weight |
|---|---|---|
| nav / header / footer | placement (region) | 0.1 |
| repetitive in-content | recurrence (`link_path`) | 0.5 |
| unique in-content | neither | 1.0 |

Position is then genuinely orthogonal — it is about reading order, not templatedness —
and composes multiplicatively:

```
edge_weight = boilerplate_weight × position_weight
```

**Two numbers, not three.**

### Why multiplicative composition is right

The reasonable-surfer sanity check falls out of the arithmetic rather than needing a rule:

| Edge | Computation | Weight |
|---|---|---:|
| Boilerplate-y CTA above the fold | `0.5 × 1.0` | 0.50 |
| Organic link in the last paragraph | `1.0 × 0.2` | 0.20 |
| Nav link at the top | `0.1 × 1.0` | 0.10 |

The above-the-fold CTA outranks the trailing organic link. That is the correct
reasonable-surfer intuition — a real user is far more likely to click the former —
and we get it without special-casing.

### Two constraints on the implementation

- **Keep a floor above zero.** Compounding factors must never reach 0, or "effectively
  dropped" sneaks back in through the back door (see §3).
- **Store the factors, not just the product.** An audit must be able to explain *why*
  an edge weighs 0.02. Persisting only the product makes the output unauditable.

---

## 3. Downweight, never drop

The `content_only` recipe in the field notes (§3) uses `c(content=1, nav=0.1, header=0.1)`
rather than filtering nav edges out. This was originally an intuition; it has two solid
justifications.

**1. Topology.** Dropping edges changes the graph's shape; downweighting only changes
transition probabilities. On the `tidioreviews` crawl nav was 86% of edges. Hard-filter
those and pages whose only inbound links were nav become unreachable (teleport mass only),
while pages whose only outbound links were nav become dangling nodes subject to the sink
policy. At that point you have stopped measuring the site and started measuring your own
sink-handling defaults. At 0.1 the graph keeps its shape and nav simply stops dominating.

**2. Heuristic tolerance.** Placement classification is a residual heuristic, not ground
truth (§4). A content link misclassified as nav and downweighted to 0.1 is a small error.
The same link dropped is a silent deletion that never appears in the output.

**Naming consequence.** `content_only` describes precisely the operation we are *not*
performing, and field-notes §4 is a warning against that operation. The preset must be
renamed. See §7.

---

## 4. Placement vocabulary

### Crawler-agnosticism

Placement is **not** a Screaming Frog concept. SF is one adapter among several — users may
have Sitebulb, a bespoke Python crawler, or Cloudflare's `/crawl` endpoint. The mechanism
belongs at the `pagerank()` argument level, with per-crawler normalizers mapping vendor
vocabulary into ours.

This is cheap because `pagerank()` already has a generic `weight_col`. Everything
SF-specific in the current placement path is a thin adapter: `.sf_apply_placement_weights()`
maps a categorical column to a numeric weight column and hands `pagerank()` a `weight_col`.
Promoting it is moving helpers up a layer, not inventing a mechanism.

### The vocabulary

Five terms: **`content` · `nav` · `header` · `footer` · `aside`**

Changes from today: `sidebar` → `aside` only. SF's own label is already "Aside", and
`sidebar` is a layout word rather than a semantic one.

**`content` is explicitly a residual bucket** — "not classified as nav/header/footer/aside" —
and stays named `content` precisely *because* it makes no tag claim. `main` was rejected: it
would upgrade a leftover into a positive assertion, and on a div-soup site with no semantic
markup everything lands there. If a crawler ever positively detects `<main>`, we add `main`
as a distinct sixth term. "Not chrome" and "inside `<main>`" are different facts and it is
fine to be able to state both.

**`<article>` rejected**, but not for the reason first proposed. The objection raised was
that category pages legitimately carry multiple `<article>` tags and we would end up writing
`if_else` chains to pick the main one. True, but irrelevant: for link *placement* we never
need to identify *the* article. We only need "is this link inside contentful markup", and
every `<article>` answers yes. Multiplicity is a page-summarization problem, not a placement
one. So `article` collapses into `content` for free if we ever want it — it stays out of
scope for now, but cheaply.

**`<aside>` kept as its own term**, not folded into non-contentful. In the wild it is pull
quotes and related-post modules, frequently nested inside `<main>`. Related-links modules are
exactly the links an analyst would most want to tune. Keeping `aside` separate means the
analyst assigns its weight; folding it in bakes a guess into the package and discards
information SF already gives us.

Other semantic tags considered and rejected (`details`, `figure`, `figcaption`, `mark`,
`section`, `summary`, `time`): inline or structural rather than layout regions, and no
crawler emits them as positions.

**A crisp vocabulary does not make detection crisp — and that is the point.** Plenty of real
sites are div soup. What a fixed vocabulary buys is that the fuzziness lives in a per-crawler
normalizer, where it is inspectable and swappable, instead of leaking into the weighting math.

### Derive the region from the DOM path, not from `Link Position`

**SF's `Link Position` discards the enclosing region when a `<nav>` is nested inside it.**
Measured on tidioreviews, cross-tabbing DOM path against `Link Position`:

| path region | SF: Navigation | SF: Header | SF: Footer |
|---|---:|---:|---:|
| `//body/footer/…` | **248** | 0 | **0** |
| `//body/header/…` | 2,666 | 62 | 0 |

The site's markup is `footer > nav > a`:

```html
<footer class="site-footer"><div class="footer-inner">
  <nav aria-label="Footer" class="footer-links"> … </nav>
</div></footer>
```

Every one of those 248 footer links reports as `Navigation`, and **the site has no `Footer`
bucket at all**. The same happens to the header: 2,666 of 2,728 header links report as
`Navigation`. The `<footer>` is simply not recoverable from that column — but the DOM path has
it unambiguously.

**Rule: derive the region from `link_path`; use `Link Position` only as a fallback when no path
is available.** Nesting needs an explicit precedence:

> The region is the **outermost** layout container — `header`, `footer`, `aside`, else `content`.
> `nav` applies only when the link sits in a `<nav>` that is *not* inside one of those.

So footer-nav → `footer`, header-nav → `header`, standalone nav → `nav`. This does not change the
default recipe (all three are 0.1), but it makes `footer` *reachable*, which today it is not — a
user wanting footer at 0.05 and nav at 0.2 currently has no way to express that.

It also makes the vocabulary genuinely crawler-neutral: derived from DOM structure we compute
ourselves rather than inherited from whichever taxonomy a vendor happened to pick.

**Implemented in PR #128** as `sf_region_from_path()`. Two details settled while writing it:

- **Predicates are stripped before matching**, so `div[@class='site-footer']` stays a div.
  Matching on classes would make the region depend on a site's *naming conventions* rather than
  its markup. Note this cuts the opposite way from the boilerplate skeleton rule (§5), which
  *keeps* class predicates — different questions: "which region is this" versus "is this the
  same component".
- **A path with no `<body>` step returns `NA`**, not the `content` residual. §10.5 dismissed
  SF's `Head` position as a non-bug because `sf_graph_eligible()` filters those rows before
  placement is consulted — true for the graph, but the observations table still carries a
  placement, and calling a stylesheet "content" would be wrong. `NA` also lets `Link Position`
  take over cleanly.

### Out of scope (deliberately)

User-supplied override columns marking URLs or regions manually. Noted as a plausible future
expansion, not part of this work.

---

## 5. Boilerplate detection

> ### Direction of the metric
>
> **`ratio` is a boilerplate score in [0, 1]. Higher = more boilerplate = lower
> final edge weight.**
>
> - `ratio = 1.0` — every time this component appeared, it pointed here. Template
>   link. Gets discounted.
> - `ratio → 0` — this component points somewhere different on each page. Genuine
>   editorial choice. Keeps full weight.
>
> Stated explicitly because the polarity is easy to invert when reading, and an
> inverted reading makes "a sitewide denominator would bring the score down" sound
> like an improvement when it is in fact a miss (see below).

### What does not work

**Per-target thresholds.** "Target linked from ~every page" flags the homepage — correctly
for the header logo link, wrongly for an in-body link from an article that genuinely chose to
point there. Same destination, opposite nature. **Boilerplate is a property of the edge, and
specifically of its source context, not of the destination.**

**Anchor text.** An SEO'd site repeats "best resume builders" naturally across genuinely
editorial links. Anchor-based detection would penalize good on-page practice. Anchor text is
available (`R/screaming_frog_links.R:157`) but is the wrong signal.

### What does work: `link_path`

SF exports a DOM path per link (`link_path`, already parsed and carried onto the edge table at
`R/screaming_frog_links.R:192` — currently unused). A template-generated link has the same
structural path across many pages; a genuine in-body link to the same destination sits at a
different path on every page, because the surrounding prose differs. That separates the cases
on **structure** rather than on wording.

### The metric: container-conditioned, target-scored

Not sitewide. Nav and footer are the only true sitewide repetitions, and those are already
covered by the placement half of the axis. Landing pages, blog categories, and post templates
each have their own component sets.

- **Denominator**: pages where that container path appears at all.
- **Numerator**: pages where that container points at *this specific target*.
- **Ratio near 1** → boilerplate.

This correctly separates two cases that look identical structurally:

- A recycled CTA that always links "Book a demo" → ratio ~1.0 → **boilerplate**.
- A recycled related-posts module linking different articles per page → low ratio →
  **not boilerplate**, despite the component recurring identically.

That is the right outcome: the harm described in field-notes §4 is uniform in-degree inflation
on a single target, not component reuse as such.

### Why the denominator is the container and not the site

Validated on natu.care (§10). Third-party consent-banner links appear on 983 of 9,655 pages:

| denominator | ratio | verdict | outcome |
|---|---:|---|---|
| sitewide | 983 / 9,655 = **0.10** | "not boilerplate" | keeps weight 1.0 — **missed** |
| container | 983 / 983 = **1.00** | boilerplate | discounted — **caught** |

Consent links are boilerplate if anything is, and the sitewide denominator lets them through at
full weight. Note *why* they are on only 10% of pages: not because the site is inconsistent, but
because that is where the renderer happened to catch the banner. Container-conditioning is robust
to that, since it only asks "when this component appeared, did it always point here?" — no
special-casing for consent widgets required.

### Path skeleton: strip positions, keep class predicates

SF's `Link Path` is **not** pure positional XPath. It is a hybrid that uses attribute predicates
where classes exist and positional indices elsewhere:

```
//body/div/main/article/div[@class='page-context']/nav/ol/li[1]/a
//body/div/main/article/p[5]/a[1]
```

That is better than first assumed. Positional indices are unstable — the same recycled CTA lands
at `p[5]` on a post with four preceding paragraphs and `p[3]` on a shorter one — but
`[@class='…']` is exactly the stable component identifier the detector wants.

**Skeleton rule: strip numeric `[n]`, keep `[@class='…']`.**

Measured compression (§10): 256 → 29 skeletons on tidioreviews (88.7%), 22,022 → 1,630 on
natu.care (92.6%). Load-bearing — without it the detector under-detects in-content components
while working fine on nav, which is backwards, since nav is already covered by placement.

### `boilerplate_threshold = 0.5`, `min_container_pages = 10`

Both settled from the three-crawl comparison (§10). The reasoning matters more than the numbers.

**The threshold only has consequences for Content-position pairs.** Nav, header, footer and aside
edges are already discounted to 0.1 by placement, so the detector failing to catch them costs
nothing. An early reading of the data ("a 0.9 threshold misses `/cart` at ratio 0.82") was
mis-emphasised — `/cart` sits in Header position and was already handled. Only content-position
edges are at stake, because that is the only place placement cannot help.

**At 0.9 the detector misses two whole families of real boilerplate**, consistent across
tidio.com and natu.care:

| family | examples | ratio |
|---|---|---|
| recurring in-content CTAs | `/panel/register` (×3 containers), `/blog/`, `/integrations/`, `/collections` | 0.54 – 0.82 |
| author byline links | `/bart-turczynski`, `/ludwik-jelonek`, `/nina-wawryszuk-1`, `/people/gosia-szaniawska-schiavo/` | 0.53 – 0.69 |

Bylines are the persuasive case, and the better documentation example than `/cart`: on natu.care
a single author page is linked from 4,116 of 7,563 pages by an identical template element. That
is the §4 pattern exactly — uniform in-degree inflation with no editorial judgment behind any
individual link — and placement can never catch it, because the byline sits in content.

Moving 0.9 → 0.5 adds 37 content pairs on tidio (against 42 already caught) and 19 on natu.care
(against 77). Meaningful, not floodgates.

**`min_container_pages` is the weaker default.** Small containers dominate the high-ratio counts
(78 pairs at ratio ≥ 0.9 from 3–10-page containers on tidio, 147 on natu.care), and "3 out of 3"
is thin evidence — a 3-page container can only produce ratios of 0.33, 0.67 or 1.0, so band
membership is partly quantization rather than genuine ambiguity. Excluding them is cheap because
they carry few edges. But 10 is a judgement call, not a measured cut.

### Known surprise: author pages lose rank

The detector discounts byline links hard, so author pages will drop. **This is correct** — their
rank was manufactured by the template rather than earned — but it is counter-intuitive enough
that it belongs in user-facing docs rather than being discovered.

### Scope philosophy

Presence/recurrence detection is a **convenience**. We will not predict every site's weirdness,
and should not design as though we could. Users who want more granularity curate their own data.
The "boilerplate but in main content" judgment call ultimately belongs to the user.

---

## 6. Where opinionated constants live

**In presets, not in function defaults.**

`pagerank()` defaults to no weighting at all — the faithful default, nothing applied. A preset
carries `c(nav = 0.1, header = 0.1, footer = 0.1, content = 1)` and `boilerplate = 0.5` as part
of its bundle.

This is exactly what presets are for: opinionated constants that are visible, inspectable via
`pr_preset()`, and recorded in the transition audit. The argument then becomes "this preset chose
0.5", not "the package believes 0.5". No magic number buried in a signature for a reviewer to
argue with.

### Every constant in play

| # | Constant | Value | What it does |
|---|---|---|---|
| 1 | placement weight — nav / header / footer / aside | 0.1 | region discount |
| 2 | placement weight — content | 1.0 | no discount |
| 3 | **`boilerplate_weight`** | 0.5 | discount applied to a *detected* repetitive in-content link |
| 4 | **`boilerplate_threshold`** | 0.5 | ratio at/above which an edge is *classified* boilerplate |
| 5 | `min_container_pages` | 10 | ignore components seen on too few pages to judge |
| 6 | position weight | TBD | decay by reading order; shape unexamined (§9 Q5) |

Constants 1–3 are the single graded boilerplate axis (§2). Constants 4–5 are the detector that
feeds it (§5). Constant 6 is the orthogonal axis.

> ### ⚠️ Two different 0.5s — do not conflate them
>
> `boilerplate_threshold` and `boilerplate_weight` are unrelated quantities that happen to share
> a value. One is a **classifier input** (a fraction of pages), the other a **weighting output**
> (a multiplier). This caused real confusion during the design session.
>
> **Never write a bare "0.5" — always attach the name.**

### Worked example, end to end

A byline link to `/bart-turczynski`, in a container appearing on 7,563 pages and pointing there
on 5,112 of them:

```
ratio = 5112 / 7563                    = 0.68
0.68 >= boilerplate_threshold (0.5)    -> classified boilerplate
7563 >= min_container_pages (10)       -> enough evidence to judge
position = Content                     -> placement weight 1.0
boilerplate_weight                     = 0.5
position weight (top of page)          = 1.0
edge_weight = 0.5 x 1.0                = 0.50
```

Classified at 0.68, weighted at 0.5. The two numbers never interact beyond the comparison.

### On `boilerplate_weight = 0.5`

Definable, and deliberately *not* 0.9 — at 0.9 the discount is a rounding error, and anything
near the top of the page would beat a genuinely good, natural link halfway down. Somewhere
between 1 and 0.1, closer to the middle. **A declared guess, not a measured constant**, and
documented as such. Unlike `boilerplate_threshold`, no data bears on it.

---

## 7. Naming

Settled:

- `sidebar` → `aside`.
- `content_only` **must** be renamed; it names the operation we are avoiding.
- `reasonable_surfer` is **reserved** — `PAGE-gkpltihp` (E. Fitted reasonable-surfer model)
  already owns it for the GA4-fitted model. Our heuristic position weighting is its cousin,
  not its replacement, and must not squat the name. (Same reasoning as the `cheirank`
  reservation.)

**Settled: the preset is `content`.** Preset set becomes **`raw` · `declared` · `reversed` ·
`content`**, with `editorial` reserved for a future composite (content weighting *plus*
boilerplate suppression) that would actually earn the name.

`content` names the region bucket it favours — nothing more. It matches SF's vocabulary, matches
our own placement term, and glosses cleanly as "weights toward the (main) content region". One
concept, one word, used consistently in both places.

The earlier `content_first` proposal was over-engineered. It defended against someone reading
`content` as "content *only*" — but that is the harmless misreading:

- Believing it *dropped* nav when it downweighted → mental model slightly off, **output still
  correct**.
- Believing you have genuine editorial data when in-content boilerplate survived → you trust a
  wrong ranking. **This is the §4 failure.**

`editorial` invites the harmful misreading; `content` invites only the harmless one. The extra
syllables in `content_first` bought protection against the wrong risk.

Note that adding a separate position dial does **not** rehabilitate `editorial` for this preset.
The preset still performs region weighting only, so the name would still claim discretionary
intent it has not established. Dials around it do not change what it does.

### Rejected candidates (for the record)

| Candidate | For | Against |
|---|---|---|
| `content_first` | Direction clear; *first* ≠ *only* | Protects against the harmless misreading at the cost of a clumsier name |
| `editorial` | Established SEO vocabulary, instantly legible | Connotation is *naturally given* — the exact overclaim, and the path that produced the §4 misreading. **Reserved** for the composite |
| `chrome_suppressed` | Structurally the most honest — describes the operation, asserts nothing about the residual | "Chrome" reads as the browser in an SEO package; UI jargon more familiar to devs than SEOs |
| `placement_weighted` | Axis-honest | **Rejected**: names the knob, not the view. A preset that *boosted* nav would be equally "placement weighted", and it breaks the family — `raw`/`declared`/`reversed` all name a resulting graph |

The related design question, still open: is boilerplate suppression **bundled into** the preset
or a **separate composable argument**? Making it a separate argument (e.g. `boilerplate_action`,
which fits the existing `nofollow_action` / `robots_blocked_action` family) avoids a
combinatorial explosion of preset names — otherwise wanting boilerplate suppression on top of
`raw` or `declared` means `raw_deboilerplated`, `declared_deboilerplated`, and so on.

If it is composable, `editorial` becomes more defensible as the name for the composite
(placement + boilerplate), with a modestly-named preset for the placement-only view.

---

## 8. Epistemic caveat on the field-notes evidence

**Field-notes §4 is an observation, not a result.**

Two things were happening on the `tidioreviews` demo site simultaneously: the site was being
built and improved, *and* the PageRank calculations were being tested against it. The
in-content boilerplate finding (`/about/methodology/` and `/about/affiliate-disclosure/`
holding 54% of editorial authority) came from a site being edited in response to the very
findings being recorded. The culprit was most likely breadcrumbs, which were subsequently
marked up as `<nav>` — partly to make the site easier to work with using this tool.

So we cannot tell how much of "in-content boilerplate is a second nav" is a general property of
websites versus an artifact of markup that had not been fixed yet.

**Direct evidence for the confound, from the pre/post crawl pair (§10).** Content-position links
scoring ratio ≥ 0.9:

| crawl | high-ratio content edges | targets caught |
|---|---:|---|
| pre-intervention (`old/`) | **130** | `/about/methodology/`, `/about/affiliate-disclosure/` |
| current | **0** | — |

The detector fires on exactly the two pages §4 names as holding 54% of editorial authority, at
ratio 1.0 across 45/45 and 20/20 pages in two distinct components — and the signal is *completely
absent* from the current crawl. §4 was real, and it was fixed. The clean-looking current crawl is
the fix, not the baseline.

**Consequences:**

- §4 remains a good *illustration of the failure mode*. It is not evidence about the failure
  mode's *prevalence*.
- Do not let §4 carry design load it cannot bear — in particular, it is not sufficient grounds
  for making boilerplate suppression a default.
- Any paper use of this material must state the confound. A reviewer will find it otherwise.
- `PAGE-kddhyhpw` (trimmed two-crawl pre/post-intervention fixture) is the closest thing to
  clean evidence, since it captures both sides of a deliberate intervention.

Related: field-notes §254-259 records that wrapping byline links in `<nav aria-label="Editorial
standards">` reclassified them. That is the same phenomenon read positively — the site
*declaring* what a detector would otherwise have to infer. Markup as intervention is a genuine
finding and a paper angle.

---

## 9. Open questions

1. ~~**Recurrence threshold**: user-facing or fixed?~~ **Answered: user-facing, default
   `boilerplate_threshold = 0.5`** (§5, §10.6). Not because the data shows a clean cut — it does
   not — but because the band is populated by two real families of boilerplate that a 0.9
   threshold would miss. Ship a documented default, let it be overridden, and do not claim
   empirical separation.
2. ~~**Natural cut**: is there a natural break in the ratio distribution?~~ **Answered (§10): no,
   and the earlier "yes" was wrong.** tidioreviews showed a strikingly clean bimodal gap
   (literally zero pairs in 0.5–0.95). That is a **small-site artifact**: on a 62-page site a
   component either appears on essentially every page or on a handful. natu.care (9,655 pages)
   populates the band. Do not re-derive this from tidioreviews and reach the old conclusion.
3. **Graceful degradation**: `link_path` may be absent from a non-SF crawl. Does boilerplate
   detection hard-require it, or fall back to something clearly labeled weaker? Same "column may
   or may not exist" shape as placement.
4. **Position signal source**: CSV row order is fragile — any filter, join, or dedup between
   ingest and weighting destroys it, and the SF path already filters by placement and origin
   *before* weighting. Order must be materialized into an explicit per-source position index at
   ingest, while it is still trustworthy, and never inferred from row order downstream.
5. **Position decay shape**: linear, exponential, or rank-bucketed? Unexamined.
6. **Unnamed placements currently get weight 1** (`R/pagerank_screaming_frog.R:183`). Under the
   field-notes recipe that makes `footer` and `aside` outweigh `nav` tenfold — almost certainly
   not intended. Fix by having the preset name all five placements explicitly rather than adding
   a `default` weight argument.

---

## 10. Validation results (2026-07-20)

Run against real All Inlinks exports. **Never read these files into an agent context** — process
them in a script with `data.table::fread(select = …)` and return only aggregates. The natu.care
export is 1.3 GB / 3.86M rows.

| crawl | pages | hyperlinks | note |
|---|---:|---:|---|
| `~/Projects/tidioreviews/old/` | 67 | 3,820 | pre-intervention |
| `~/Projects/tidioreviews/` | 62 | 3,599 | post-intervention |
| `_scratch/crawls/natu.care/` | 9,655 | 2,344,199 | large, uncleaned, multilingual e-commerce |
| `_scratch/crawls/tidio/` | 2,767 | 611,108 | large SaaS marketing site, different stack |

### 1. Skeleton normalization — confirmed

| crawl | raw paths | skeletons | compression |
|---|---:|---:|---:|
| tidioreviews | 256 | 29 | 88.7% |
| natu.care | 22,022 | 1,630 | 92.6% |

Strip numeric `[n]`, keep `[@class='…']`. Groups as designed at both scales.

### 2. Ratio distribution — the "natural cut" was a small-site artifact

tidioreviews (62 pages) is strikingly bimodal — **zero** pairs between 0.5 and 0.95 on the
pre-intervention crawl. That does **not** generalize. natu.care, on 104,738 pairs:

| band | pairs |
|---|---:|
| 0.5–0.6 | 210 |
| 0.6–0.7 | 122 |
| 0.7–0.8 | 62 |
| 0.8–0.9 | 105 |
| ≥ 0.95 | 485 |

No void. What survives is the weaker claim that the ambiguous band is ~0.5% of pairs, so the
threshold has low leverage. See §9 Q1–Q2.

### 3. The intervention is visible — and the detector catches the §4 culprits

Content-position, ratio ≥ 0.9: **130 edges pre-intervention → 0 after.** The pre-crawl hits are
`/about/methodology/` and `/about/affiliate-disclosure/` at ratio 1.0 (45/45 and 20/20 pages,
two distinct components) — precisely the pages §4 names. See §8.

### 4. At scale, axis 2 finds what placement cannot

natu.care, content-position with ratio ≥ 0.9: **398 pairs, 244 distinct targets, 71,224 edges**
— 3.5% of content edges. Targeted, not a blunt instrument.

Split by scope, since roughly half would be excluded by internal-only scoping anyway:

| | distinct targets |
|---|---:|
| internal | 132 |
| external | 112 |

Internal catches are the §4 pattern at scale: `/cart`, `/collections/all`, `/pl/regulamin`
(terms), `/pl/o-nas` (about), promo-terms pages, `/policies/privacy-policy` — each on 634–986
pages. External catches are consent-banner and third-party privacy links.

**Both results are consistent.** Axis 2 found nothing on the cleaned tidioreviews site and 132
internal targets on an uncleaned one. It matters exactly when a site has not already been fixed
by hand, which is the normal case.

### 5. Incidental findings

- **`Link Position` hides nested regions** — see §4. This is what motivated deriving the region
  from the DOM path.
- **`Head` is not a gap.** SF emits a `Head` position (254 rows on tidioreviews) that
  `sf_normalize_position()` does not map. It is the HTML `<head>` — paths are `//head/link[…]`,
  types are CSS / HTML Canonical / HTML Hreflang. None are graph-eligible, so `sf_graph_eligible()`
  filters them before placement is consulted. Investigated and dismissed; not a bug.
- **Non-HTML is ~35% of natu.care rows** (Image 472,239; Misc 397,390; JavaScript 299,640;
  Font 154,252). Feeds `PAGE-ztmtdzzu`.

### 6. The ambiguous band, resolved

Isolating **Content-position** pairs — the only ones where the threshold has consequences, since
everything else is already discounted by placement:

**tidio.com**

| container pages | <0.5 | 0.5–0.7 | 0.7–0.9 | ≥0.9 |
|---|---:|---:|---:|---:|
| 3–10 | 340 | 18 | 10 | 78 |
| 11–50 | 2,298 | 17 | 8 | 204 |
| 50+ | 16,762 | 29 | 8 | 42 |

**natu.care**

| container pages | <0.5 | 0.5–0.7 | 0.7–0.9 | ≥0.9 |
|---|---:|---:|---:|---:|
| 3–10 | 1,722 | 93 | 19 | 147 |
| 11–50 | 13,124 | 71 | 139 | 174 |
| 50+ | 85,546 | 18 | 1 | 77 |

The band is not noise. It contains two recognisable families — recurring in-content CTAs and
author byline links — that recur across both sites and that placement cannot reach. This settles
`boilerplate_threshold = 0.5` and motivates `min_container_pages`; see §5.

Whether tidioreviews' sibling crawl (once colocated) shifts anything: unlikely on this evidence,
since the small site contributes 0 band pairs pre-intervention and 3 after, two of which come
from ≤10-page containers.

---

## 11. Where this lives

- `PAGE-ooqveone` — preset convenience layer; consumes the naming and constants decisions.
- `PAGE-gkpltihp` — fitted reasonable-surfer model (GA4-gated); owns the `reasonable_surfer`
  name and is the eventual successor to heuristic position weighting.
- `PAGE-bcggkykd` — vocabulary collision (conceptual raw vs the `raw` preset); adjacent to §4.
- `PAGE-vqfytgam` — the paper. Its headline is *placement-aware PageRank*, which makes this
  file's model the paper's core contribution. §2 (two-axis model), §3 (downweight-not-drop),
  §5 (structural boilerplate detection), and §8 (the confound) are all paper material.
- `PAGE-kddhyhpw` — two-crawl fixture; the validation substrate for §10.
