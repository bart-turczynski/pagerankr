# Replication playbook: running an internal-link audit

An operational condensation of `pagerank-behavior-field-notes.md`. The field
notes explain *why*; this file is the checklist for doing it on a site that is
not the case-study site.

The worked example with live numbers is `vignette("case-study")`. Where this
playbook and that vignette disagree, the vignette wins — it is executed at build
time, this file is prose.

---

## 1. What to collect

From Screaming Frog, per crawl:

| Export | Why |
| --- | --- |
| `Internal > All` | the page inventory: status, indexability, canonical, depth |
| `Bulk Export > Links > All Inlinks` | the link graph itself |

Two columns decide whether the analysis is possible at all, and both are easy to
lose to a column-trimming step:

- **`Link Position`** — the entire placement lens. Without it there is no
  editorial graph, only a full graph.
- **`Link Path`** — the entire boilerplate lens. It is the XPath the detector
  reads to identify a template container.

Crawl configuration: render JavaScript if the site needs it, and keep the crawl
scoped to the host you intend to analyse. If you are measuring an intervention,
**crawl both phases with the same configuration** — a settings change between
crawls is indistinguishable from a site change in the output.

### Archive the exports per-date

Screaming Frog writes to fixed filenames. Re-exporting overwrites the baseline
you will need later, usually before anyone has decided that a before/after is
worth doing.

Commit or archive every export under a dated path at the time you take it. On
the case-study site the pre-intervention crawl survived only because it had been
committed to the site repo and was recoverable from git history after the
working copies were overwritten. That was luck, not process.

---

## 2. Settings

The `pagerankr` defaults are the intended audit settings; the table is here so
they can be stated explicitly in a writeup rather than left implicit.

| Setting | Value | Note |
| --- | --- | --- |
| `damping` | `0.85` | the standard value; report it |
| solver | `prpack` | exact direct solver, no tunable convergence |
| `self_loops` | `"drop"` | a page voting for itself is noise |
| `duplicate_edge_policy` | `"collapse"` | five links A→B are one editorial decision |
| `drop_isolates_flag` | `TRUE` | unlinked nodes distort teleport mass |
| `nofollow_action` | `"evaporate"` | see gotcha 4 |
| `out_of_scope_fold` | `"relabel"` | see gotcha 1 — **check this one** |

Setting `eps` or `niter` silently switches the solver to `arpack`, because
`prpack` has no convergence knobs. If you did not mean to change solver, do not
set them.

Restrict to internal links and followed links before scoring. External edges
belong in the graph only if you are deliberately measuring outbound leakage.

---

## 3. The two-lens discipline

**Always compute and report both:**

- **Full graph** — every link. This is what a naive audit produces, and on most
  template-driven sites it is mostly a readout of the navigation. Expect it to
  be nearly flat and nearly immovable.
- **Editorial graph** — `accepted_placements = "Content"`. Links a human chose
  on that page.

The two answer different questions and routinely disagree. On the case-study
site an intervention moved the top page from rank #1 to #38 on the editorial
lens while the full graph correlated at Pearson 0.9999 before and after — the
same change was both dramatic and invisible depending on which was scored.

Reporting only one lens is how an audit produces a confident wrong answer. If
the two disagree, that disagreement *is* the finding: it localises the change to
the editorial layer or the template.

A middle option exists — `preset = "content"` downweights chrome instead of
excluding it (`vignette("presets")`). Use it when you want one blended number;
use the two-lens split when you want to understand what moved.

---

## 4. Gotchas

### 1. A cross-domain canonical can silently relabel the whole graph

`pagerankr` folds redirects and `rel=canonical` through the same engine: a
canonical rewrites *both* endpoints of every edge. If a staging host canonicals
1:1 to a production host that was never crawled, the fold is a graph
isomorphism — PageRank is computed on the crawled topology and every node is
*renamed* to its uncrawled twin. Nothing about the target host was measured.

The tell is that filtering on the domain you actually crawled returns **zero
nodes**, and you must filter on a domain that never appeared in the input.

Worse, it can corrupt rather than merely relabel: a genuine external link to the
canonical target merges into the relabeled internal node. A forced repro of five
such links raised the internal node's PageRank by 9.3%.

**Check before scoring:** are the canonicals in scope? `audit_canonicals()` and
`audit_fold()` answer this. A cross-domain canonical to an uncrawled target is
advisory, not a redirect, and folding it is a modeling choice you should make
deliberately.

### 2. In-content boilerplate is a second navigation

Stripping the template nav is not enough. Sitewide links that live *inside*
article bodies — methodology, affiliate disclosure, "contact us", compliance
footnotes — sit in the content region and keep full weight forever, while
carrying exactly the problem chrome does: one editorial decision cast as
thousands of votes.

On the case-study site two such pages held 52% of all editorial authority, with
in-degree 67 (every page) and out-degree 2.

Two ways to handle them:

- **Detect them** — the boilerplate detector asks whether a container always
  points at the same target (`vignette("boilerplate")`).
- **Fix the markup** — move them into a semantic `<nav>`, and the crawler
  reclassifies them from `Content` to `Navigation`. They leave the editorial
  graph entirely.

The second was the single largest lever in the case study: 99 internal content
edges disappeared and almost no HTML changed. It is both correct semantics and
the cheapest available de-sink.

### 3. Two-crawl comparisons have confounds — state them

A before/after on two real crawls is a natural experiment, not a controlled
simulation on a fixed graph.

- **The node set will change.** Pages get retired, added, merged. Absolute
  PageRank is therefore **not comparable across crawls**. Read rank shifts,
  concentration metrics (Gini, entropy, top-N share), and relative deltas.
- **Absolute levels drift mechanically.** When pages leave, their mass
  redistributes and everything else rises a few percent. That spread is a
  node-set artifact, not a treatment effect. Where a correlation is available it
  is the honest statistic; per-page percentages on the insensitive lens are not.
- **Shipped changes come in bundles.** If reclassification and new links land in
  the same release, the measurement is the net. Attributing shares between them
  needs `simulate_changes()` on a fixed graph.

Three mechanical traps in the same neighbourhood:

- **Scores do not sum to 1.** Mass reaching a page with no onward links leaves
  the distribution and is accounted separately. Take every share against
  `sum(pagerank)`.
- **Match pages on the full URL, not the path.** External hosts share path
  strings with internal ones; keying on path silently merges distinct nodes.
- **`pr_top_k_share(x, k)` takes a fraction of nodes, not a count.**
  `pr_top_k_share(x, 0.1)` is the top 10%. Passing `5` is an error.

### 4. `nofollow` does not redistribute equity

Since Google's 2019 change, `nofollow` is a hint and the equity *evaporates*
rather than flowing to the page's other links. Classic PageRank sculpting is
dead, and any recommendation built on it is wrong.

`nofollow_action = "evaporate"` models this. It shows up in the mass accounting
as sink/hidden mass, not as a transfer — so if a report shows equity being
"redirected" by nofollow, the model is misconfigured.

---

## 5. Reading the output

**Low PageRank is not automatically a defect.** Signup, login and app-download
pages *should* rank low. The graph declining to spend authority on them is
correct behavior.

Act on the **gap between what the graph does and what intent says it should**:

- high intent, low PageRank → a money page the internal graph is starving; fix
- low intent, low PageRank → correct; leave it

**PageRank is a lens, not the deliverable.** None of these views is what a search
engine computes — they see the full graph plus external signals. Internal
PageRank sculpting on a small site is low-leverage regardless. Use the views as
diagnostics that pose questions, and resolve those questions with intent and
content quality.

---

## 6. Checklist

Collection

- [ ] `Internal > All` and `All Inlinks` exported for every phase
- [ ] `Link Position` and `Link Path` retained
- [ ] Same crawl configuration across phases
- [ ] Exports archived under a dated path

Before scoring

- [ ] Canonicals audited for out-of-scope targets (gotcha 1)
- [ ] Restricted to internal, followed links
- [ ] Settings recorded as configured, not assumed

Scoring

- [ ] Full graph computed
- [ ] Editorial graph computed
- [ ] `sum(pagerank)` inspected, not assumed to be 1
- [ ] Pages matched on full URL

Reporting

- [ ] Both lenses reported, and any disagreement between them called out
- [ ] Concentration and rank movement reported; absolute PageRank not compared
      across differing node sets
- [ ] Confounds stated
- [ ] Recommendations framed against intent, not rank alone
