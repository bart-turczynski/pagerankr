# URL-level what-if modeling (`simulate_changes()` v2)

**Design spec — source of truth.** Agreed 2026-07-22. Status: unbuilt.
Supersedes the ad-hoc, edge-level-only behavior of the current
`simulate_changes()` and retires its buggy `add_redirects_df` path.

Related: field notes §8 ("what-if is cheap") and §11 (dead pages / teleport);
epic `PAGE-qzskzcfd` (page-state modeling) and its children
`PAGE-ijrokqgo` / `PAGE-pvfdijrw` / `PAGE-bcpacnfm` / `PAGE-oqehfvlm`.

---

## 1. Purpose

Lift `simulate_changes()` from **edge/link-level** what-ifs to **URL-level**
ones. Let a user model, on the stored graph before touching the site:

- **creating a redirect** — retire a live page and send its authority onward;
- **changing (repointing) a redirect** — A→B becomes A→C;
- **removing a URL** — the page 404s.

Recompute the whole graph and return before/after. **Interpretation is left to
the user** — this is a faithful recompute primitive, *not* a ranking or
target-optimization engine. (The "where should I redirect for the most gain / is
it worth it vs. leaving it be" workflow is served by running this primitive and
reading the result; no objective functions or candidate ranking are built in.)

### Non-goals
- No candidate-target generation or ranking.
- No redirect-equity decay knob (see §5).
- No rename/move primitive: a consistent rename is a pure relabel (score-neutral);
  the interesting "moved but inbound links not fixed" case is `remove_urls` +
  `redirect_urls_df` composed.

---

## 2. The verbs → two graph primitives

| English intent | Primitive |
|---|---|
| "create a redirect" | **`redirect_urls_df`** (create) |
| "change a URL" (repoint an existing redirect) | **`redirect_urls_df`** (override) |
| "remove a URL" | **`remove_urls`** (404 / evaporate) |

Edge-level `add_links_df` / `remove_links_df` are unchanged and orthogonal
("links" = edges; "urls" = nodes).

---

## 3. Semantics

Every verb is a choice about **A's inbound authority** and **A's outbound links**:

| Verb | Inbound links → A | A's outbound links | A as a node in output |
|---|---|---|---|
| **`redirect_urls_df`** (A→B) | rewritten to B, **100% pass-through** | **dropped** — an honest 301 has no body | folded into B; disappears from proposed set (`pagerank_proposed = NA`) |
| **`remove_urls`** (A → 404) | **kept but dead**: flows into A, then **evaporates** to the waste sink (mass exits, booked) | **dropped** | **stays** as a dead node holding the mass it absorbed once; flagged `node_status` |

### Why strip the redirect source's outlinks (the fix)
The current `resolve_redirects()` fold *relabels* A→B on both edge endpoints, so
A's outlinks become B's. On real crawl data this is harmless (a page returning
3xx has no crawled body → no `A→Y` edges). But `simulate_changes()` applies the
same fold to a **currently-live** A, whose outlinks *do* exist — silently
modeling a **content move** (B inherits A's links) when the user meant **retire**.
The retire model strips A's outedges before folding: B gains A's inbound
authority only. See §7.

### Why remove = evaporate (not delete-node, not dangle, not self-loop)
Removal = "change to 404." A 404 keeps its inbound links (other pages still link
to it) but they now point at a dead page; its outbound links are gone. Authority
flows in and **stops** — it does not (a) redistribute across the site via
teleport (DANGLE, today's accidental behavior — §11's 95% collapse), nor
(b) self-amplify via a self-loop (TRAP — inflates ~8×), nor (c) vanish as if
every inbound link were instantly cleaned up (delete-node — understates the real
cost). Only the sink is true. This is the `PAGE-qzskzcfd` decision applied to
the 4xx/5xx class; the what-if is its first consumer. If a user *also* wants to
model cleaning up the inbound links, that is `remove_urls` + `remove_links_df`,
composable.

### Override
A `redirect_urls_df` row for source A **wins** over any prior redirect for A —
whether from an earlier proposed row or from the baseline crawl's real 3xx.
"Change A→B into A→C" is thus a single override on the final proposed graph,
scored **once** (not recompute-after-remove then recompute-after-add).

---

## 4. API surface

### `simulate_changes()` (bare edge list — the CSV-only path)
New arguments:
- **`redirect_urls_df`** — a two-column `from`/`to` data frame (redirects are
  pairs). Create-or-override; retire semantics (strips the live source's
  outedges). Defaults to `from`/`to` columns.
- **`remove_urls`** — a **character vector** of URLs to treat as 404 (URLs are
  singletons → vector, matching `keep_domains` / `exclude_domains`). Under the
  hood it builds the `status_df` (all `404`; 4xx = 5xx, no split) that
  `pagerank()` consumes.
- **`on_unknown_target`** — `c("warn", "error", "allow")`, default `"warn"`
  (see §6).

**Removed:** `add_redirects_df` (it *was* the bug — a move masquerading as a
redirect). Pre-CRAN, experimental, single-consumer → deleted, not deprecated.

Unchanged: `edge_list_df`, `redirects_df` (baseline), `add_links_df`,
`remove_links_df`, `...` (forwarded to both `pagerank()` calls).

### `simulate_changes_screaming_frog(bundle, …)` (the SF-bundle path)
A **thin wrapper** mirroring `pagerank_screaming_frog()`. Reuses the existing
bundle→args adapter (`.sf_build_pr_args()` and friends) to build the baseline
and proposed inputs, applies the same verbs, and delegates to the **same generic
engine**. No SF-specific simulate logic. Placement / boilerplate / nofollow /
status are preserved automatically. The what-if's forced-dead `status` override
composes on top of the bundle's real crawled status.

Both entry points funnel into one internal changeset engine → capability parity
for CSV and SF users.

---

## 5. Modeling assumptions

- **No redirect decay.** A redirect passes authority through at 100%. The only
  per-hop loss is the global `damping` (0.85) every edge already incurs; the
  "~15% lost through a 301" folklore is not a graph-level effect and modeling it
  would violate faithful-default. No knob (addable later if ever justified).

---

## 6. Composition & conflicts

Build order: **`remove_urls` → `redirect_urls_df` → `remove_links_df` /
`add_links_df` → recompute once.** Deterministic and documented; because
contradictions error out, order rarely changes the result.

**Error** on:
- the same URL in both `remove_urls` and `redirect_urls_df` (a URL cannot be
  both a 301 and a 404 — genuine contradiction; do not guess precedence);
- a duplicate source in `redirect_urls_df` (A→B and A→C in one changeset),
  matching `resolve_redirects()`'s default `duplicate_from_policy = "strict"`.

**Compose naturally** (no special-casing): redirect into a URL that is also
removed (authority passes through, then evaporates); adding a link to a removed
URL (it evaporates — odd but honest).

**Unknown targets** (`on_unknown_target`, default `"warn"`): a redirect/link
target not in the current node set is **not** an error — it may be a legitimate
new page.
- same-domain, not crawled → modeled as a **new node** carrying inbound
  authority, no outlinks yet; warn.
- off-domain → governed by the existing `out_of_scope_fold`
  (`relabel` / `keep` / `leak`); warn rides on top.

---

## 7. Output (option (b))

Headline: the `compare_pagerank()` table, which already carries **raw before/
after values as columns** — `node_name`, `pagerank_baseline`,
`pagerank_proposed`, `delta`, `pct_change`, `rank_baseline`, `rank_proposed`,
`rank_delta` — **plus a per-row `node_status`** column
(`normal` / `removed-dead` / `new-target`) so a removed node's residual absorbed
mass is never misread as earned authority. (This is a minimal slice of
`PAGE-oqehfvlm`, scoped to the what-if output only; the universal per-row status
inside `pagerank()` remains a later epic slice.)

Attributes:
- the **full proposed `pagerank()` result** including its `transition_audit`
  (so the evaporated-mass *cost* of a removal is surfaced by default);
- a **change manifest** — what was removed, which redirects applied/overrode,
  which targets triggered warnings.

Rationale: the delta stays the headline; "recalculate everything" is one
`attr()` away; no two-call dance.

---

## 8. Dependencies & sequencing

**Phase 1 — epic core slice (`PAGE-qzskzcfd`; decided, unbuilt). Prerequisite.**
- `PAGE-ijrokqgo` — `status_df` + `status_col` as a first-class input on bare
  `pagerank()`. *Input contract only* here; the SF-status plumbing half is not
  needed by the what-if (it supplies an explicit URL list) and stays deferred.
- `PAGE-pvfdijrw` — unified waste sink: exactly one outgoing edge to the sink for
  the dead class; the **"adds-one-edge"** half is what makes an outlink-less 404
  evaporate instead of dangling. Rename `__pr_nofollow_sink__` → general waste
  sink.
- `PAGE-bcpacnfm` — exclude the dead class from the teleport vector.

**Phase 2 — this feature.** Consumes Phase 1. `remove_urls` feeds a forced
`status_df` (404). The what-if is the first real consumer/test of the mechanism.

The redirect and change verbs have **no** epic dependency (pure fold-map graph
surgery + the outedge-strip fix); only `remove_urls` depends on Phase 1.

---

## 9. The `add_redirects_df` bug (filed separately)

`simulate_changes(add_redirects_df =)` folds a live source without stripping its
outedges, so it models a **content move** (target inherits the source's outlinks)
when the user meant a **retire** — over-crediting the target. Blast radius is the
what-if layer only; `resolve_redirects()` and `pagerank()`'s baseline redirect
handling are correct (real 3xx sources have no outlinks). Resolution: replaced by
`redirect_urls_df` with strip-and-override semantics (§3, §4).
