# Non-HTML resources as PageRank nodes (research)

Ticket `PAGE-ztmtdzzu`. Question: should an image — or a PDF, CSS, JS, or any
non-HTML resource — be a node that collects PageRank?

Status: **research complete, no `pagerank()` code change.** Resolution is
"filter at the adapter boundary, document the contract." The one shipped change
is a documentation note on `?pagerank`'s `edge_list_df` param.

---

## 1. Finding: handled at the Screaming Frog boundary, open only off it

Screaming Frog's All Inlinks / All Outlinks carry link types beyond
`Hyperlink` — `Image`, `Stylesheet`, `Script`, `HTML Canonical`,
`HTML Hreflang`, and more. The edge adapter filters to true hyperlinks via
`sf_graph_eligible()` (`R/screaming_frog_contract.R`), whose eligible set is
`sf_contract()$graph_eligible_types = "Hyperlink"`. So on the Screaming Frog
path a resource URL **never becomes a graph vertex** — this is already the
shipped default and it is correct.

The exposure exists only on paths that bypass that filter:

- a non-Screaming-Frog crawler,
- a hand-built edge list,
- a user who overrides `graph_eligible_types`.

There, an image URL becomes an ordinary vertex: it collects authority from every
page that embeds it and, having no outlinks, behaves as a dangling sink — or,
under uniform teleport, gets *paid for existing* (see field notes §11).

## 2. Why a resource should not collect PageRank in an internal-link audit

The audit answers "where does link authority flow **between pages**." A resource
is not a page: it is not a navigation destination, it is not indexable as a
standalone result, and an `<img src>` or `<link rel=stylesheet>` is a fetch, not
an editorial vote. Counting one as a node:

- **inflates the denominator** — authority is divided among page *and* resource
  destinations, so every real page is understated;
- **manufactures sinks / danglers** — resources have no outlinks, so they hoard
  (and, pre-`prior_exclude_waste`, redistribute via teleport);
- **double-counts chrome** — a logo embedded site-wide accumulates exactly like
  a footer link, the same template-noise problem placement weighting exists to
  correct (field notes §2, §4).

Google's reasonable-surfer framing weights a link by its probability of being
followed; a stylesheet reference has ~zero click probability. The faithful
default is therefore: **resources are edges' cargo, not nodes.**

## 3. Why not push type-awareness into `pagerank()`

`pagerank()` is graph-agnostic by design: it receives a bare `from`/`to` edge
list and cannot reliably tell an image URL from a page URL without a link-type
signal it is never given. Adding resource detection there would either (a) guess
from file extensions — fragile, and wrong for extensionless resources or
`.php`-served images — or (b) invent a new required type column, breaking the
minimal edge-list contract. Type-awareness belongs at the **adapter boundary**,
where the crawler's own type labels are still attached, exactly as the Screaming
Frog adapter already does it.

## 4. Recommendation

1. **Keep the SF-boundary filter as the shipped default.** Done —
   `sf_graph_eligible()`, `Hyperlink` only.
2. **Do not add resource detection to bare `pagerank()`.** Document the
   assumption instead. Shipped: a note on `?pagerank`'s `edge_list_df` stating
   that edges are expected to be page-to-page hyperlinks and that resource links
   must be filtered upstream, pointing at `sf_graph_eligible()` as the reference.
3. **Any future non-SF adapter** that ingests typed links should apply the same
   hyperlink-only filter at *its* boundary, mirroring the SF adapter — never by
   teaching `pagerank()` about link types.

No further action. The modeling question resolves to a boundary contract, now
documented.
