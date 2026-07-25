# Case-study fixture: a 67-page reviews microsite, before and after

Two Screaming Frog crawls of the same small site, taken either side of a
deliberate internal-linking intervention. They exist so the case-study vignette
and the placement/boilerplate analyses run from package data instead of an
external repository.

```
reviews-microsite-before/    reviews-microsite-after/
  all_inlinks.csv              all_inlinks.csv
  internal_all.csv             internal_all.csv
```

## What the two crawls differ by

Between them, sitewide in-content links were restructured: methodology and
disclosure bylines that had appeared in every page's body were moved into a
semantic container, and body-content links were added to the most-orphaned
pages. The point of shipping both is that the *same* site is measured twice, so
a change in PageRank is attributable to the intervention rather than to a
difference between sites.

`notes/edge-weighting-model.md` and `notes/pagerank-behavior-field-notes.md`
analyse this pair; the vignette reproduces the headline before/after table.

## Loading

```r
crawl <- function(phase) {
  d <- system.file("extdata", paste0("reviews-microsite-", phase), package = "pagerankr")
  screaming_frog_bundle(
    internal = file.path(d, "internal_all.csv"),
    links    = file.path(d, "all_inlinks.csv")
  )
}

before <- crawl("before")
after  <- crawl("after")

pagerank_screaming_frog(before)
```

## Shape

| | before | after |
| --- | ---: | ---: |
| `all_inlinks` rows | 4,440 | 4,131 |
| `internal_all` rows | 92 | 85 |
| graph-eligible (`Hyperlink`) edges | 3,820 | 3,599 |
| nodes | 92 | 85 |
| ranked pages | 74 | 69 |

Both exports are trimmed to the columns declared by `sf_contract()` — 18 for
`all_inlinks`, 24 for `internal_all`, down from the 103 columns Screaming Frog
writes by default. `Link Path` is retained deliberately: it is the entire signal
the boilerplate detector reads, and dropping it would make the fixture unable to
answer the questions it exists for. The whole fixture is ~70 KB gzipped.

## The site is pseudonymous, and this is not reversible

The underlying crawls are of a real site, and everything identifying has been
replaced rather than redacted:

- **Hosts** map to `reviews-microsite.example.net` (the crawled host) and
  `reviews-microsite.example.com` (the canonical target, which was never
  crawled). Third-party hosts become `external-NN.example.org`.
- **Paths** are relabelled to `/s2/p07/`-style tokens that preserve depth and
  sibling grouping and discard meaning. No real path segment survives.
- **Anchors and alt text** are regenerated from the destination's label. Only
  the empty / non-empty distinction is preserved, because an empty anchor is a
  real signal for image links.
- **Titles, meta descriptions, headings and author/category fields** are blanked.
  They carry identity as directly as URLs do.

Retained verbatim, because they are structural rather than identifying: status
codes, indexability, canonical and redirect relationships, crawl depth, word
counts, link counts, response times, `Link Position`, and `Link Path` — whose
attribute vocabulary is ten generic component names (`nav-item`, `table-wrap`,
`faq-list` and similar).

The mapping is deterministic — the same source reproduces this fixture exactly —
but it is **not** stored anywhere in this repository, and it is not invertible
from the fixture alone. `data-raw/build-fixture.R` regenerates it from the
private source crawls, which are not distributed. A scrub gate in that script
fails the build if any real host token or path segment reaches the output, and
`tests/testthat/test-fixture-scrub.R` re-checks the shipped files independently.
