# Screaming Frog Representative Crawl Acceptance

Recorded on 2026-06-18 from read-only local inputs under
`/Users/bartturczynski/Projects/semantic/_scratch`. The source CSV files are
not part of this repository and must not be committed.

## Inputs

| Export | Size | Lines |
| --- | ---: | ---: |
| `internal_all.csv` | 52 MB | 55,739 |
| `all_inlinks.csv` | 522 MB | 1,120,542 |
| `all_outlinks.csv` | 576 MB | 1,300,239 |

## Import Measurements

Measured with `devtools::load_all(quiet = TRUE)` and the public import APIs.
Object size is `object.size()` of the returned S3 object.

| Step | Elapsed | Object | Key counts |
| --- | ---: | ---: | --- |
| `screaming_frog_internal()` | 2.491 s | 33.1 MB | 55,701 nodes; 738 redirects; 2,173 canonicals; 55,701 indexability rows |
| `screaming_frog_links(..., "all_inlinks")` | 31.760 s | 245.5 MB | 1,120,159 observations; 538,065 graph edges; 582,094 excluded non-Hyperlink rows |
| `screaming_frog_links(..., "all_outlinks")` | 38.253 s | 274.8 MB | 1,299,856 observations; 538,065 graph edges; 761,791 excluded non-Hyperlink rows |

Both link exports produced 21,766 nofollow graph edges. The Outlinks export
contained 1,494 blank destinations overall, but none of those blank destination
rows were graph-eligible Hyperlink rows. Both exports contained 30,285
Hyperlink self-link observations.

## Inlinks vs Outlinks Reconciliation

Normalized edge keys used `from`, `to`, `nofollow`, `placement`, and
`link_origin`.

| Metric | Count |
| --- | ---: |
| Unique Inlinks keys | 403,628 |
| Unique Outlinks keys | 403,628 |
| Unique union keys | 403,628 |
| Inlinks-only keys | 0 |
| Outlinks-only keys | 0 |
| Keys with different multiplicity | 0 |
| Total absolute multiplicity delta | 0 |

The representative Inlinks and Outlinks Hyperlink edge multisets are identical
after normalization. Either export can supply the graph for this crawl.

## Bundle and PageRank

`screaming_frog_bundle(internal, outlinks)` produced:

- 55,701 nodes
- 1,299,856 raw observations
- 538,065 graph edges
- 738 redirects
- 2,173 canonicals
- 58,311 absent edge endpoints in cross-table diagnostics
- 52,429 node URLs absent from the graph

`pagerank_screaming_frog(bundle, nofollow_action = "evaporate",
canonical_conflict_policy = "redirect_wins")` completed in 20.787 seconds.
The result had 9,191 scored rows. The attached transition audit reported:

- 538,065 input edge rows
- 251,854 final transition edges
- 9,191 vertices
- 253,234 duplicate rows dropped
- 32,961 self-loops dropped
- 286,211 rows collapsed during transition construction
- 479 robots-blocked rows
- reported mass 0.8329032
- sink mass 0.1670968
- hidden mass 0
- total mass 1

Top ten URLs by PageRank in this acceptance run:

| URL | PageRank |
| --- | ---: |
| `https://www.tidio.com/` | 0.004935325 |
| `https://www.tidio.com/privacy-policy/` | 0.003027118 |
| `https://www.tidio.com/terms/` | 0.002763665 |
| `https://updates.tidio.com/en` | 0.002337998 |
| `https://updates.tidio.com/roadmap/en` | 0.002277789 |
| `https://www.tidio.com/blog/` | 0.002230975 |
| `https://www.tidio.com/live-chat/` | 0.002148721 |
| `https://www.tidio.com/integrations/` | 0.002096044 |
| `https://status.tidio.com/` | 0.002071174 |
| `https://developers.tidio.com/` | 0.002068699 |

## Operational Contract

Required exports are Screaming Frog **Internal: All** plus one **All Inlinks**
or **All Outlinks** bulk link export. The default graph policy is
Hyperlink-only. Non-Hyperlink resource and signal rows are excluded from graph
edges by policy; redirects and canonicals come from dedicated Internal: All
node signals.

Origin filters, placement filters, and placement weights are opt-in scoring
choices. Missing optional export columns degrade to typed `NA` values and are
reported in diagnostics; missing required columns error early. Production
pipelines should check the bundle provenance `contract_version` before relying
on stored assumptions.
