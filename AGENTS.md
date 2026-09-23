R package `pagerankr`: SEO-focused PageRank modeling on crawl data — edge lists, redirect
reports, Screaming Frog exports. Lifecycle experimental; not on CRAN yet.

`rurl` installs from CRAN, at or above the floor `DESCRIPTION` declares. Its canonicalization
profile decides node identity, so a profile change re-keys the graph and must stay in sync
with the other rurl consumers. `canonical_profile()` pins every knob that shapes the key;
a new `get_clean_url` argument fails `test-canonicalization.R` until it is pinned or
triaged, on purpose.

`page_state` (crawl-derived page condition) and `node_status` (graph role) are separate
axes; keep them separate.

`NEWS.md` describes changes to the library. Crawl measurements and ranking-outcome
predictions belong in a vignette or `notes/`.

Push runs a local six-check gate, armed once per clone with
`pre-commit install && pre-commit install --hook-type pre-push`. For the gate,
its skip flags, and the spelling/WORDLIST rule, see CONTRIBUTING.md.

fp issue-tracking rules are fp-managed; regenerate them with `fp agent setup
standard` rather than editing FP_AGENTS.md by hand.

## A red gate on an untouched tree

Toolchain drift makes the verify gate go red on a tree nobody changed, and it
looks exactly like a defect in the change being made. `scripts/check-toolchain.R`
runs ahead of the expensive step and names it in one line: roxygen2's installed
version against this package's `Config/roxygen2/version`, and any installed
package built under a newer R than the one running. Both have happened, and both
cost an afternoon (SEOR-tcytizic).

If that check passes and the gate is still red on a tree you have not touched,
say so and keep the evidence rather than assuming your change caused it.

@FP_AGENTS.md
