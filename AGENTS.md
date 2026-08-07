R package `pagerankr`: SEO-focused PageRank modeling on crawl data — edge lists, redirect
reports, Screaming Frog exports. Lifecycle experimental; not on CRAN yet.

`rurl` installs from GitHub main via `Remotes`, deliberately unpinned. Its canonicalization
profile decides node identity, so a profile change re-keys the graph and must stay in sync
with the other rurl consumers.

`page_state` (crawl-derived page condition) and `node_status` (graph role) are separate
axes; keep them separate.

`NEWS.md` describes changes to the library. Crawl measurements and ranking-outcome
predictions belong in a vignette or `notes/`.

Push runs a local four-check gate, armed once per clone with
`git config core.hooksPath .githooks`. For the gate, its skip flags, and the
spelling/WORDLIST rule, see CONTRIBUTING.md.

fp issue-tracking rules are fp-managed; regenerate them with `fp agent setup
standard` rather than editing FP_AGENTS.md by hand.

@FP_AGENTS.md
