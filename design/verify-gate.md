# pagerankr's local verify gate

How the pre-push gate (`.githooks/pre-push`, run through pre-commit) works,
moved from `CONTRIBUTING.md`, which keeps what a contributor runs.

## The `docs` gate (gate 7)

It checks the **commit being pushed**,
not the working tree: it exports that commit with `git archive` to a temp
directory, regenerates there and deletes the directory, so it never
rewrites your checkout. Under `pre-commit run --all-files` it checks `HEAD`.

## Notices and missing checkers

Between gates 4 and 5 it prints a non-gating notice when the installed rurl is
older than the version CRAN serves. A **missing checker fails its gate**: no
`python3`, `lintr`, `spelling` or `rcmdcheck` is a broken environment, not a
passing tree (SEOR-dzrisdmi).

## How the hook is armed

The script itself is unchanged; only how it is armed changed. It now runs
through a `pre-commit` local hook (`.pre-commit-config.yaml`) rather than
`git config core.hooksPath`, because that file also carries the fleet's
commit-stage hygiene hooks (trailing-whitespace, large-file guard,
merge-conflict markers, and so on), and `pre-commit install` refuses to run at
all while `core.hooksPath` is set.

## Why spelling is gated (gate 6)

Spelling is gated because prose regressed silently once already: the
case-study vignette introduced two en-GB spellings and two untracked terms
*after* `inst/WORDLIST` was built, and nothing caught them (PAGE-xevimisi).

## Why gate 8 runs the full check

The full check — it is what catches correctness regressions such as a dependency
bump that turns test fixtures red (this class of failure previously slipped
onto `main` while remote CI was billing-disabled; see PR #50).
