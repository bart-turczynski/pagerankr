# Contributing

Report bugs and request features in the GitLab issue tracker:
<https://gitlab.com/bart-turczynski/pagerankr/-/work_items>. Report security issues
privately as described in `SECURITY.md`. Send changes as merge requests on
GitLab; the GitHub repository is a read-only mirror.

New code needs tests, and each user-facing change needs one `NEWS.md` bullet.
A merge request must pass the verification command below.

Install dependencies:

```sh
Rscript -e 'pak::local_install_deps(dependencies = TRUE)'
```

Run verification (the pre-push chain: the hygiene hooks, the toolchain check,
the URL check, then the eight-gate `verify` hook described below):

```sh
pre-commit run --hook-stage pre-push --all-files
```

## Verification gate

GitLab CI runs these checks on every push to `main`; what CI runs, and when,
is in
[`design/ci.md`](https://gitlab.com/bart-turczynski/pagerankr/-/blob/main/design/ci.md).

The same checks also run **locally**, as a committed pre-push hook script,
`.githooks/pre-push`, so a red result costs seconds instead of a round trip
through CI. It runs eight gates, cheapest first, and reports every failure in
one summary rather than stopping at the first:

1. `news-version`: top `NEWS.md` heading matches `DESCRIPTION` `Version:` (or
   is `(development version)`), via `scripts/gates.R`, the file the `gates` CI
   job runs
2. `codemeta`: `codemeta.json` version and URL sanity, also via
   `scripts/gates.R`
3. `citation`: citation metadata vs `DESCRIPTION` (`scripts/check-citation.py`,
   the same script the `citation-version` CI job runs)
4. `bugreports`: `BugReports:` and every tracker link name `/-/work_items`
   (`scripts/check-bugreports.py`, also run by `citation-version`)
5. `lint`: `lintr::lint_package()` reports no lints
6. `spelling`: `spelling::spell_check_package()` reports no misspellings
   against `DESCRIPTION` `Language: en-US`
7. `docs`: `man/`, `NAMESPACE` and `DESCRIPTION` match what roxygen2
   regenerates from `R/` (`scripts/check-docs-drift.R` via `scripts/gates.R`,
   the file the `gates` CI job runs). It checks the **commit being pushed**,
   not the working tree: it exports that commit with `git archive` to a temp
   directory, regenerates there and deletes the directory, so it never
   rewrites your checkout. Under `pre-commit run --all-files` it checks `HEAD`.
   An uncommitted `devtools::document()` fix does not pass it; commit the fix.
8. `rcmdcheck`: `R CMD check --as-cran`, which **fails on errors AND warnings**
   (the package is warning-clean; the only allowed NOTE is the CRAN-incoming
   new-submission one)

Between gates 4 and 5 it prints a non-gating notice when the installed rurl is
older than the version CRAN serves. A **missing checker fails its gate**: no
`python3`, `lintr`, `spelling` or `rcmdcheck` is a broken environment, not a
passing tree (SEOR-dzrisdmi).

The script itself is unchanged; only how it is armed changed. It now runs
through a `pre-commit` local hook (`.pre-commit-config.yaml`) rather than
`git config core.hooksPath`, because that file also carries the fleet's
commit-stage hygiene hooks (trailing-whitespace, large-file guard,
merge-conflict markers, and so on), and `pre-commit install` refuses to run at
all while `core.hooksPath` is set. Enable both once per clone:

```bash
pre-commit install
pre-commit install --hook-type pre-push
```

It blocks a push that would turn the `gates` / `citation-version` / `check`
CI jobs red.
Emergency bypass: `SKIP_VERIFY=1 git push` (skips all eight);
`SKIP_RCMDCHECK=1 git push` (skips only gate 8); `SKIP_SPELLING=1 git push`
(skips only gate 6). An opt-in skip reports as SKIP in the summary, never as
PASS.

### Spelling (gate 6)

Spelling is gated because prose regressed silently once already: the
case-study vignette introduced two en-GB spellings and two untracked terms
*after* `inst/WORDLIST` was built, and nothing caught them (PAGE-xevimisi).
Fix genuine misspellings in the text; add real terms to `inst/WORDLIST`
(`spelling::update_wordlist()` refreshes it). When `spelling` is not installed
the gate **fails**; install it, or push with `SKIP_SPELLING=1` deliberately.

### Gate 8 needs the Suggests toolchain

`R CMD check` (gate 8) requires the full `Suggests` set (`covr`,
`goodpractice`, `DT`, `visNetwork`, `rcmdcheck`, ...). When `rcmdcheck` is not
installed, gate 8 **fails** rather than skipping, so a fresh clone must install
the Suggests (or push with `SKIP_RCMDCHECK=1` deliberately). The full check — it is what catches correctness regressions such as a dependency
bump that turns test fixtures red (this class of failure previously slipped
onto `main` while remote CI was billing-disabled; see PR #50).

## Where else to look

The source layout and durable project context live in `ARCHITECTURE.md`
and `design/`: CI in
[`design/ci.md`](https://gitlab.com/bart-turczynski/pagerankr/-/blob/main/design/ci.md),
the linter set in
[`design/lint.md`](https://gitlab.com/bart-turczynski/pagerankr/-/blob/main/design/lint.md),
renaming an export in
[`design/renaming-exports.md`](https://gitlab.com/bart-turczynski/pagerankr/-/blob/main/design/renaming-exports.md),
and the CRAN release deltas in
[`design/release-deltas.md`](https://gitlab.com/bart-turczynski/pagerankr/-/blob/main/design/release-deltas.md).

Keep local-only planning state in `_scratch/`. Do not commit `_scratch/`,
`tmp/`, `worklog/`, `.fp/`, secrets, dependency folders, build outputs, or
generated caches.
