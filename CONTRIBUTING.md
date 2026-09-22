# Contributing

## Verification gate

The four checks run remotely in **GitLab CI** (`.gitlab-ci.yml`): `news-version`
(NEWS/DESCRIPTION consistency), `lint` (lintr + spelling) and `check`
(`R CMD check`). They run on every merge request, every push to `main`, and on
`v*` tags. Alongside them, `codemeta` checks `codemeta.json` against
`DESCRIPTION`, and `coverage` measures test coverage — reported through GitLab's
own cobertura ingestion, so merge requests get per-line coverage in the diff
without a Codecov account. Coverage is `allow_failure`, deliberately: coverage
that blocks a merge turns every honest refactor into a fight with a number.

Two things about that pipeline are worth knowing before you rely on it.

**It runs on a self-hosted runner, not GitLab's shared fleet.** The namespace is
on the free plan, where shared runners return `ci_quota_exceeded` before
executing a line, and this project does not pay for minutes. The gate therefore
depends on one machine being awake with Docker running: a push made while it is
not will sit *queued* rather than fail, so a merge request with no pipeline
result is waiting, not passing.

**The nine `.github/workflows/` files are dormant and are NOT the gate.** They
target a suspended account and have not run since 2026-08-07. They are kept as
the source material for what GitLab CI cannot carry — the macOS and Windows
checks, R-hub, and the OSS Index audit — not because they execute. What could be
ported has been (PAGE-ppmceqnr); `.gitlab-ci.yml`'s header records each
remaining gap and why it is a gap rather than a to-do.

The same checks also run **locally**, as a committed pre-push hook script,
`.githooks/pre-push`, so a red result costs seconds instead of a round trip
through CI. It runs five checks, cheapest first:

1. top `NEWS.md` heading matches `DESCRIPTION` `Version:` (or is `(development version)`)
2. citation metadata vs `DESCRIPTION` (`scripts/check-citation.py`, the same
   script the `citation-version` CI job runs)
3. `lintr::lint_package()` reports no lints
4. `spelling::spell_check_package()` reports no misspellings against
   `DESCRIPTION` `Language: en-US`
5. `R CMD check --as-cran` — **fails on errors AND warnings** (the package is
   warning-clean; the only allowed NOTE is the CRAN-incoming new-submission
   one)

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

It blocks a push that would turn the `lint` / `news-version` /
`citation-version` / `check` CI jobs red.
Emergency bypass: `SKIP_VERIFY=1 git push` (skips all five);
`SKIP_RCMDCHECK=1 git push` (skips only step 5); `SKIP_SPELLING=1 git push`
(skips only step 4).

### Spelling (step 4)

Spelling is gated because prose regressed silently once already: the
case-study vignette introduced two en-GB spellings and two untracked terms
*after* `inst/WORDLIST` was built, and nothing caught them (PAGE-xevimisi).
Fix genuine misspellings in the text; add real terms to `inst/WORDLIST`
(`spelling::update_wordlist()` refreshes it). When `spelling` is not installed
the step is **skipped with a warning**, so clones predating its addition to
`Suggests` are not broken.

### Step 5 needs the Suggests toolchain

`R CMD check` (step 5) requires the full `Suggests` set (`covr`,
`goodpractice`, `DT`, `visNetwork`, `rcmdcheck`, ...). When `rcmdcheck` is not
installed, step 5 is **skipped with a warning** rather than failing, so a fresh
clone still gets the light gate (steps 1-4). Install the Suggests to arm the
full check — it is what catches correctness regressions such as a dependency
bump that turns test fixtures red (this class of failure previously slipped
onto `main` while remote CI was billing-disabled; see PR #50).

The cross-platform matrix (`full-check.yml`) and R-hub (`rhub.yaml`) are the
**"remote testing when submitting to CRAN"** path — on demand / at release-tag
time, because that matrix is slow rather than expensive.

One slice of the matrix now runs on GitLab: `check-oldrel` checks the package
under the previous R minor release, manually on branches and automatically on
`v*` tags. The rest cannot follow it, and a CRAN submission still has to account
for that:

- **macOS and Windows** have no runner. The self-hosted runner is Docker on one
  Mac, so Linux containers only, and shared runners are unusable on the free
  plan.
- **R-devel** is left out on purpose: `rocker/r-devel` publishes no arm64
  variant, so it would run emulated on this host.
- **R-hub cannot be ported at all.** R-hub v2 works by dispatching workflows
  inside a GitHub repository; there is no GitLab equivalent to translate.

So **a CRAN submission still needs R-hub and win-builder run by hand.** Nothing
in GitLab CI substitutes for either, and `check-oldrel` does not narrow that gap
— it widens R-version coverage, not platform coverage.

## The rurl floor job (`rurl-floor`)

`Imports:` declares a minimum rurl. Nothing else installs *that* version — the
other jobs resolve whatever CRAN currently serves — so without this job the
floor is a number someone reasoned to rather than a release anything ran
against. That gap is how `rurl (>= 2.1.0)` survived while rurl had no `v2.1.0`
at all, and how `>= 3.0.0` survived after it: rurl went 1.2.0 straight to 3.0.1
on CRAN and never published a 3.0.0 (PAGE-tsbkxhoz).

The job reads the floor out of `DESCRIPTION`, checks CRAN has actually published
that version — current **or archived**, since a floor is normally superseded —
installs exactly it into a job-local library, and runs the suite with that
library prepended.

It resolves from **CRAN, not a forge tag**. Until 2026-09-09 rurl was off CRAN,
`DESCRIPTION` carried `Remotes: gitlab::bart-turczynski/rurl`, and this job read
the repo and its host out of that field to install a git tag. rurl is on CRAN
now and the field is gone (CRAN does not honor it), so the installable version
is the one CRAN serves. A tag that never became a release is not a floor a user
can reach — which is precisely the failure `>= 3.0.0` was.

It runs on **every merge request**. It was manual while it could not possibly
pass; it can now, so it gates like any other check.

Two properties to preserve when editing it:

- **It must stay additive.** Making an existing job pin the floor would trade
  away coverage of the version users actually get.
- **It must not write the floor into the shared library.** rurl is installed
  into `.rurl-floor-lib` and prepended via `R_LIBS` for the test step only.
  Installing over the top would let the cached dependency library the other
  jobs share come back holding the floor instead of the CRAN version users
  actually get.

If it goes red, the floor is wrong, not the suite: correct `Imports:` to the
lowest released version that passes.

**What no longer happens.** While `Remotes:` existed, every routine job resolved
rurl to **main HEAD**, which gave continuous reverse-dependency coverage against
unreleased upstream work. Removing the field removed that: drift is now bounded
by rurl *releases* rather than rurl *commits*, so an upstream break reaches
pagerankr only once it is already on CRAN. Restoring it needs a separate
additive job (`PAGE-majaowtn`); until that exists, this section does not claim
coverage the pipeline does not have.
