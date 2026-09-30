# Contributing

## Verification gate

The four checks run remotely in **GitLab CI** (`.gitlab-ci.yml`): `news-version`
(NEWS/DESCRIPTION consistency), `lint` (lintr + spelling) and `check`
(`R CMD check`). They run on every push to `main` and on `v*` tags — **not** on
merge requests or feature-branch pushes; see "One pipeline, not three" below.
Alongside them, `codemeta` checks `codemeta.json` against `DESCRIPTION`, and
`coverage` measures test coverage — reported through GitLab's own cobertura
ingestion. Coverage is `allow_failure`, deliberately: coverage that blocks a
merge turns every honest refactor into a fight with a number.

Two things about that pipeline are worth knowing before you rely on it.

**It runs on a self-hosted runner, not GitLab's shared fleet.** This is not a
platform restriction — the free plan does grant shared-runner minutes. The
namespace has simply used its allowance: shared runners return
`ci_quota_exceeded` before executing a line because the Free plan's 400
minutes/month were spent (406/400, measured 2026-09-23), and this project does
not pay for more. That fact is true and worth knowing on its own, but it does
not explain why this pipeline works: self-hosted runner minutes do not count
against that quota at all, quota-exhausted or not. The eight repos in this
fleet, pagerankr included, run on Docker runners registered to one Mac
(`~/.gitlab-runner/config.toml`), which is a machine this project's owner
runs, not a GitLab-hosted resource — see
`design/adr/0003-fleet-ci-runs-on-self-hosted-runners.md` in the sibling
`seor` repo for how this was verified. The gate therefore depends on one
machine being awake with Docker running: a push to `main` made while it is not
will sit *queued* rather than fail, so a `main` commit with no pipeline result
yet is waiting, not passing.

**One pipeline, not three.** `workflow:` in `.gitlab-ci.yml` suppresses both the
merge-request pipeline and the branch pipeline, leaving only the one on `main`
(and on `v*` tags). A feature-branch push, and the merge request built from it,
get **no CI at all** — not a red result, no result. That is deliberate, not a
gap: on this fleet's runners the branch and MR pipelines were created
*before* the one on `main`, tested the same tree it was about to test again,
and could hold a runner slot for a full check run after their own branch was
already deleted at merge (SEOR-bmgkzhvy). Raising `concurrent` does not fix
that — it is a queue-ordering collision within one project, not a capacity
shortage, and stays possible at any concurrency (this fleet's runners are
configured for `concurrent = 4`, a value in `~/.gitlab-runner/config.toml` on
the host this project's owner controls, not a GitLab-imposed limit). It costs
nothing here in
practice — `only_allow_merge_if_pipeline_succeeds` is `false` on this project
(verified against the `projects` API), so a pipeline result has never gated a
merge; `main` is protected by role (Maintainer-only push/merge, no force-push),
not by CI status — and `.githooks/pre-push` runs the same checks locally
*before* the push happens, so a red result costs seconds, not a round trip
through CI. What it does cost: GitLab's per-line coverage diff annotation on a
merge request needs a pipeline associated with that MR, and none runs now, so
`coverage` numbers only ever land on `main`, after the fact — not in the MR
diff. An API-triggered pipeline on a non-default branch also produces nothing,
for the same reason (`$CI_COMMIT_BRANCH` is set but not `$CI_DEFAULT_BRANCH`,
so `workflow:` falls through to `when: never`).

**A pipeline you start by hand is the exception.** Open **Build > Pipelines >
Run pipeline**, pick the branch, and the full gate runs against it — this is
how you get a server-side answer about a branch before merging it, and it is
worth doing for anything the local hook cannot speak to. Only `pages` is out
of reach, pinned to `main` because it publishes rather than reports. It moves
every top-level `.md` file that is not on an explicit public keep-list out of
the way first — pkgdown renders every top-level `.md`, and its own skip list
(`README`/`NEWS`/`LICENSE`) is hardcoded and cannot be extended from
`_pkgdown.yml`, so `AGENTS.md`, `CLAUDE.md` and friends were being published
next to the function reference as `AGENTS.html`, `CLAUDE.html`: internal
working notes served as if they were user documentation (SEOR-pibdjanz). A
glob naming the private families (`rm -f AGENTS*.md CLAUDE*.md FP_*.md`) used
to cover this, but a glob is fail-open: a family it does not name is public
by default until someone notices and extends it, which is how the leak
reached four repos after being fixed in one (SEOR-wqxhftpv). The job now
names what IS public instead — README, NEWS, LICENSE, CONTRIBUTING,
SECURITY, CODE_OF_CONDUCT, THIRD_PARTY_NOTICES, plus `ACKNOWLEDGMENTS.md`
(linked from `_pkgdown.yml`'s navbar) — and `mv`s everything else into
`/tmp/agent-md/` immediately before `build_site`, so an unnamed file (a
future `GEMINI.md`, `cran-comments.md`, anything) stays private by default.
The job aborts if one of the named public docs is missing, so a rename can't
silently drop it from the site without the build failing loudly. Note the
button specifically: `glab ci run` starts an `api`-source pipeline, which
`workflow:` still refuses on a branch.

**There is no GitHub Actions gate.** The old `.github/workflows/` files were
deleted (PAGE-yfmrrrhp); GitHub now holds a read-only mirror with Actions
turned off. What GitLab CI could carry has been ported (PAGE-ppmceqnr); what it
cannot — the macOS and Windows checks and R-hub — is recorded in
`.gitlab-ci.yml`'s header, with why each is a gap rather than a to-do.

**The two dependency audits run only on a schedule.** `osv-audit` (OSV, no
account) and `security-audit` (Sonatype OSS Index, needs the `OSSINDEX_USER` /
`OSSINDEX_TOKEN` CI/CD variables) fire on a pipeline schedule that sets
`SCHEDULE_KIND=dependency-audit`, or by hand from **Run pipeline**. No push
pipeline runs them.

The same checks also run **locally**, as a committed pre-push hook script,
`.githooks/pre-push`, so a red result costs seconds instead of a round trip
through CI. It runs seven gates, cheapest first, and reports every failure in
one summary rather than stopping at the first:

1. `news-version`: top `NEWS.md` heading matches `DESCRIPTION` `Version:` (or
   is `(development version)`), via `scripts/gates.R`, the file the `gates` CI
   job runs
2. `codemeta`: `codemeta.json` version and URL sanity, also via
   `scripts/gates.R`
3. `citation`: citation metadata vs `DESCRIPTION` (`scripts/check-citation.py`,
   the same script the `citation-version` CI job runs)
4. `bugreports`: the BugReports tracker-link split
   (`scripts/check-bugreports.py`, also run by `citation-version`)
5. `lint`: `lintr::lint_package()` reports no lints
6. `spelling`: `spelling::spell_check_package()` reports no misspellings
   against `DESCRIPTION` `Language: en-US`
7. `rcmdcheck`: `R CMD check --as-cran`, which **fails on errors AND warnings**
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
Emergency bypass: `SKIP_VERIFY=1 git push` (skips all seven);
`SKIP_RCMDCHECK=1 git push` (skips only gate 7); `SKIP_SPELLING=1 git push`
(skips only gate 6). An opt-in skip reports as SKIP in the summary, never as
PASS.

### Lint (gate 5): the linter set

`.lintr` is intentionally aligned with the linter set `goodpractice::gp()` runs
(`goodpractice:::linters_to_lint()`), so the local and CI `lintr::lint_package()`
gate surfaces the same findings as the goodpractice report reviewers run. Without
that alignment a package passes its own lint gate and then trips a pile of
goodpractice findings later. Regenerate the list after a goodpractice upgrade,
comparing against `names(goodpractice:::linters_to_lint())`.

**Keep `.lintr` free of `#` comments.** It is parsed with `read.dcf()`, which
only learned to skip comment lines in R 4.6. On R 4.5 and older a single comment
makes `lint_package()` abort with `Invalid DCF format`, so the rationale lives
here instead. Keep it ASCII too: a non-ASCII byte in `.lintr` comes back
`bytes`-encoded on older R and makes any config error surface as a confusing
`sprintf()` failure instead of the real message.

Documented deviations from the goodpractice set — test-idiom and public-API
reasons a real package hits as it grows:

- `object_name_linter` / `object_usage_linter`: not part of the goodpractice set
  and deliberately NOT added. The cucumber DSL (`when`/`then`/`context`) and the
  testthat helpers read as undefined globals to `object_usage_linter`, and
  packages commonly expose mixed-case or dotted public parameters plus
  `._`-prefixed internal helpers that `object_name_linter` would flag.
- `expect_identical_linter`: off. Suites routinely rely on `expect_equal()`'s
  numeric tolerance (`expect_equal(nrow(x), 2)` compares integer vs double) and
  its string-encoding normalization, both of which `identical()` rejects; a
  wholesale swap means retyping literals for no behavioral gain.
- `implicit_assignment_linter`: off. Tests use the standard
  `expect_warning(res <- f(), "msg")` idiom to capture both the warning and the
  return value (`expect_warning()` returns the condition, not the value).
- `library_require_linter`: off. `tests/testthat.R` and vignette setup chunks
  legitimately call `library()`.
- `undesirable_operator_linter`: configured to keep flagging `<<-`/`->>` but
  allow `:::`, which tests use to reach internal (unexported) functions.

`strings_as_factors_linter` is off, as in goodpractice, which dropped it in 1.2.0
(ropensci-review-tools/goodpractice#321). It only guarded the pre-R-4.0
`data.frame()` default, and this package Depends on R >= 4.0.0. The fleet
turned it off on 2026-07-18, before goodpractice did (`PAGE-iiqjlfxl`).
Its absence does not make `stringsAsFactors = FALSE` removable everywhere:
`expand.grid()` kept `TRUE` as its default through R 4.0, so the argument in
`R/auto_grid.R` is load-bearing.

**Measuring cyclomatic complexity.** `cyclocomp::cyclocomp_package_dir()` does
not see `.`-prefixed functions, and those are most of this package: it reports
83 functions against the 447 in the namespace, 364 of them dot-prefixed
(measured 2026-09-29). Score the loaded namespace instead:

```r
pkgload::load_all(".")
ns <- asNamespace("pagerankr")
fns <- Filter(function(n) is.function(get(n, envir = ns)), ls(ns, all.names = TRUE))
sort(vapply(fns, function(n) cyclocomp::cyclocomp(get(n, envir = ns)), 0), decreasing = TRUE)
```

The cheapest reduction is usually splitting one `&&`/`||` guard chain into
sequential single-condition `if`s: a four-way `||` guard scores 11, the same
four checks as separate `if`s score 5.

### Spelling (gate 6)

Spelling is gated because prose regressed silently once already: the
case-study vignette introduced two en-GB spellings and two untracked terms
*after* `inst/WORDLIST` was built, and nothing caught them (PAGE-xevimisi).
Fix genuine misspellings in the text; add real terms to `inst/WORDLIST`
(`spelling::update_wordlist()` refreshes it). When `spelling` is not installed
the gate **fails**; install it, or push with `SKIP_SPELLING=1` deliberately.

### Gate 7 needs the Suggests toolchain

`R CMD check` (gate 7) requires the full `Suggests` set (`covr`,
`goodpractice`, `DT`, `visNetwork`, `rcmdcheck`, ...). When `rcmdcheck` is not
installed, gate 7 **fails** rather than skipping, so a fresh clone must install
the Suggests (or push with `SKIP_RCMDCHECK=1` deliberately). The full check — it is what catches correctness regressions such as a dependency
bump that turns test fixtures red (this class of failure previously slipped
onto `main` while remote CI was billing-disabled; see PR #50).

The cross-platform matrix (`full-check.yml`) and R-hub (`rhub.yaml`) are the
**"remote testing when submitting to CRAN"** path — on demand / at release-tag
time, because that matrix is slow rather than expensive.

One slice of the matrix now runs on GitLab: `check-oldrel` checks the package
under the previous R minor release, automatically on `v*` tags and manually
otherwise. "Otherwise" is narrower than it used to be: with only `main` and tag
pipelines existing at all (see "One pipeline, not three" above), the manual
trigger is only reachable from a pipeline on `main` — there is no longer a
branch or MR pipeline to run it from before merging. The rest cannot follow it,
and a CRAN submission still has to account for that:

- **macOS and Windows** have no runner. The self-hosted runner is Docker on
  one Mac, so Linux containers only, and shared runners are not an
  alternative today because the namespace's Free-plan quota is exhausted (see
  "It runs on a self-hosted runner, not GitLab's shared fleet" above) — not
  because the plan structurally disallows them.
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

It runs on **every pipeline that exists** — `rules: [{when: always}]`, unfiltered
by pipeline source. In practice that means every push to `main` and every `v*`
tag: merge-request and branch pipelines no longer run at all (see "One pipeline,
not three" above), so it no longer runs on merge requests specifically, only on
`main` after one merges. It was manual while it could not possibly pass; it can
now, so it gates like any other check.

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

## The rurl development job (`rurl-devel`)

While `Remotes:` existed, every routine job resolved rurl to **main HEAD**,
which gave continuous reverse-dependency coverage against unreleased upstream
work (PAGE-fjqyruaf). Removing the field removed that, and `rurl-devel` puts
it back as one additive job (PAGE-majaowtn). It installs rurl from GitLab at
`RURL_DEVEL_REF` (default `main`) into `.rurl-devel-lib`, checks that the suite
will load that install (by its `RemoteSha`), and runs the suite with the
library prepended for the test step only, the same shape as `rurl-floor`, so
the shared cache keeps the CRAN rurl.

It reports on upstream, so it never blocks anything: `allow_failure` whichever
way it runs. It runs on a pipeline schedule that sets
`SCHEDULE_KIND=rurl-devel`, or by hand from **Run pipeline**. Setting
`RURL_DEVEL_REF` on a manual run points it at a rurl branch or commit, which is
how to check that a deliberate upstream break shows up. Coverage is only as
continuous as the schedule: without one, the job runs only when started by
hand, and drift is bounded by rurl releases again.

When it goes red, read it as news about rurl, not about pagerankr: either rurl
`main` broke a reverse dependency, which is fixed in rurl before it releases,
or pagerankr needs to adapt before that rurl release. It is how the
`path_normalisation` guard failure (PAGE-lgsbjjuf) would have surfaced before
rurl's release prep instead of during it.

## Renaming an exported function

`_pkgdown.yml`'s `reference:` index lists topics by name, and a stale entry
fails the site build with "must be a known topic name or alias". Nothing
before the merge sees it: tests, lint and `R CMD check --as-cran` all pass,
the pre-push gate does not run pkgdown, and the `pages` job runs only on
`main`, so the first red result arrives after the merge. The
`resolve_urls()` -> `resolve_redirect_urls()` rename (PR #119) cost a round
trip exactly this way.

On any export rename:

1. Sweep for the old name with **no file-extension filter**:
   `git grep -nwE 'old_name'`. A sweep limited to `.R`/`.Rmd`/`.md`/`.Rd`
   skips `.yml`.
2. Run `pkgdown::check_pkgdown()`. It validates the whole reference index in
   seconds without building the site.
3. Also update `NAMESPACE` (via roxygen), the `R/` and `tests/testthat/` file
   names, `README.Rmd` (then `devtools::build_readme()`), vignette reference
   tables, and a `NEWS.md` entry.

## CRAN release checklist

Follow the fleet checklist,
[seor `design/release-checklist.md`](https://gitlab.com/bart-turczynski/seor/-/blob/main/design/release-checklist.md).
pagerankr's deltas:

- **Step 1: the rurl floor is a CRAN release.** The `rurl-floor` job (see
  "The rurl floor job" above) installs exactly the floor `Imports:` declares
  from CRAN and runs the suite. Confirm it passed on the release commit's
  `main` pipeline. Before win-builder's R-release queue, also check that CRAN
  serves Windows binaries of that rurl version: 0.1.0's R-release run waited
  for them (PAGE-xylymvme).
- **Step 6: `check-oldrel` runs by itself only on the tag.** To see it
  before submission, start it by hand from the release commit's `main`
  pipeline (agent+go).
