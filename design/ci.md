# pagerankr's CI

pagerankr runs fleet-ci's tiered CI (seor ADR 0012). `.gitlab-ci.yml`
includes fleet-ci's `templates/r-package.yml`, pinned by commit SHA, and sets
only package variables plus two `x-*` jobs, the rurl jobs below;
`.fleet-ci.yml` holds the local gate's settings, and `.fleet-ci/` is fleet-ci's
vendored gate (re-run fleet-ci's `bin/vendor`, never edit it). The job names
below are the template's.

| Tier | Where | What runs |
| --- | --- | --- |
| hygiene hooks | pre-commit, at commit and at push | `.pre-commit-config.yaml`. At commit and again at push: the hygiene fixers (end of file, trailing whitespace, line endings), the large-file, merge-conflict, YAML, TOML and case-conflict checks, and codespell. At push only: the citation, BugReports, NEWS terseness and toolchain checks, the `check-citation.py`, `check-bugreports.py`, `check-news.py` and `bestpractices-url.py` self-tests when those scripts change, and the warn-only `fleet-ci-hook-installed` check |
| T0 | every push whose diff against `main` is docs-only (`design/**/*.md` and the like, minus `.fleet-ci.yml`'s `docs_exclude`, `_pkgdown.yml`), locally, the pre-push hook | the gate's docs tier, `.fleet-ci/gate` in the fleet image (`fleet-r`): spelling and `pkgdown::check_pkgdown()` |
| T1 | every other push, locally, the pre-push hook | the gate's full tier: T0's checks, then the `NEWS.md` heading (`scripts/check-news-version.R`), `codemeta.json`'s version (`scripts/check-codemeta.R`), docs drift, lintr, README drift (`scripts/check-readme-drift.sh`), the URL check, and one `R CMD check --as-cran` (incoming checks off) |
| T2 | the weekly `deep-check` schedule; every T3 run | `t2-check-release`, `t2-check-oldrel`, `t2-check-devel`, `t2-floor` (R 4.1.3), `t2-locale` (`R CMD check` under `LC_ALL=C`), `t2-ascran`, `coverage` (fails below 95%), `osv-audit`, `security-audit`, `citation-version`, and pagerankr's `x-rurl-floor` |
| T3 | a `RELEASE=1` web pipeline on `main`; a `v*` tag | all of T2 plus `t3-manual`; the tag, or `RELEASE=1` on a tag, adds `pages` |
| rurl-devel | the weekly `rurl-devel` schedule | `x-rurl-devel` (allowed to fail) and `coverage` |

Pushes to `main` or a branch, merge requests and API pipelines (`glab ci run`)
create no pipeline at all. An empty pipeline list is not a pass: the merge
gate is the local gate (T0 or T1), run before the branch leaves the machine.

Before a CRAN submission, start T3 on `main` from **Build > Pipelines > Run
pipeline**, ref `main`, variable `RELEASE=1` (see
[release-deltas.md](release-deltas.md)). `PAGES=1` on a `v*` tag runs `pages`
alone.

## The hook, once per clone

```sh
pre-commit install
.fleet-ci/pre-push --install
```

It runs the gate on each pushed ref's tip, then the pre-commit pre-push
stage. Two commands drop it: `pre-commit install -f`, which installs both
hook types (`.pre-commit-config.yaml`'s `default_install_hook_types`), and
`pre-commit install -f --hook-type pre-push`; re-run the install after
either. Without `-f`, `pre-commit install` (with or without
`--hook-type pre-push`) moves the hook to `hooks/pre-push.legacy`
(pre-commit's migration mode), which still runs the gate first.
`.fleet-ci/gate auto` runs the gate by hand on `HEAD`. A pass is stamped by
tree, tier, classifier version, image and (full tier) resolved dependency
lock, and a stamped commit is skipped.

The gate checks the pushed commit, exported from git, never the working
tree, so an uncommitted fix cannot pass it (the former hook's docs gate
worked the same way). It installs the dependencies fresh in the container,
from CRAN as the image resolves it, so the former hook's notice about an
installed rurl behind CRAN is gone: the gate never runs against a stale
local rurl. Spelling stays gated because prose regressed silently once: the
case-study vignette introduced two en-GB spellings and two untracked terms
after `inst/WORDLIST` was built (PAGE-xevimisi).

NOTEs from `R CMD check` don't fail the gate, as they did not fail the former
hook (`error_on = "warning"`): `.fleet-ci.yml` sets no `allowed_notes`.

## Runner

Every job runs on pagerankr's own project runner, `pagerankr-local-docker`, a
self-hosted Docker runner on one Mac (seor
`design/adr/0003-fleet-ci-runs-on-self-hosted-runners.md`). The template's
jobs carry no `tags:`, and that runner also runs untagged jobs, so the former
`tags: [local-docker]` is gone. A scheduled or release pipeline started while
the Mac sleeps waits, queued; it has not passed. The images come from
fleet-ci's private registry, pulled with the job token (seor ADR 0013).

## Platforms CI cannot cover

- **macOS and Windows** have no runner: the self-hosted runner is Docker on
  one Mac, so Linux containers only, and the namespace's Free-plan
  shared-runner quota is spent.
- **R-hub cannot be ported at all.** R-hub v2 works by dispatching workflows
  inside a GitHub repository; there is no GitLab equivalent to translate.

So **a CRAN submission still needs R-hub and win-builder run by hand.** T2
widens R-version coverage, not platform coverage. There is no GitHub Actions
gate: GitHub holds a read-only mirror with Actions turned off (PAGE-yfmrrrhp).

## The rurl floor job (`x-rurl-floor`)

`Imports:` declares a minimum rurl. Nothing else installs *that* version: the
other jobs resolve whatever CRAN currently serves, so without this job the
floor is a number someone reasoned to rather than a release anything ran
against. That gap is how `rurl (>= 2.1.0)` survived while rurl had no `v2.1.0`
at all, and how `>= 3.0.0` survived after it: rurl went 1.2.0 straight to 3.0.1
on CRAN and never published a 3.0.0 (PAGE-tsbkxhoz).

The job reads the floor out of `DESCRIPTION`, checks CRAN has actually
published that version (current **or archived**, since a floor is normally
superseded), installs exactly it into a job-local library, and runs the suite
with that library prepended. It resolves from **CRAN, not a forge tag**: rurl
is on CRAN and `Remotes:` is gone, so the installable version is the one CRAN
serves, and the job fails if `Remotes:` comes back.

It extends `.t2-job`, so it runs in every T2 and T3 pipeline, the `RELEASE=1`
pre-submission run among them. Two properties to preserve when editing it:

- **It must stay additive.** Making an existing job pin the floor would trade
  away coverage of the version users actually get.
- **It must not write the floor into the run library.** rurl is installed into
  its own library and prepended for the test run only.

If it goes red, the floor is wrong, not the suite: correct `Imports:` to the
lowest released version that passes.

## The rurl development job (`x-rurl-devel`)

While `Remotes:` existed, every routine job resolved rurl to **main HEAD**,
which gave continuous reverse-dependency coverage against unreleased upstream
work (PAGE-fjqyruaf). Removing the field removed that, and this job puts it
back (PAGE-majaowtn). It installs rurl from GitLab at `RURL_DEVEL_REF`
(default `main`) into a job-local library, checks that the suite will load
that install (by its `RemoteSha`), and runs the suite with the library
prepended for the test run only.

It extends `.schedule-job` with `FLEET_JOB_SCHEDULE: rurl-devel`, so it runs
only on the `rurl-devel` schedule, pagerankr's one schedule exception in the
template's event table (seor ADR 0012), which also runs `coverage`, so the
coverage badge reads a pipeline at least weekly. It reports on upstream, so it
never fails the pipeline (`allow_failure`). When it goes red, read it as news
about rurl, not about pagerankr: either rurl `main` broke a reverse
dependency, which is fixed in rurl before it releases, or pagerankr needs to
adapt before that rurl release. It is how the `path_normalisation` guard
failure (PAGE-lgsbjjuf) would have surfaced before rurl's release prep instead
of during it.

## Project settings CI depends on, which are not in this repository

These live in GitLab's project settings, so a clone does not carry them:

- **Pipeline schedule 4467476** (Build > Pipeline schedules), "Weekly deep
  check", `0 21 * * 3` Europe/Warsaw on `main`, carrying
  `SCHEDULE_KIND=deep-check`. Required: it is T2's only unattended run.
- **Pipeline schedule 4466631**, "rurl-devel", `0 5 * * 1` UTC on `main`,
  carrying `SCHEDULE_KIND=rurl-devel`: `x-rurl-devel` and `coverage`. A
  schedule variable `RURL_DEVEL_REF` points the job at another rurl branch or
  commit; **Run** on the schedule starts it by hand.
- **Pipeline schedule 4467475**, `SCHEDULE_KIND=dependency-audit`. The
  template starts no pipeline for it any more (the audits run in T2); its
  removal is owner-gated.
- **`OSSINDEX_USER` / `OSSINDEX_TOKEN`** (Settings > CI/CD > Variables;
  `OSSINDEX_TOKEN` masked; the user is the literal `x`, not the account
  email). `security-audit` sets `OSSINDEX_AUDIT_REQUIRED=true`, so without
  them it fails rather than passing having audited nothing. oysteR 0.1.4
  hardcodes the legacy OSS Index endpoint, which Sonatype sunsets on
  2026-12-31; after that the job goes red on transport.
- **fleet-ci's job token allowlist** must admit pagerankr, or every fleet
  image pull fails (fleet-ci README, "The registry is private").
- **Pages**: `pages` publishes only the top-level `.md` files on
  `.fleet-ci.yml`'s `pages_keep`.

## Gaps accepted in the fleet-ci migration

- **No server-side cheap gates.** The former `gates` and `citation-version`
  jobs ran the NEWS heading, codemeta, README drift, lint, spelling, docs
  drift, `check-bugreports.py` and `check-news.py` on every push to `main`,
  which caught a web edit or a `--no-verify` push. Those runs are gone; only
  the local gate and the pre-push hooks run them, and T2's
  `citation-version` runs `check-citation.py` alone.
- **No per-push CI check or coverage.** The former `check` (`R CMD check
  --as-cran` with incoming on), `coverage` and `rurl-floor` ran on every push
  to `main`; now T1 runs the check before every push (incoming off), and
  `coverage` runs on the two schedules and in T3, `x-rurl-floor` in T2 and
  T3. A change that breaks the rurl floor shows on the next weekly run, not
  at its merge.
- **No incoming check before a merge.** T1's check runs with
  `_R_CHECK_CRAN_INCOMING_=false`; the incoming feasibility and URL checks run
  in `t2-ascran` (T2 and T3), and `scripts/check-urls.R` still checks every
  declared URL in T1.
- **No hand-started pipeline on a branch.** The former `workflow:` admitted a
  web pipeline on any ref, running the full gate there, and an API pipeline
  with `DEEP_CHECK=1` running the R matrix. The template admits a web
  pipeline only with `RELEASE=1` on `main` or a `v*` tag, or `PAGES=1` on a
  tag; `x-rurl-devel` runs only from its schedule (or **Run** on it).
- **`pages` deploys from a `v*` tag only**, not on every push to `main`, so
  the site documents the release CRAN serves.
- **The floor leg is unproven.** `t2-floor` installs every dependency,
  `Suggests` included, from one dated snapshot (`FLEET_FLOOR_SNAPSHOT`), and
  `rurl (>= 3.0.1)` puts that snapshot in late 2026, after R 4.6.0. The former
  `floor-check` took a 2025 snapshot for igraph, the test and vignette tools
  and rcmdcheck, and installed rurl, punycoder and pslr from CRAN source;
  the template has no such mix. If the first T2 run shows a dependency that
  no longer supports R 4.1, move the date or record `FLEET_FLOOR: "false"`
  here with the reason.
- **Locales.** The former CI had no locale leg and ran no test under a
  non-UTF-8 locale. The one locale run it had was the former hook's, on
  macOS: `test-ascii.R` under `tr_TR.UTF-8`, which skips where the locale is
  missing (as it did on the rocker images). The local gate keeps it:
  `.fleet-ci.yml`'s `locales` builds `tr_TR.UTF-8`, and a skip fails T1.
  `t2-locale` (new) builds the same locale and checks under `LC_ALL=C`, the
  template's default leg, which passed in a local gate run before the
  merge; the gate itself runs no extra locale leg. The T2 check legs run
  without building `locales`, so `test-ascii.R` may skip there, as it did
  in the former CI.
- **No sanitizer leg** (`FLEET_SANITIZERS: "false"`): pagerankr has no
  compiled code, and the former CI never had one.
- **`NOT_CRAN=true` in T1 and the T2 check legs.** The former hook and
  `check` ran `R CMD check` without it; now the `skip_on_cran()` tests run
  there (`test-cli.R`'s large-CSV timing test and its installed-script
  test). The T2 check legs also run `test-osv.R` against the network and
  `test-security.R` (which skips without the OSS Index credentials); T1
  excludes both (`.fleet-ci.yml`'s `exclude_tests`), and `osv-audit` and
  `security-audit` run them. A skipped test fails T1: `allowed_skips` is
  empty. `x-rurl-floor` and `x-rurl-devel` run the suite through
  `testthat::test_local()` and set no `NOT_CRAN` of their own, as the former
  jobs did; their skips (no OSS Index credentials, `tr_TR.UTF-8` missing)
  don't fail them.
- **Self-tests run when their script changes.** The former hook ran the
  `check-citation.py` and `check-bugreports.py` self-tests on every push.
- **The validators that read the old CI are retired**:
  `scripts/check-pkgdown-md-filter.py` (the `pages` keep-list is
  `.fleet-ci.yml`'s `pages_keep`, which the template enforces) and
  `scripts/check-libpaths-order.py` (no job keeps an R library cache), with
  `scripts/gates.R`, whose gates are now `.fleet-ci.yml` cheap gates.
- **Comments in `tests/` still name the former jobs and hook**
  (`test-security.R`, `test-launch_explorer.R`); a CI migration does not
  edit `tests/`.
- **Pandoc** comes from the fleet image (`.fleet-ci/toolchain`, 3.10), which
  reproduces the committed `README.md`. `scripts/check-toolchain.R` checks a
  local pandoc against it, so a local README knit writes what the gate
  accepts.
