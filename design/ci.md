# pagerankr's CI

What GitLab CI runs, on which pipelines and runners, and the two rurl
jobs. The local pre-push gate and its eight checks are in
[`CONTRIBUTING.md`](../CONTRIBUTING.md).

## Pipelines and runners

The checks run remotely in **GitLab CI** (`.gitlab-ci.yml`): `gates`
(news-version, codemeta, README drift, lint and spelling), `citation-version`
(citation, BugReports and NEWS terseness) and `check` (`R CMD check --as-cran`, failing on warnings). They run on every
push to `main` and on `v*` tags — **not** on merge requests or feature-branch
pushes; see "One pipeline, not three" below. Alongside them, `coverage`
measures test coverage — reported through GitLab's own cobertura ingestion —
and fails below 95% total coverage, the fleet minimum (seor
`design/fleet-standard.md`). The threshold is never lowered: a package under
it adds tests.

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

## The R-version matrix

The R-version matrix runs on GitLab, on the weekly `deep-check` schedule (a
pipeline schedule that sets `SCHEDULE_KIND=deep-check`), on `v*` tags, by hand
from **Run pipeline**, and from the terminal with
`glab ci run --branch main --variables DEEP_CHECK:1`. `full-check` runs
`R CMD check --as-cran` on R release, oldrel and devel; `floor-check` runs it
on R 4.1.3, the floor `DESCRIPTION` declares, with dependencies from a dated
Posit Package Manager snapshot (the job's comment in `.gitlab-ci.yml` says
which come from CRAN instead, and why). No push pipeline runs them. The
platform half of the old matrix cannot follow, and a CRAN submission still
has to account for that:

- **macOS and Windows** have no runner. The self-hosted runner is Docker on
  one Mac, so Linux containers only, and shared runners are not an
  alternative today because the namespace's Free-plan quota is exhausted (see
  "It runs on a self-hosted runner, not GitLab's shared fleet" above) — not
  because the plan structurally disallows them.
- **R-hub cannot be ported at all.** R-hub v2 works by dispatching workflows
  inside a GitHub repository; there is no GitLab equivalent to translate.

So **a CRAN submission still needs R-hub and win-builder run by hand.** Nothing
in GitLab CI substitutes for either, and the `deep-check` legs do not narrow
that gap — they widen R-version coverage, not platform coverage.

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
