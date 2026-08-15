# Contributing

## Verification gate

The four checks run remotely in **GitLab CI** (`.gitlab-ci.yml`): `news-version`
(NEWS/DESCRIPTION consistency), `lint` (lintr + spelling) and `check`
(`R CMD check`). They run on every merge request, every push to `main`, and on
`v*` tags.

Two things about that pipeline are worth knowing before you rely on it.

**It runs on a self-hosted runner, not GitLab's shared fleet.** The namespace is
on the free plan, where shared runners return `ci_quota_exceeded` before
executing a line, and this project does not pay for minutes. The gate therefore
depends on one machine being awake with Docker running: a push made while it is
not will sit *queued* rather than fail, so a merge request with no pipeline
result is waiting, not passing.

**The nine `.github/workflows/` files are dormant and are NOT the gate.** They
target a suspended account and have not run since 2026-08-07. They are kept as
the source material for anything still unported — the cross-platform matrix,
R-hub, the codemeta refresh, and the OSV / OSS Index audits — not because they
execute.

The same checks also run **locally**, as a committed pre-push hook in
`.githooks/pre-push`, so a red result costs seconds instead of a round trip
through CI. It runs four checks, cheapest first:

1. top `NEWS.md` heading matches `DESCRIPTION` `Version:` (or is `(development version)`)
2. `lintr::lint_package()` reports no lints
3. `spelling::spell_check_package()` reports no misspellings against
   `DESCRIPTION` `Language: en-US`
4. `R CMD check --as-cran` — **fails on errors AND warnings** (the package is
   warning-clean; the only allowed NOTE is the CRAN-incoming dev-version /
   `Remotes` one)

Enable it once per clone:

```bash
git config core.hooksPath .githooks
```

It blocks a push that would turn the `lint` / `news-version` / `check` CI jobs
red.
Emergency bypass: `SKIP_VERIFY=1 git push` (skips all four);
`SKIP_RCMDCHECK=1 git push` (skips only step 4); `SKIP_SPELLING=1 git push`
(skips only step 3).

### Spelling (step 3)

Spelling is gated because prose regressed silently once already: the
case-study vignette introduced two en-GB spellings and two untracked terms
*after* `inst/WORDLIST` was built, and nothing caught them (PAGE-xevimisi).
Fix genuine misspellings in the text; add real terms to `inst/WORDLIST`
(`spelling::update_wordlist()` refreshes it). When `spelling` is not installed
the step is **skipped with a warning**, so clones predating its addition to
`Suggests` are not broken.

### Step 4 needs the Suggests toolchain

`R CMD check` (step 4) requires the full `Suggests` set (`covr`,
`goodpractice`, `DT`, `visNetwork`, `rcmdcheck`, ...). When `rcmdcheck` is not
installed, step 4 is **skipped with a warning** rather than failing, so a fresh
clone still gets the light gate (steps 1-3). Install the Suggests to arm the
full check — it is what catches correctness regressions such as a dependency
bump that turns test fixtures red (this class of failure previously slipped
onto `main` while remote CI was billing-disabled; see PR #50).

The cross-platform matrix (`full-check.yml`) and R-hub (`rhub.yaml`) are the
**"remote testing when submitting to CRAN"** path — on demand / at release-tag
time, because that matrix is slow rather than expensive. Neither was ported, so
both are dormant with the rest of the GitHub Actions set: a CRAN submission
needs them ported or run by hand first. The single-platform `check` job in
GitLab CI does not substitute for either.

## The rurl floor job (`rurl-floor`)

Every job above resolves `Remotes: gitlab::bart-turczynski/rurl` to **main
HEAD**.
That is deliberate — it gives continuous reverse-dependency coverage against
upstream — but it means none of them installs the version `Imports:` actually
requires, so the declared floor is never exercised. That is how
`rurl (>= 2.1.0)` survived while `rurl` had no `v2.1.0` tag at all: a minimum
no user could install, and a green board (PAGE-tsbkxhoz).

The `rurl-floor` job closes that gap. It reads the floor, the source repo and
its forge out of `DESCRIPTION` — a bare `owner/repo` means GitHub, matching
`remotes`' own default, and a `gitlab::` prefix sends every query to GitLab
instead — then fails if the version names a tag that does not exist, installs
that tag into a job-local library, and runs the suite with it prepended.

**It is expected red today, and that is the correct reading.** `Imports:`
declares `rurl (>= 3.0.0)`; rurl's newest tag is `v2.2.1`, and the code genuinely
depends on 3.0.0's argument surface after commit `fc1caba` — measured against
both released candidates, which fail (v2.2.1: 157 errors; v2.2.0: red on
canonicalization alone). So the floor is the *right* number naming a version
nobody can install yet.

Because of that it is **manual** on branches and merge requests — a permanently
red job must not block unrelated work — and **automatic on scheduled
pipelines**, which is where the signal is wanted: the next schedule after rurl
3.0.0 is tagged turns it green on its own. Flip it to `when: always` for merge
requests then.

Two properties to preserve when editing it:

- **It must stay additive.** Making an existing job pin the floor would trade
  away the main-HEAD revdep coverage, which is relied on deliberately
  (PAGE-fjqyruaf).
- **It must not write the floor into the shared library.** rurl is installed
  into `.rurl-floor-lib` and prepended via `R_LIBS` for the test step only.
  Installing over the top would let the cached dependency library the other
  jobs share come back holding the floor instead of main HEAD.

If it goes red, the floor is wrong, not the suite: correct `Imports:` to the
lowest released version that passes.
