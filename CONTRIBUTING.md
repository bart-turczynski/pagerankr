# Contributing

## Verification gate (local pre-push hook)

The CI workflows (`Verify` = lint + spelling + R CMD check, and `news-version` =
NEWS/DESCRIPTION consistency) are written for GitHub Actions and are currently
**dormant**: the project's canonical home is GitLab and no pipeline has been
ported yet. Until it is, the pre-push hook below is the operative gate, not a
second line of defense — treat a local red as the only signal you will get.

The same checks also run **locally**, as a committed pre-push hook in
`.githooks/pre-push`, so a red result costs seconds instead of a round trip
through Actions. It runs four checks, cheapest first:

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

It blocks a push that would turn the `lint` / `news-version` / `check`
workflows red.
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
time, because that matrix is slow rather than expensive. Both are dormant with
the rest of the GitHub Actions set, so a CRAN submission needs them ported to
GitLab CI (or run by hand) first.

## The rurl floor job (`rurl-floor.yml`)

Every workflow above resolves `Remotes: gitlab::bart-turczynski/rurl` to **main
HEAD**.
That is deliberate — it gives continuous reverse-dependency coverage against
upstream — but it means none of them installs the version `Imports:` actually
requires, so the declared floor is never exercised. That is how
`rurl (>= 2.1.0)` survived while `rurl` had no `v2.1.0` tag at all: a minimum
no user could install, and a green board (PAGE-tsbkxhoz).

`rurl-floor.yml` closes that gap. It reads the floor, the source repo and its
forge out of `DESCRIPTION` — a bare `owner/repo` means GitHub, matching
`remotes`' own default, and a `gitlab::` prefix sends every query to GitLab
instead — then fails if the version names a tag that does not exist, installs
that tag into a job-local library, and runs the suite with it prepended. It runs
**monthly, on demand, and on any pull request touching `DESCRIPTION`** — the
path filter catches a bad floor when it is proposed, the schedule catches
upstream retagging or deleting a release underneath a floor that was fine when
it was written.

Two properties to preserve when editing it:

- **It must stay additive.** Making an existing job pin the floor would trade
  away the main-HEAD revdep coverage, which is relied on deliberately
  (PAGE-fjqyruaf).
- **It must not write the floor into the shared library.** rurl is installed
  into `.rurl-floor-lib` and prepended via `R_LIBS` for the test step only.
  Installing over the top would let the cached dependency library the other
  workflows share come back holding the floor instead of main HEAD.

If it goes red, the floor is wrong, not the suite: correct `Imports:` to the
lowest released version that passes.
