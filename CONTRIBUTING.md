# Contributing

## Verification gate (local pre-push hook)

The GitHub-hosted CI workflows (`Verify` = lint + R CMD check, and
`news-version` = NEWS/DESCRIPTION consistency) run on **every push to `main` and
every pull request**.

The same checks also run **locally**, as a committed pre-push hook in
`.githooks/pre-push`, so a red result costs seconds instead of a round trip
through Actions. It runs three checks, cheapest first:

1. top `NEWS.md` heading matches `DESCRIPTION` `Version:` (or is `(development version)`)
2. `lintr::lint_package()` reports no lints
3. `R CMD check --as-cran` — **fails on errors AND warnings** (the package is
   warning-clean; the only allowed NOTE is the CRAN-incoming dev-version /
   `Remotes` one)

Enable it once per clone:

```bash
git config core.hooksPath .githooks
```

It blocks a push that would turn the `lint` / `news-version` / `check`
workflows red.
Emergency bypass: `SKIP_VERIFY=1 git push` (skips all three);
`SKIP_RCMDCHECK=1 git push` (skips only step 3).

### Step 3 needs the Suggests toolchain

`R CMD check` (step 3) requires the full `Suggests` set (`covr`,
`goodpractice`, `DT`, `visNetwork`, `rcmdcheck`, ...). When `rcmdcheck` is not
installed, step 3 is **skipped with a warning** rather than failing, so a fresh
clone still gets the light gate (steps 1-2). Install the Suggests to arm the
full check — it is what catches correctness regressions such as a dependency
bump that turns test fixtures red (this class of failure previously slipped
onto `main` while remote CI was billing-disabled; see PR #50).

The cross-platform matrix (`full-check.yml`) and R-hub (`rhub.yaml`) remain the
**"remote testing when submitting to CRAN"** path — they run on demand / at
release-tag time, because that matrix is slow, not because it is expensive (this
repo is public, so GitHub-hosted runners are free).
