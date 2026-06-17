# Contributing

## Verification gate (local pre-push hook)

The GitHub-hosted CI workflows (`Verify` = lint + R CMD check, and
`news-version` = NEWS/DESCRIPTION consistency) are **temporarily gated to
CRAN-submission prep only** — they run on `workflow_dispatch` and release tags
(`v*`), not on routine pushes or PRs, because GitHub-hosted runners are disabled
on this account.

So the routine gate runs **locally**, as a committed pre-push hook in
`.githooks/pre-push`. It is the **light, deterministic** gate — the two cheap
checks that need no extra toolchain:

- top `NEWS.md` heading matches `DESCRIPTION` `Version:` (or is `(development version)`)
- `lintr::lint_package()` reports no lints

Enable it once per clone:

```bash
git config core.hooksPath .githooks
```

It blocks a push that would turn the `lint` / `news-version` workflows red.
Emergency bypass: `SKIP_VERIFY=1 git push`.

### Heavyweight checks live at CRAN-submission time

`R CMD check`, the cross-platform matrix (`full-check.yml`), and R-hub
(`rhub.yaml`) are the **"remote testing when submitting to CRAN"** path — they
run on demand / at release-tag time and require the full `Suggests` set
(`covr`, `goodpractice`, `DT`, `visNetwork`, ...). The pre-push hook
deliberately does **not** run `R CMD check`: without those packages installed a
local check fails on optional-dependency artefacts (e.g. the `visNetwork`
explorer tests) that are green in CI. Before tagging a release, install the
Suggests and run `R CMD check` (or `rcmdcheck::rcmdcheck()`) locally as a
pre-flight.

### Restoring per-push/PR remote CI

When GitHub-hosted runners are available again, re-enable remote feedback by
uncommenting the `push.branches` / `pull_request` triggers in
`.github/workflows/verify.yml` and `.github/workflows/news-version.yaml`.
