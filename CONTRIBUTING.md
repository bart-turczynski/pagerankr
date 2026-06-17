# Contributing

## Verification gate (local pre-push hook)

The GitHub-hosted CI workflows (`Verify` = lint + R CMD check, and
`news-version` = NEWS/DESCRIPTION consistency) are **temporarily gated to
CRAN-submission prep only** — they run on `workflow_dispatch` and release tags
(`v*`), not on routine pushes or PRs, because GitHub-hosted runners are disabled
on this account.

So the routine gate runs **locally**, as a committed pre-push hook in
`.githooks/pre-push`. It mirrors those workflows exactly:

- top `NEWS.md` heading matches `DESCRIPTION` `Version:` (or is `(development version)`)
- `lintr::lint_package()` reports no lints
- a standard `R CMD check` (`--no-manual`) passes

Enable it once per clone:

```bash
git config core.hooksPath .githooks
```

It blocks a push whose tree would turn those workflows red. Emergency bypass:
`SKIP_VERIFY=1 git push`.

The strict cross-platform `--as-cran` matrix (`full-check.yml`) and R-hub
(`rhub.yaml`) still run remotely on demand / at release-tag time — that's the
"send for remote testing when submitting to CRAN" path.

### Restoring per-push/PR remote CI

When GitHub-hosted runners are available again, re-enable remote feedback by
uncommenting the `push.branches` / `pull_request` triggers in
`.github/workflows/verify.yml` and `.github/workflows/news-version.yaml`.
