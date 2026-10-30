# Contributing

Report bugs and request features in the GitLab issue tracker:
<https://gitlab.com/bart-turczynski/pagerankr/-/work_items>. Report security issues
privately as described in `SECURITY.md`. Send changes as merge requests on
GitLab; the GitHub repository is a read-only mirror.

New code needs tests, and each user-facing change needs one `NEWS.md` bullet.
A merge request must pass the verification gate below.

Install dependencies:

```sh
Rscript -e 'pak::local_install_deps(dependencies = TRUE)'
```

## Verification gate

Verification is fleet-ci's local gate, which runs in the fleet Docker image
(settings in `.fleet-ci.yml`), so it needs Docker, python3, Ruby, git,
[`pre-commit`](https://pre-commit.com/) (for example `pipx install
pre-commit`) and a `docker login registry.gitlab.com`. Install the hooks once
per clone, from the package root:

```sh
pre-commit install
.fleet-ci/pre-push --install
```

From then on the gate runs on every push, on each pushed ref's tip, followed
by the pre-commit pre-push hooks (the citation, BugReports, NEWS terseness and
toolchain checks). `pre-commit install -f` (which installs both hook types,
from `.pre-commit-config.yaml`'s `default_install_hook_types`) and
`pre-commit install -f --hook-type pre-push` replace that hook and drop the
gate; re-run `.fleet-ci/pre-push --install` after either. Without `-f`,
`pre-commit install` keeps the gate (pre-commit's migration mode). To run the
gate by hand on `HEAD`:

```sh
.fleet-ci/gate auto
```

The gate checks the committed tree, never uncommitted edits: an uncommitted
`devtools::document()` fix does not pass it, so commit the fix. A diff that is
docs-only (`design/**/*.md` and the like) gets the docs tier: spelling and
`pkgdown::check_pkgdown()`. Any other diff gets the full tier: spelling,
`pkgdown::check_pkgdown()`, the `NEWS.md` heading against `DESCRIPTION`'s Version, `codemeta.json`'s version,
docs drift (`man/`, `NAMESPACE` and `DESCRIPTION` against a fresh roxygen2
run), lintr, `README.md` against a fresh knit of `README.Rmd`, the URL check,
and one `R CMD check --as-cran` (incoming checks off), which **fails on errors
and warnings**. A test skip that `.fleet-ci.yml` does not allow fails it too.
GitLab CI runs no pipeline for a branch, a merge request or a push to `main`;
what it runs, and when, is in
[`design/ci.md`](https://gitlab.com/bart-turczynski/pagerankr/-/blob/main/design/ci.md).

### Spelling

Fix genuine misspellings in the text; add real terms to `inst/WORDLIST`
(`spelling::update_wordlist()` refreshes it).

## Where else to look

The source layout and durable project context live in `ARCHITECTURE.md`
and `design/`: CI, the local gate and the project settings CI depends on in
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
