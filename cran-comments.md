<!--
NOTE: This file is authored ahead of an actual CRAN submission.
pagerankr is NOT being submitted to CRAN yet. Before any real submission,
re-run the platform checks below (local, plus win-builder and R-hub runs) and
refresh these results so they reflect the exact tarball being submitted.
-->

## Test environments

- Local: macOS (darwin), R 4.6.0 -- `rcmdcheck::rcmdcheck(args = "--as-cran")`
- GitLab CI: Ubuntu (rocker/r-ver:4.6), R 4.6.1 -- lint, spelling and
  `R CMD check`, on every merge request and every push to `main`
- GitLab CI: Ubuntu (rocker/r-ver:4.5), R 4.5.x -- `R CMD check`, on `v*` tags
  and on demand (`check-oldrel`)

**Still only one platform is covered remotely, on two R versions.** Both GitLab
CI jobs are Linux, on a self-hosted runner. macOS and Windows have no runner at
all, and R-hub cannot be ported — it works by dispatching workflows inside a
GitHub repository, so there is nothing to translate. win-builder and R-hub have
therefore **not** been run, and both must be before an actual submission;
nothing in the current CI can stand in for them.

## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is the CRAN-incoming feasibility note:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Bart Turczynski <bartek@turczynski.pl>'

  New submission

  Found the following (possibly) invalid URLs:
    URL: https://gitlab.com/bart-turczynski/pagerankr/-/issues
      From: DESCRIPTION
            man/pagerankr-package.Rd
            README.md
      Status: 404
      Message: Not Found

Explanation:

- **New submission** -- this is expected; pagerankr 0.1.0 is a first release.
- **The `BugReports` 404** -- not a broken link; see the section below.

Every hard dependency now resolves from CRAN. 'rurl' reached CRAN as 3.0.1 on
2026-09-09, so the `Remotes:` field -- which CRAN does not honor, and which
previously drew a second line on this NOTE -- has been removed and `Imports:`
declares 'rurl' (>= 3.0.1), the lowest 'rurl' that exists as an installable
release and that the suite has been run against.

## A note on the `BugReports` URL

An automated URL check may report `BugReports:`
(`https://gitlab.com/bart-turczynski/pagerankr/-/issues`) as **404**. This is a
GitLab.com behavior, not a broken link: gitlab.com serves 404 for issue *list*
pages to unauthenticated automated clients. The same request against
`https://gitlab.com/gitlab-org/gitlab/-/issues` -- one of the most public
trackers on the site -- returns 404 identically, while the anonymous REST API
(`/api/v4/projects/<id>/issues`) returns 200 and the page loads normally for a
human in a browser. The URL is correct and reachable; only scripted fetches see
the 404.

## Downstream dependencies

There are no downstream reverse dependencies; this is a new package.

## Notes

This is a new submission (first release of pagerankr).
