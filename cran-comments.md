<!--
NOTE: This file is authored ahead of an actual CRAN submission.
pagerankr is NOT being submitted to CRAN yet. Before any real submission,
re-run the platform checks below (local, plus win-builder and R-hub runs) and
refresh these results so they reflect the exact tarball being submitted. The
repository CI is dormant: the workflows target GitHub Actions and the project's
canonical home is now GitLab, with no pipeline ported yet.
-->

## Test environments

- Local: macOS (darwin), R 4.6.0 -- `rcmdcheck::rcmdcheck(args = "--as-cran")`

win-builder and R-hub have not yet been run; they should be run before an
actual submission.

## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is the CRAN-incoming feasibility note:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Bart Turczynski <bartek@turczynski.pl>'

  New submission

  Unknown, possibly misspelled, field in DESCRIPTION: 'Remotes'

Explanation:

- **New submission** -- this is expected; pagerankr 0.1.0 is a first release.
- **`Remotes` field** -- the package Imports 'rurl' (>= 3.0.0), which is not
  yet on CRAN, so `DESCRIPTION` currently declares a `Remotes:` entry pointing
  at the GitLab source. CRAN does not honor the `Remotes` field, so this must
  be resolved before a real CRAN submission (i.e. 'rurl' >= 3.0.0 must be on
  CRAN and the `Remotes` field removed). This is tracked separately.

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
