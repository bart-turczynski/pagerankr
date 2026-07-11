<!--
NOTE: This file is authored ahead of an actual CRAN submission.
pagerankr is NOT being submitted to CRAN yet. Before any real submission,
re-run the platform checks below (local, GitHub Actions, and add win-builder
and R-hub runs) and refresh these results so they reflect the exact tarball
being submitted.
-->

## Test environments

- Local: macOS (darwin), R 4.6.0 -- `rcmdcheck::rcmdcheck(args = "--as-cran")`
- GitHub Actions (repository CI): ubuntu-latest, R release

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
- **`Remotes` field** -- the package Imports 'rurl' (>= 2.1.0), which is not
  yet on CRAN, so `DESCRIPTION` currently declares a `Remotes:` entry pointing
  at the GitHub source. CRAN does not honor the `Remotes` field, so this must
  be resolved before a real CRAN submission (i.e. 'rurl' >= 2.1.0 must be on
  CRAN and the `Remotes` field removed). This is tracked separately.

## Downstream dependencies

There are no downstream reverse dependencies; this is a new package.

## Notes

This is a new submission (first release of pagerankr).
