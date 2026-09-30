## Submission

This is an update. CRAN serves pagerankr 0.1.0, accepted 2026-09-28; this is
0.1.1.

**Why so soon after 0.1.0.** This is a compatibility release that 'rurl', one
of pagerankr's imports, needs before its own next version can be submitted.
'rurl' 3.1.0 (same maintainer) adds an argument alias to
`rurl::get_clean_url()`. pagerankr 0.1.0's test suite deliberately fails
whenever that function's argument list changes: a guard that makes sure a new
argument is reviewed for whether it can change pagerankr's node keys. With
0.1.0 on CRAN, 'rurl' 3.1.0 would therefore break a reverse dependency. 0.1.1
reviews the alias (it cannot change the key) and passes against both 'rurl'
3.0.1 and 3.1.0. The incoming check's "Days since last update" refers to
this.

## Test environments

All runs below checked the same source: `main` f9d6d71, the release commit.

- Local: macOS Tahoe 26.7, aarch64-apple-darwin23, R 4.6.0.
  `R CMD check --as-cran` with `_R_CHECK_CRAN_INCOMING_=true` and
  `_R_CHECK_CRAN_INCOMING_REMOTE_=true`, on the submission tarball built from
  a clean `git archive` export, against a library holding only CRAN packages
  ('rurl' 3.0.1): 0 errors | 0 warnings | 2 notes (below).
- The same tarball against 'rurl' 3.1.0, built from its release candidate
  (rurl commit 4b48dbf), with `R CMD check`: `Status: OK`.
- win-builder, R-oldrelease (R 4.5.3, Windows Server 2022): 1 note, the
  incoming note below.
- win-builder, R-release (R 4.6.1) and R-devel (2026-09-29 r90598): not
  completed. Both stopped at "checking CRAN incoming feasibility" twice, with
  nothing after that line run. This is a builder-side stop that other packages
  saw the same week. The incoming checks are covered by the local run above,
  and Windows by R-hub's Windows run below.
- macOS builder (mac.r-project.org), R 4.6.1, macOS 26.6 arm64: `Status: OK`.
- R-hub (R Consortium runners), R-devel (2026-09-29), `Status: OK` on each:
  - linux: Ubuntu 24.04.5 LTS, x86_64-pc-linux-gnu
  - macos: macOS Sequoia 15.7.9, x86_64-apple-darwin20
  - macos-arm64: macOS Tahoe 26.6.2, aarch64-apple-darwin23
  - windows: Windows Server 2022, x86_64-w64-mingw32
- GitLab CI on the release commit: Ubuntu 24.04, R 4.6.1, `Status: OK`; the
  same suite under R 4.5.3; and the suite against the declared 'rurl' floor,
  3.0.1, installed from CRAN.

## R CMD check results

0 errors | 0 warnings | 2 notes (local, on the submission tarball).

* checking CRAN incoming feasibility ... NOTE

      Days since last update: 2

      Found the following (possibly) invalid URLs:
        URL: https://gitlab.com/bart-turczynski/pagerankr/-/issues
          From: DESCRIPTION
                man/pagerankr-package.Rd
          Status: 404

  win-builder's R-oldrelease run of the same note also lists "PageRank",
  "SEO" and "pipeable" as possibly misspelled words in `DESCRIPTION`. They
  are the algorithm's name, a standard abbreviation, and a term for functions
  that compose with the pipe; 0.1.0 was accepted with the same words.

* checking HTML version of manual ... NOTE: "Skipping checking math
  rendering: package 'V8' unavailable". Local only; the check library lacks
  'V8'.

**Days since last update** is explained under Submission above.

**The `BugReports` 404** is not a broken link. GitLab has migrated issues to
work items and serves 404 on the legacy `/-/issues` path to any client that is
not signed in, on every project (`gitlab.com/gitlab-org/gitlab/-/issues` does
the same), while a browser is redirected to `/-/work_items`. The field keeps
`/-/issues` because R's incoming check requires that form for a gitlab.com
tracker (`tools:::.check_package_CRAN_incoming()` tests the path against
`/issues(/new)?/?$`). 0.1.0 was accepted with the same arrangement, and the
0.1.0 note records the full measurement.

## Downstream dependencies

There are no reverse dependencies on CRAN.
