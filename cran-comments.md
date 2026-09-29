<!--
Drafted 2026-09-29 for the 0.1.1 submission (PAGE-mxsbbsaq), ahead of the
owner's release-prep commit. PENDING items must be filled from the exact
submission tarball (fleet checklist steps 5 and 6) before submitting.
-->

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
3.0.1 and 3.1.0. The incoming check's "Days since last update: 1" refers to
this.

## Test environments

- Local: macOS aarch64, R 4.6.0, `R CMD check --as-cran` with
  `_R_CHECK_CRAN_INCOMING_=true` and `_R_CHECK_CRAN_INCOMING_REMOTE_=true`, on a
  tarball built from a clean `git archive` export, against a library holding
  only CRAN packages ('rurl' 3.0.1). PENDING: rerun on the exact submission
  tarball.
- The same tarball's tests against 'rurl' 3.1.0 (built from its release
  candidate): `Status: OK`.
- GitLab CI: Ubuntu (rocker/r-ver:4.6), R 4.6.x, on every push to `main`.
- PENDING (step 6): win-builder R-devel, R-release and R-oldrelease; macOS
  builder; R-hub.

## R CMD check results

Candidate at `main` 91a45ff (version still 0.1.0.9000), measured 2026-09-29:
0 errors | 0 warnings | 2 notes. PENDING: the submission tarball.

* checking CRAN incoming feasibility ... NOTE

      Days since last update: 1

      Found the following (possibly) invalid URLs:
        URL: https://gitlab.com/bart-turczynski/pagerankr/-/issues
          From: DESCRIPTION
                man/pagerankr-package.Rd
          Status: 404

  The candidate also reported "Version contains large components
  (0.1.0.9000)"; the release version removes it.

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
