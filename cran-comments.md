<!--
NOTE: This file is authored ahead of an actual CRAN submission.
pagerankr is NOT being submitted to CRAN yet. Before any real submission,
re-run the platform checks below and refresh these results so they reflect the
exact tarball being submitted. One blocker is open and is NOT in this package:
see "Windows and the 'rurl' binary" below.
-->

## Test environments

- Local: macOS (darwin), R 4.6.0 -- `rcmdcheck::rcmdcheck(args = "--as-cran")`
- GitLab CI: Ubuntu (rocker/r-ver:4.6), R 4.6.1 -- lint, spelling and
  `R CMD check`, on every merge request and every push to `main`
- GitLab CI: Ubuntu (rocker/r-ver:4.5), R 4.5.x -- `R CMD check`, on `v*` tags
  and on demand (`check-oldrel`)
- win-builder R-devel -- R Under development (unstable) (2026-09-09 r90510
  ucrt), x86_64-w64-mingw32, Windows Server 2022 x64 (build 20348).
  Submitted 2026-09-10, checked 12:49:11 UTC. **1 NOTE**, the incoming
  feasibility note below. Tests `[37s] OK`, vignettes re-built OK, PDF manual
  `[15s] OK`. <https://win-builder.r-project.org/8iFbbLUPQsE3>
- win-builder R-release -- R 4.6.1 (2026-06-24 ucrt), same platform. Submitted
  in the same run, checked 12:40:04 UTC. **1 ERROR, 1 NOTE**, and the ERROR is
  an environment defect rather than a package defect -- see below.
  <https://win-builder.r-project.org/6kKLhSl1SxKL>

Both win-builder queues were uploaded to over FTP, response 226 each, from a
tarball built with `R CMD build` from a clean `git archive` export of `main` at
`001ea7e` (`pagerankr_0.1.0.tar.gz`, 588625 bytes). One upload per queue
produced one run and one email each; there was no second submission.

**R-hub has not been run and cannot be ported** -- it works by dispatching
workflows inside a GitHub repository, so there is nothing to translate. Both
GitLab CI jobs are Linux, on a self-hosted runner; macOS has no runner at all.

## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is the CRAN-incoming feasibility note:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Bart Turczynski <bartek@turczynski.pl>'

  New submission

  Possibly misspelled words in DESCRIPTION:
    PageRank (3:28, 7:73, 7:245, 7:323)
    SEO (7:137)
    pipeable (7:41)

  Found the following (possibly) invalid URLs:
    URL: https://gitlab.com/bart-turczynski/pagerankr/-/issues
      From: DESCRIPTION
            man/pagerankr-package.Rd
            README.md
      Status: 404
      Message: Not Found

Explanation:

- **New submission** -- this is expected; pagerankr 0.1.0 is a first release.
- **The three flagged words are spelled correctly and are intended.**
  *PageRank* is the proper name of the Page-Brin algorithm this package
  implements, capitalised as Google and the original literature capitalise it.
  *SEO* is the standard abbreviation for search engine optimisation. *pipeable*
  describes a function designed to be composed with R's `|>` pipe, and is the
  term the tidyverse design guide uses. None is a typographical error and none
  is dropped. This line appears only under CRAN's incoming check, which uses
  aspell; the package's own `inst/WORDLIST` already lists all three, so the
  local and CI `spelling::spell_check_package()` gate is clean and cannot
  surface it.
- **The `BugReports` 404** -- not a broken link; see the section below.

Every hard dependency now resolves from CRAN. 'rurl' reached CRAN as 3.0.1 on
2026-09-09, so the `Remotes:` field -- which CRAN does not honor, and which
previously drew a second line on this NOTE -- has been removed and `Imports:`
declares 'rurl' (>= 3.0.1), the lowest 'rurl' that exists as an installable
release and that the suite has been run against.

## A note on the `BugReports` URL

An automated URL check may report `BugReports:`
(`https://gitlab.com/bart-turczynski/pagerankr/-/issues`) as **404**. This is a
GitLab.com behavior, not a broken link: GitLab has migrated issues to work
items and serves 404 on the legacy `/-/issues` path to any client that is not
signed in, on every project. The same request against
`https://gitlab.com/gitlab-org/gitlab/-/issues` -- one of the most public
trackers on the site -- returns 404 identically. A browser follows the redirect
to `/-/work_items`, which is why the page loads normally by hand.

What is stale is the path, not the project, and this is not a block on scripted
clients. Measured 2026-09-10 from one anonymous client, one user agent, a
single run:

    gitlab.com/gitlab-org/gitlab/-/issues              404
    gitlab.com/gitlab-org/gitlab/-/work_items          200
    gitlab.com/bart-turczynski/pagerankr/-/issues      404
    gitlab.com/bart-turczynski/pagerankr/-/work_items  200
    gitlab.com/bart-turczynski/pagerankr               200

The same anonymous scripted client that is refused `/-/issues` is served
`/-/work_items`, so the anonymous REST API is not the only scripted path that
answers. The address is correct and is the one users need; it is not dropped.
`BugReports:` will name the `work_items` path from the next version, so that
the change goes through a release cycle rather than a submission -- editing
`DESCRIPTION` now would invalidate the tarball every check row above was
measured against.

## Windows and the 'rurl' binary -- the open blocker

The win-builder **R-release** run stopped at:

    * checking package dependencies ... ERROR
    Package required and available but unsuitable version: 'rurl'

It aborted there, before running anything: install 2 s, check 12 s. **This is
not a defect in pagerankr**, and the same tarball is clean on win-builder
R-devel, which ran the full check in 128 s and returned 1 NOTE.

CRAN has published 'rurl' 3.0.1 as source but has not yet built a Windows
binary for it. Measured 2026-09-10:

    https://cran.r-project.org/src/contrib/PACKAGES               rurl 3.0.1
    https://cran.r-project.org/bin/windows/contrib/4.5/PACKAGES   rurl 1.2.0
    https://cran.r-project.org/bin/windows/contrib/4.6/PACKAGES   rurl 1.2.0
    https://cran.r-project.org/bin/windows/contrib/4.7/PACKAGES   rurl 1.2.0

CRAN's own check-results page for 'rurl' shows the same split, with every
flavor `OK` on one of two versions -- 3.0.1 on three Linux flavors and three
macOS flavors, 1.2.0 on all three Windows flavors (page timestamped 2026-09-10
13:51 CEST). Nothing has failed; the Windows binary simply has not been built
yet. 'rurl' 3.0.1 was published 2026-09-09 07:40:02 UTC.

So the R-release queue installed 'rurl' 1.2.0, the only Windows binary that
exists, and `Imports: rurl (>= 3.0.1)` correctly refused it.

**The floor is not negotiable and will not be lowered to clear this.** 'rurl'
1.2.0's `get_clean_url()` takes 11 arguments and lacks 10 of the 20
`canonical_profile()` pins, `url_standard` among them, which is where node
identity lives. 3.0.1 is the lowest 'rurl' that exists as an installable
release and that this suite has been run against.

This submission is therefore held until a Windows binary of 'rurl' 3.0.1
appears in `bin/windows/contrib`, at which point the R-release run is repeated
and this section is replaced by its result.

## Downstream dependencies

There are no downstream reverse dependencies; this is a new package.

## Notes

This is a new submission (first release of pagerankr).
