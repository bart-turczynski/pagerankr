<!--
NOTE: This file is authored ahead of an actual CRAN submission.
Before any real submission, confirm the platform results below still describe
the exact tarball being submitted. No blocker is open: the 'rurl' Windows
binary that held the previous attempt is now published, and both win-builder
queues are clean -- see "Windows and the 'rurl' binary" below.
-->

## Test environments

- Local: macOS (darwin), R 4.6.0 -- `rcmdcheck::rcmdcheck(args = "--as-cran")`
- GitLab CI: Ubuntu (rocker/r-ver:4.6), R 4.6.1 -- lint, spelling and
  `R CMD check`, on every merge request and every push to `main`
- GitLab CI: Ubuntu (rocker/r-ver:4.5), R 4.5.x -- `R CMD check`, on `v*` tags
  and on demand (`check-oldrel`)
- win-builder R-devel -- R Under development (unstable) (2026-09-16 r90549
  ucrt), x86_64-w64-mingw32, Windows Server 2022 x64 (build 20348).
  Checked 2026-09-17 17:11:03 UTC. **1 NOTE**, the incoming feasibility note
  below, and nothing else. Package dependencies `OK`, tests `[56s] OK`,
  vignettes re-built `[13s] OK`, PDF manual `[24s] OK`, HTML manual `OK`.
  <https://win-builder.r-project.org/oyB8jH4otNFE>
- win-builder R-release -- R 4.6.1 (2026-06-24 ucrt), same platform.
  Checked 2026-09-17 16:40:15 UTC. **1 NOTE**, the same one, and nothing else.
  Package dependencies `OK` -- the ERROR that stopped the previous R-release
  attempt is gone. Tests `[58s] OK`, vignettes re-built `[13s] OK`, PDF manual
  `[24s] OK`, HTML manual `[22s] OK`.
  <https://win-builder.r-project.org/3KuPXi20VE1Q>

**Both rows above describe one tarball.** It was built with `R CMD build` from
a clean `git archive` export of `main` at `36b3815` and uploaded to both queues
over FTP, response 226 each, one run and one email per queue:

    artifact  pagerankr_0.1.0.tar.gz
    size      588696 bytes
    sha256    017b761fb79b27adccdad76227addd1d14e2c3b1676027bf1d237e2bd0f952c3

That is the tarball intended for submission. `R CMD build` writes a `Packaged:`
timestamp, so it is not bit-reproducible: a rebuild from the same commit gives
a different hash and would not be the artifact these two runs describe.

One later R-release upload was made from such a rebuild, in error, and returned
the same 1 NOTE (<https://win-builder.r-project.org/F9sI3P5JGEV6>). It is
recorded here for completeness and is not cited above; the rebuilt tarball is
not the submission artifact.

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

An automated URL check reports `BugReports:`
(`https://gitlab.com/bart-turczynski/pagerankr/-/issues`) as **404**. This is a
GitLab.com behavior, not a broken link: GitLab has migrated issues to work
items and serves 404 on the legacy `/-/issues` path to any client that is not
signed in, on every project. The same request against
`https://gitlab.com/gitlab-org/gitlab/-/issues` -- one of the most public
trackers on the site -- returns 404 identically. A browser follows the redirect
to `/-/work_items`, which is why the page loads normally by hand.

Measured 2026-09-17 from one anonymous client, one user agent, a single run
(unchanged from the same measurement on 2026-09-10):

    gitlab.com/gitlab-org/gitlab/-/issues              404
    gitlab.com/gitlab-org/gitlab/-/work_items          200
    gitlab.com/bart-turczynski/pagerankr/-/issues      404
    gitlab.com/bart-turczynski/pagerankr/-/work_items  200
    gitlab.com/bart-turczynski/pagerankr               200

The same anonymous scripted client that is refused `/-/issues` is served
`/-/work_items`, so the anonymous REST API is not the only scripted path that
answers. The address is correct and is the one users need; it is not dropped.

**The field names `/-/issues` deliberately, and will keep naming it.** R's own
incoming check requires it. `tools:::.check_package_CRAN_incoming()` validates
a `github.com` or `gitlab.com` `BugReports` against `/issues(/new)?/?$` and
NOTEs anything else, recommending `/issues` in its place:

    if ((endsWith(tolower(z$authority), "github.com") ||
         endsWith(tolower(z$authority), "gitlab.com")) &&
        !grepl("/issues(/new)?/?$", z$path)) {

So the two paths trade one NOTE for another, and they are not equivalent in
cost. Pointing the field at `/-/work_items` satisfies the URL fetch but fails
this check -- the NOTE that archived our sibling package 'pslr' 1.2.1 at the
CRAN pretest. The 404 on `/-/issues` is the form CRAN already accepts in
practice: 'rurl' 3.0.1 is published with the same arrangement. An earlier
revision of this package did name `/-/work_items`; it was moved back for this
reason, and this file previously stated an intention to move it forward again,
which is withdrawn.

## Windows and the 'rurl' binary -- resolved

A previous win-builder **R-release** run stopped before it checked anything:

    * checking package dependencies ... ERROR
    Package required and available but unsuitable version: 'rurl'

'rurl' 3.0.1 had been published to CRAN as source on 2026-09-09, but no Windows
binary had been built yet, so the R-release queue installed 'rurl' 1.2.0 -- the
only Windows binary that existed -- and `Imports: rurl (>= 3.0.1)` correctly
refused it. That was never a defect in pagerankr, and the same tarball was
clean on win-builder R-devel throughout.

**This has cleared.** Measured 2026-09-17:

    https://cran.r-project.org/src/contrib/PACKAGES               rurl 3.0.1
    https://cran.r-project.org/bin/windows/contrib/4.5/PACKAGES   rurl 3.0.1
    https://cran.r-project.org/bin/windows/contrib/4.6/PACKAGES   rurl 3.0.1
    https://cran.r-project.org/bin/windows/contrib/4.7/PACKAGES   rurl 3.0.1

The R-release run was repeated against the current tarball and is clean:
`checking package dependencies ... OK`, the full check ran in 204s, and the
result is **1 NOTE** -- the incoming feasibility note above, identical to
R-devel's. See the R-release row under "Test environments".

The declared floor stays at `rurl (>= 3.0.1)` and was never a candidate for
lowering. 'rurl' 1.2.0's `get_clean_url()` takes 11 arguments and lacks 10 of
the 20 `canonical_profile()` pins, `url_standard` among them, which is where
node identity lives. 3.0.1 is the lowest 'rurl' that exists as an installable
release and that this suite has been run against.

## Downstream dependencies

There are no downstream reverse dependencies; this is a new package.

## Notes

This is a new submission (first release of pagerankr).
