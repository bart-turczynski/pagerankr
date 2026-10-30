# pagerankr's CRAN release deltas

Follow the fleet checklist,
[seor `design/release-checklist.md`](https://gitlab.com/bart-turczynski/seor/-/blob/main/design/release-checklist.md).
pagerankr's deltas:

- **Step 1: the rurl floor is a CRAN release.** The `x-rurl-floor` job (see
  "The rurl floor job" in [`ci.md`](ci.md)) installs exactly the floor `Imports:` declares
  from CRAN and runs the suite. Confirm it passed in the release commit's
  `RELEASE=1` pipeline (step 6). Before win-builder's R-release queue, also
  check that CRAN serves Windows binaries of that rurl version: 0.1.0's
  R-release run waited for them (PAGE-xylymvme).
- **Step 6: the release checks run in T3.** Start it on the release commit
  from **Build > Pipelines > Run pipeline**, ref `main`, variable
  `RELEASE=1` (agent+go): the release, oldrel, devel and floor legs,
  `t2-locale`, `t2-ascran`, `coverage`, the audits, `x-rurl-floor` and the
  PDF manual (`t3-manual`). `glab ci run` starts an API pipeline, which
  creates nothing.
