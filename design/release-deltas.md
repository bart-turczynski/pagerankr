# pagerankr's CRAN release deltas

Follow the fleet checklist,
[seor `design/release-checklist.md`](https://gitlab.com/bart-turczynski/seor/-/blob/main/design/release-checklist.md).
pagerankr's deltas:

- **Step 1: the rurl floor is a CRAN release.** The `rurl-floor` job (see
  "The rurl floor job" in [`ci.md`](ci.md)) installs exactly the floor `Imports:` declares
  from CRAN and runs the suite. Confirm it passed on the release commit's
  `main` pipeline. Before win-builder's R-release queue, also check that CRAN
  serves Windows binaries of that rurl version: 0.1.0's R-release run waited
  for them (PAGE-xylymvme).
- **Step 6: `full-check` and `floor-check` run by themselves only on the
  tag and the weekly schedule.** To see them before submission, run
  `glab ci run --branch main --variables DEEP_CHECK:1` on the release commit
  (agent+go).
