#!/usr/bin/env python3
"""check-pkgdown-md-filter: pin which top-level *.md files survive the
`pages` job's filter, by actually RUNNING the filter step extracted from
`.gitlab-ci.yml`, not by re-describing it.

WHY THIS EXISTS. SEOR-wqxhftpv: `pkgdown:::package_mds()` hardcodes its own
skip list (README/NEWS/LICENSE) and ignores `_pkgdown.yml`, so every OTHER
top-level `.md` file gets rendered and published unless the `pages` job
removes it first. A filter that lists private families by name (globs like
`AGENTS*.md`) is fail-OPEN: a new family (`GEMINI.md`, `CODEX.md`, ...) is
published by default until someone notices and extends the glob -- which is
exactly how the leak reached four repos after being fixed in one
(SEOR-pibdjanz). This script pins the actual survivor set so that gap is
caught here, in CI, instead of on the published site.

It does not re-implement or guess at the filter's shell logic: it extracts
the real script fragment from the `pages` job (the one step whose command
mentions "AGENTS", identifying it by content rather than by position so a
reordering of the job's steps does not silently stop testing anything) and
executes it for real in a scratch directory, so a change to the filter's
*shape* (rm vs. mv, glob vs. keep-list) is exercised exactly as CI would run
it.

Usage:
  check-pkgdown-md-filter.py               # assert against the real repo tree
  check-pkgdown-md-filter.py --inject X.md  # also test with an extra file
"""
from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).resolve().parents[1]
CI_FILE = REPO_ROOT / ".gitlab-ci.yml"

# Golden snapshot of what the CURRENT `pages` job filter produces. This is a
# pin, not an aspiration: it is updated deliberately, in the same commit that
# changes the filter, to describe the NEW behavior -- see SEOR-wqxhftpv's
# commit pinning the keep-list shape for the post-fix value.
EXPECTED_SURVIVORS = {
    "ACKNOWLEDGMENTS.md",
    "CODE_OF_CONDUCT.md",
    "CONTRIBUTING.md",
    "LICENSE.md",
    "NEWS.md",
    "README.md",
    "SECURITY.md",
    "cran-comments.md",
}


def load_pages_filter_script() -> str:
    with open(CI_FILE) as fh:
        doc = yaml.safe_load(fh)
    steps = doc["pages"]["script"]
    hits = [s for s in steps if "AGENTS" in s]
    if len(hits) != 1:
        raise AssertionError(
            f"expected exactly one `pages.script` step mentioning AGENTS.md "
            f"(the md filter step), found {len(hits)}"
        )
    return hits[0]


def real_top_level_md_files() -> set[str]:
    return {p.name for p in REPO_ROOT.glob("*.md")}


def run_filter(script: str, files: set[str]) -> set[str]:
    """Populate a scratch dir with empty stand-ins for `files`, run `script`
    in it exactly as the job would (sh -c), and return whichever *.md files
    remain in the scratch dir afterward."""
    agent_md_dir = Path("/tmp/agent-md")
    shutil.rmtree(agent_md_dir, ignore_errors=True)
    with tempfile.TemporaryDirectory() as scratch:
        scratch_path = Path(scratch)
        for name in files:
            (scratch_path / name).write_text("")
        result = subprocess.run(
            ["sh", "-c", script], cwd=scratch, capture_output=True, text=True,
        )
        if result.returncode != 0:
            raise RuntimeError(
                f"filter script exited {result.returncode}\n"
                f"stdout: {result.stdout}\nstderr: {result.stderr}"
            )
        survivors = {p.name for p in scratch_path.glob("*.md")}
    shutil.rmtree(agent_md_dir, ignore_errors=True)
    return survivors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--inject", action="append", default=[],
        help="extra filename to add to the scenario (simulates a new, "
             "not-yet-seen top-level .md family, e.g. GEMINI.md)",
    )
    args = parser.parse_args()

    script = load_pages_filter_script()
    scenario_files = real_top_level_md_files() | set(args.inject)
    survivors = run_filter(script, scenario_files)

    expected = EXPECTED_SURVIVORS & scenario_files
    # An injected file is, by construction, never part of the golden
    # snapshot: if it survives the filter, that is exactly the fail-open
    # condition this pin exists to catch.
    print(f"scenario files : {sorted(scenario_files)}")
    print(f"survivors      : {sorted(survivors)}")
    print(f"expected       : {sorted(expected)}")

    unexpected = survivors - expected
    missing = expected - survivors
    if unexpected or missing:
        if unexpected:
            print(
                f"\nPIN VIOLATED: unexpected survivor(s) {sorted(unexpected)} -- "
                "a file that should not be public would be published.",
                file=sys.stderr,
            )
        if missing:
            print(
                f"\nPIN VIOLATED: expected survivor(s) {sorted(missing)} did "
                "not survive -- a public doc would silently drop off the site.",
                file=sys.stderr,
            )
        return 1
    print("\nPIN HOLDS: survivor set matches the golden snapshot.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
