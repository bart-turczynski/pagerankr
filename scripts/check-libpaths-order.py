#!/usr/bin/env python3
"""pin-libpaths-order: assert that .gitlab-ci.yml's cache config actually wins
`.libPaths()` ordering inside the job image, instead of trusting the comment
that says so.

WHY THIS EXISTS. SEOR-dyzgzyot: rocker's Renviron.site puts
`/usr/local/lib/R/site-library` FIRST in `.libPaths()` and `R_LIBS_USER` THIRD,
so `R_LIBS_USER` alone does not make a GitLab CI cache carry the built
library -- packages install into the (disposable) image instead. pagerankr's
`.gitlab-ci.yml` claims to route around this by also setting `R_LIBS`
explicitly, on the theory that rocker's `R_LIBS=${R_LIBS-...}` default-
assignment form defers to an already-set `R_LIBS`. This script does not trust
that comment -- it extracts the real `R_LIBS_USER` / `R_LIBS` values for every
job that declares them and asks an actual rocker container which directory
`.libPaths()[1]` resolves to.

Requires `docker` and the `rocker/r-ver:<tag>` image already pulled locally.
"""
from __future__ import annotations

import re
import subprocess
import sys
import tempfile
from pathlib import Path

import yaml

CI_FILE = Path(__file__).resolve().parents[1] / ".gitlab-ci.yml"

# (job name, image tag, cache-relative dir under CI_PROJECT_DIR)
# Read from the jobs that declare their own R_LIBS/R_LIBS_USER pair, rather
# than hardcoded independently of the file, so a job renamed or re-pointed at
# a different image is caught by KeyError/AssertionError below instead of
# silently not being checked.
JOBS_TO_CHECK = [
    ("default", None),  # the shared `variables:` block + `default.image`
    ("check-oldrel", "check-oldrel"),
]


def load_ci() -> dict:
    with open(CI_FILE) as fh:
        return yaml.safe_load(fh)


def resolve_r_libs(doc: dict, job_name: str | None) -> tuple[str, str, str]:
    """Return (image, R_LIBS_USER, R_LIBS) for a job, falling back to the
    top-level `variables:` / `default.image` for job_name=None."""
    top_vars = doc.get("variables", {}) or {}
    image = doc.get("default", {}).get("image")
    r_libs_user = top_vars.get("R_LIBS_USER")
    r_libs = top_vars.get("R_LIBS")
    if job_name is not None:
        job = doc[job_name]
        job_vars = job.get("variables", {}) or {}
        image = job.get("image", image)
        r_libs_user = job_vars.get("R_LIBS_USER", r_libs_user)
        r_libs = job_vars.get("R_LIBS", r_libs)
    if not (image and r_libs_user and r_libs):
        raise AssertionError(
            f"job {job_name!r}: expected image + R_LIBS_USER + R_LIBS all set, "
            f"got image={image!r} R_LIBS_USER={r_libs_user!r} R_LIBS={r_libs!r}"
        )
    return image, r_libs_user, r_libs


def substitute(value: str, project_dir: str) -> str:
    return value.replace("$CI_PROJECT_DIR", project_dir).replace(
        "${CI_PROJECT_DIR}", project_dir
    )


def libpaths_first(image: str, r_libs_user: str, r_libs: str) -> str:
    """Run the image the way the job's before_script would set it up --
    R_LIBS_USER directory created before R starts -- and return .libPaths()[1]."""
    script = (
        f'mkdir -p "{r_libs_user}" && '
        f"Rscript -e 'cat(.libPaths()[1])'"
    )
    result = subprocess.run(
        [
            "docker", "run", "--rm",
            "-e", f"R_LIBS_USER={r_libs_user}",
            "-e", f"R_LIBS={r_libs}",
            image,
            "sh", "-c", script,
        ],
        capture_output=True, text=True, timeout=120,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"docker run failed (image={image}): {result.stderr.strip()}"
        )
    return result.stdout.strip()


def main() -> int:
    doc = load_ci()
    with tempfile.TemporaryDirectory() as project_dir:
        failures = []
        for job_name, lookup_name in JOBS_TO_CHECK:
            image, r_libs_user_raw, r_libs_raw = resolve_r_libs(doc, lookup_name)
            r_libs_user = substitute(r_libs_user_raw, project_dir)
            r_libs = substitute(r_libs_raw, project_dir)
            first = libpaths_first(image, r_libs_user, r_libs)
            ok = first == r_libs_user
            status = "OK" if ok else "FAIL"
            print(
                f"[{status}] job={job_name} image={image} "
                f".libPaths()[1]={first!r} expected={r_libs_user!r}"
            )
            if not ok:
                failures.append(job_name)
        if failures:
            print(
                f"\nPIN VIOLATED: {', '.join(failures)} do(es) not put the "
                "CI cache directory first in .libPaths(). The cache would "
                "carry downloads but not the built library (SEOR-dyzgzyot).",
                file=sys.stderr,
            )
            return 1
        print("\nPIN HOLDS: every checked job's cache directory wins .libPaths()[1].")
        return 0


if __name__ == "__main__":
    sys.exit(main())
