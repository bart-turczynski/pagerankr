#!/usr/bin/env Rscript

# Folded verify-stage gate harness (SEOR-pgammbgo).
#
# WHY THIS EXISTS. `news-version`, `codemeta` and `lint` used to be three
# separate GitLab CI jobs. Measured 2026-09-23, `news-version` and `codemeta`
# do a few seconds of real work between them, but pagerankr pays the fleet's
# own ~2.6-minute runner-pickup tax PER JOB regardless of how little that job
# does -- 9-10 jobs, 7.3 minutes of compute against 31.1 minutes of wall
# time. Folding these three into one `gates` job removes two of those three
# pickups without dropping a single check.
#
# `citation-version` (CITATION.cff / .zenodo.json vs DESCRIPTION) is
# DELIBERATELY NOT folded in here. It runs `scripts/check-citation.py`, which
# needs python3, and this job runs on the R image (`rocker/r-ver:4.6`), which
# has none -- verified directly with `docker run --rm rocker/r-ver:4.6 sh -c
# 'which python3'`. Folding it in without a python3 install fails every run
# with exit 127; adding an apt-get for it would add a new network/package
# dependency to a job whose other two members touch no package manager at
# all. citation-version stays its own job on `python:3.13-alpine`, where
# python3 already exists at near-zero cost. See the `gates:` job's own
# comment in .gitlab-ci.yml for the full reasoning and the two options
# weighed.
#
# THE SHAPE IS PORTED FROM rurl's tools/verify.R (RURL-mvsxmyww): every gate
# below runs regardless of an earlier one failing, each verdict is captured,
# ONE final summary names every gate that failed, and only THEN does the
# process exit non-zero. A fail-fast harness would silently gut the point of
# folding: a red `news-version` would hide whatever `lint` also found behind a
# job that stopped before running it.
#
# WHAT STAYS SEPARATE, on purpose: `check`, `coverage`, `pages`,
# `citation-version`, `osv-audit`, `check-oldrel` and `rurl-floor` are not
# part of this harness -- different failure meanings (or a missing
# interpreter, for citation-version), and `osv-audit` in particular is a
# security signal that should not lose its own red/green by being buried
# inside an unrelated metadata check. See the `gates:` job's own comment in
# .gitlab-ci.yml for the fold / no-fold reasoning in full.
#
# Each gate below reproduces its former CI job's script VERBATIM -- same
# commands, same comparisons, same exit conditions -- so folding changes
# nothing about what is checked, only how many jobs check it.
#
# Usage: Rscript scripts/gates.R [gate ...]
#
# With no arguments every gate runs -- that is what the CI job does. Named
# gates run just that subset, which is how `.githooks/pre-push` reuses this
# file: it calls `gates.R news-version codemeta` for the two base-R gates and
# runs lint itself, because the hook keeps SKIP_SPELLING as a separate opt-in
# that this file's combined `lint` gate has no way to express (SEOR-dzrisdmi).
#
# Selecting a subset does NOT make it fail-fast: whatever is selected still
# runs to completion and reports once, below.
#
# `lint` assumes `lintr` and `spelling` are already installed: the CI job's own
# `script:` installs them (and the package's dev deps) before invoking this,
# the same way the old `lint` job did. `news-version` and `codemeta` need
# nothing but base R, which is why the hook can afford them unconditionally.

results <- list()

record <- function(label, ok, detail = character()) {
  cat(sprintf("[gates] %-16s %s\n", label, if (ok) "PASS" else "FAIL"))
  if (!ok && length(detail)) {
    cat(paste0("        | ", detail, collapse = "\n"), "\n", sep = "")
  }
  list(label = label, ok = ok)
}

read_version <- function() {
  d <- read.dcf("DESCRIPTION")
  as.character(d[1L, "Version"])
}

# 1. news-version -- ported verbatim from the `news-version` CI job: the top
# NEWS.md heading must be "(development version)" or exactly the DESCRIPTION
# Version.
gate_news_version <- function() {
  version <- read_version()
  lines <- readLines("NEWS.md", warn = FALSE)
  heading_line <- grep("^# ", lines, value = TRUE)[1L]
  heading <- sub("^#\\s+pagerankr\\s*", "", heading_line)
  ok <- identical(heading, "(development version)") || identical(heading, version)
  detail <- sprintf(
    paste0(
      "top NEWS.md heading ('%s') is neither '(development version)' nor ",
      "the DESCRIPTION Version ('%s'). Update NEWS.md before release."
    ),
    heading, version
  )
  record("news-version", ok, if (!ok) detail else character())
}

# 2. codemeta -- ported verbatim from the `codemeta` CI job: exactly one
# top-level `"version":` line, matching DESCRIPTION, and no mangled
# `host::repo` remote spec inside a URL (the codemetar 0.3.7 damage class,
# PAGE-bpwkoofa).
gate_codemeta <- function() {
  version <- read_version()
  lines <- readLines("codemeta.json", warn = FALSE)
  hits <- grep('^  "version":', lines, value = TRUE)
  if (length(hits) != 1L) {
    return(record("codemeta", FALSE, sprintf(
      paste0(
        "expected exactly one top-level '\"version\":' line in codemeta.json, ",
        "found %d. The file's shape changed; this check needs updating."
      ),
      length(hits)
    )))
  }
  declared <- sub('.*"version":\\s*"([^"]*)".*', "\\1", hits)
  if (!identical(version, declared)) {
    return(record("codemeta", FALSE, sprintf(
      paste0(
        "codemeta.json is stale (version '%s'; DESCRIPTION says '%s'). ",
        "Hand-edit its version -- do NOT run codemetar::write_codemeta(), ",
        "which drops readme/releaseNotes/keywords."
      ),
      declared, version
    )))
  }
  mangled <- grep('https?://[^"]*::', lines, value = TRUE)
  if (length(mangled)) {
    return(record("codemeta", FALSE, c(
      "codemeta.json carries a remote spec inside a URL:", mangled
    )))
  }
  record("codemeta", TRUE)
}

# 3. lint -- ported verbatim from the `lint` CI job: lintr, then spelling
# against DESCRIPTION `Language: en-US`.
gate_lint <- function() {
  l <- lintr::lint_package()
  if (length(l)) {
    return(record("lint", FALSE, utils::capture.output(print(l))))
  }
  bad <- spelling::spell_check_package()
  if (nrow(bad)) {
    return(record("lint", FALSE, c(
      utils::capture.output(print(bad)),
      "Correct the prose, or add genuine terms to inst/WORDLIST."
    )))
  }
  record("lint", TRUE)
}

available <- list(
  "news-version" = gate_news_version,
  "codemeta" = gate_codemeta,
  "lint" = gate_lint
)

args <- commandArgs(trailingOnly = TRUE)

# `--no-summary` suppresses the count and VERDICT lines, keeping the per-gate
# PASS/FAIL lines and the exit status. .githooks/pre-push passes it: the hook
# drives this file one gate at a time and prints its OWN verdict list across all
# seven of its gates, so an inner "VERDICT: PASS -- codemeta" in the middle of a
# failing push is noise that contradicts the real summary.
summarize <- !("--no-summary" %in% args)
selected <- setdiff(args, "--no-summary")
if (!length(selected)) {
  selected <- names(available)
}

unknown <- setdiff(selected, names(available))
if (length(unknown)) {
  cat(
    "[gates] unknown gate(s):", paste(unknown, collapse = ", "), "\n",
    "[gates] available:", paste(names(available), collapse = ", "), "\n",
    sep = " "
  )
  quit(status = 2L)
}

# Run ALL selected gates before reporting any verdict. A failure here must not
# stop the next gate -- see THE SHAPE IS PORTED FROM rurl's tools/verify.R
# above; the whole point is that one run names every failure.
results <- lapply(selected, function(name) available[[name]]())

failed <- Filter(function(r) !r$ok, results)
if (summarize) {
  cat(sprintf("\n[gates] %d gate(s), %d failed\n", length(results), length(failed)))
}
if (length(failed)) {
  if (summarize) {
    cat("VERDICT: FAIL --",
        paste(vapply(failed, function(r) r$label, character(1)), collapse = ", "),
        "\n")
  }
  quit(status = 1L)
}
if (summarize) {
  cat("VERDICT: PASS --", paste(selected, collapse = ", "), "\n")
}
