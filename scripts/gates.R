#!/usr/bin/env Rscript

# Folded verify-stage gate harness (SEOR-pgammbgo).
#
# WHY THIS EXISTS. `news-version`, `codemeta`, `citation-version` and `lint`
# used to be four separate GitLab CI jobs. Measured 2026-09-23, the first
# three do about 11 seconds of real work between them, but pagerankr pays the
# fleet's own ~2.6-minute runner-pickup tax PER JOB regardless of how little
# that job does -- 9-10 jobs, 7.3 minutes of compute against 31.1 minutes of
# wall time. Folding these four into one `gates` job removes three of those
# four pickups without dropping a single check.
#
# THE SHAPE IS PORTED FROM rurl's tools/verify.R (RURL-mvsxmyww): every gate
# below runs regardless of an earlier one failing, each verdict is captured,
# ONE final summary names every gate that failed, and only THEN does the
# process exit non-zero. A fail-fast harness would silently gut the point of
# folding: a red `news-version` would hide whatever `lint` also found behind a
# job that stopped before running it.
#
# WHAT STAYS SEPARATE, on purpose: `check`, `coverage`, `pages`, `osv-audit`,
# `check-oldrel` and `rurl-floor` are not part of this harness -- different
# failure meanings, and `osv-audit` in particular is a security signal that
# should not lose its own red/green by being buried inside an unrelated
# metadata check. See the `gates:` job's own comment in .gitlab-ci.yml for the
# fold / no-fold reasoning in full.
#
# Each gate below reproduces its former CI job's script VERBATIM -- same
# commands, same comparisons, same exit conditions -- so folding changes
# nothing about what is checked, only how many jobs check it.
#
# Usage: Rscript scripts/gates.R
# Assumes `lintr` and `spelling` are already installed: the CI job's own
# `script:` installs them (and the package's dev deps) before invoking this,
# the same way the old `lint` job did.

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

# 3. citation-version -- ported verbatim from the `citation-version` CI job:
# `--self-test` first (the tree is green by construction, so a passing check
# and a broken script look identical without it), then the real check.
gate_citation_version <- function() {
  self_test <- system2("python3", c("scripts/check-citation.py", "--self-test"),
                        stdout = TRUE, stderr = TRUE)
  self_test_status <- attr(self_test, "status")
  if (!(is.null(self_test_status) || identical(self_test_status, 0L))) {
    return(record("citation-version", FALSE, c("--self-test failed:", self_test)))
  }
  out <- system2("python3", "scripts/check-citation.py", stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  ok <- is.null(status) || identical(status, 0L)
  record("citation-version", ok, if (!ok) out else character())
}

# 4. lint -- ported verbatim from the `lint` CI job: lintr, then spelling
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

results <- list(
  gate_news_version(),
  gate_codemeta(),
  gate_citation_version(),
  gate_lint()
)

failed <- Filter(function(r) !r$ok, results)
cat(sprintf("\n[gates] %d gate(s), %d failed\n", length(results), length(failed)))
if (length(failed)) {
  cat("VERDICT: FAIL --",
      paste(vapply(failed, function(r) r$label, character(1)), collapse = ", "),
      "\n")
  quit(status = 1L)
}
cat("VERDICT: PASS -- news-version, codemeta, citation-version, lint\n")
