#!/usr/bin/env Rscript
# check-codemeta v1
#
# codemeta.json has exactly one top-level `"version":` line, and it names the
# DESCRIPTION Version; no URL in it carries a mangled `host::repo` remote spec
# (the codemetar 0.3.7 damage class, PAGE-bpwkoofa).
#
#   Rscript scripts/check-codemeta.R      # from the package root
#
# A cheap gate of the fleet-ci local gate (.fleet-ci.yml), run in the full
# tier. It was the `codemeta` gate of the former scripts/gates.R, which the
# `gates` CI job and the old pre-push hook ran; the rule is unchanged. Base R
# only. Exit 1, saying what is wrong, on any failure.

fail <- function(...) {
  cat(..., "\n", sep = "")
  quit(status = 1L)
}

version <- read.dcf("DESCRIPTION", fields = "Version")[[1L]]
lines <- readLines("codemeta.json", warn = FALSE)
hits <- grep('^  "version":', lines, value = TRUE)
if (length(hits) != 1L) {
  fail(
    "Expected exactly one top-level '\"version\":' line in codemeta.json, ",
    "found ", length(hits), ". The file's shape changed; this check needs ",
    "updating."
  )
}
declared <- sub('.*"version":\\s*"([^"]*)".*', "\\1", hits)
if (!identical(version, declared)) {
  fail(
    "codemeta.json is stale (version '", declared, "'; DESCRIPTION says '",
    version, "'). Hand-edit its version: do NOT run ",
    "codemetar::write_codemeta(), which drops readme/releaseNotes/keywords."
  )
}
mangled <- grep('https?://[^"]*::', lines, value = TRUE)
if (length(mangled)) {
  fail(
    "codemeta.json carries a remote spec inside a URL:\n",
    paste(mangled, collapse = "\n")
  )
}
cat("codemeta: version '", declared, "' matches DESCRIPTION\n", sep = "")
