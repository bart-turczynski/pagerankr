# Independent re-check of the shipped case-study fixture.
#
# data-raw/build-fixture.R runs a scrub gate before writing, but that gate lives
# with the code that produced the output. These tests re-derive the guarantee
# from the shipped files alone.
#
# The check is a POSITIVE assertion — every host and every path segment must
# match the synthetic vocabulary — rather than a blocklist of forbidden
# strings. A blocklist would have to name the very tokens the fixture exists to
# remove, putting them back into the repository, and it would only catch leaks
# somebody thought to enumerate.

host_pattern <- paste0(
  "^(reviews-microsite|external-[0-9]{2})",
  "\\.example\\.(net|com|org)$"
)
segment_pattern <- "^(s[0-9]+|p[0-9]+)(\\.[A-Za-z0-9]{1,5})?$"
label_pattern <- "(home|s[0-9]+|p[0-9]+)(\\.[A-Za-z0-9]{1,5})?$"

fixture_dir <- function(phase) {
  d <- system.file(
    "extdata", paste0("reviews-microsite-", phase),
    package = "pagerankr"
  )
  if (!nzchar(d)) testthat::skip("fixture not installed")
  d
}

read_links <- function(phase) {
  utils::read.csv(
    file.path(fixture_dir(phase), "all_inlinks.csv"),
    colClasses = "character"
  )
}

read_fixture_urls <- function(phase) {
  internal <- utils::read.csv(
    file.path(fixture_dir(phase), "internal_all.csv"),
    colClasses = "character"
  )
  links <- read_links(phase)
  urls <- c(
    links$Source, links$Destination, internal$Address,
    internal$Canonical.Link.Element.1, internal$Redirect.URL
  )
  unique(urls[nzchar(urls)])
}

fixture_hosts <- function(phase) {
  unique(sub("^[a-z]+://([^/]+).*$", "\\1", read_fixture_urls(phase)))
}

fixture_segments <- function(phase) {
  paths <- sub(
    "^[a-z]+://[^/]+", "",
    sub("[?#].*$", "", read_fixture_urls(phase))
  )
  segs <- unlist(strsplit(paths, "/", fixed = TRUE))
  unique(segs[nzchar(segs)])
}

test_that("every fixture host is a reserved example domain", {
  for (phase in c("before", "after")) {
    hosts <- fixture_hosts(phase)
    bad <- hosts[!grepl(host_pattern, hosts)]
    expect_identical(bad, character(0), info = toString(bad))
  }
})

test_that("no real path segment survives in fixture URLs", {
  # Synthetic segments are s<n> at depth 1 and p<nn> deeper, optionally
  # carrying a file extension. Anything else means a source segment leaked.
  for (phase in c("before", "after")) {
    segs <- fixture_segments(phase)
    bad <- segs[!grepl(segment_pattern, segs)]
    expect_identical(bad, character(0), info = toString(bad))
  }
})

test_that("anchor and alt text carry no free text from the source", {
  for (phase in c("before", "after")) {
    links <- read_links(phase)
    anchors <- unique(links$Anchor[nzchar(links$Anchor)])
    alts <- unique(links$Alt.Text[nzchar(links$Alt.Text)])
    expect_true(all(grepl(paste0("^link to ", label_pattern), anchors)))
    expect_true(all(grepl(paste0("^image of ", label_pattern), alts)))
  }
})

test_that("the two crawls share one URL dictionary", {
  # The before/after comparison is only meaningful if a page appearing in both
  # crawls carries the SAME synthetic URL in both. A per-crawl dictionary would
  # relabel independently and silently destroy the pairing while leaving every
  # file well-formed, so assert the overlap directly.
  shared <- intersect(read_fixture_urls("before"), read_fixture_urls("after"))
  expect_gt(length(shared), 50L)
})

test_that("both fixture crawls parse and rank", {
  for (phase in c("before", "after")) {
    d <- fixture_dir(phase)
    bundle <- screaming_frog_bundle(
      internal = file.path(d, "internal_all.csv"),
      links    = file.path(d, "all_inlinks.csv")
    )
    expect_gt(nrow(bundle$nodes), 50L)
    expect_gt(nrow(bundle$edges), 1000L)

    pr <- pagerank_screaming_frog(bundle)
    expect_gt(nrow(pr), 50L)
    expect_true(all(is.finite(pr$pagerank)))
    expect_true(all(pr$pagerank > 0))
    # Scores do not sum to 1: mass leaks to sinks and is accounted for
    # separately in wasted_mass. The mass model has its own tests; all this
    # needs to know is that the fixture ranks non-degenerately.
    expect_lte(sum(pr$pagerank), 1)
    expect_gt(sum(pr$pagerank), 0.5)
  }
})
