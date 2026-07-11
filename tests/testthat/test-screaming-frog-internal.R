sf_internal_fixture <- function() {
  data.frame(
    URL = c(
      "https://example.com/",
      "https://example.com/image.png",
      "https://example.com/old",
      "https://example.com/gone",
      "https://example.com/private",
      "https://example.com/blocked",
      "https://example.com/canonical",
      "https://example.com/",
      "",
      "https://example.com/bad-status",
      "https://example.com/no-target",
      "https://example.com/non-redirect-target"
    ),
    `Content-Type` = c(
      "text/html", "image/png", "text/html", "text/html", "text/html",
      "text/html", "text/html", "text/html", "text/html", "text/html",
      "text/html", "text/html"
    ),
    `HTTP Status Code` = c(
      "200", "200", "301", "404", "200", "200", "200", "200", "200",
      "abc", "302", "200"
    ),
    Indexability = c(
      "Indexable", "Non-Indexable", "Non-Indexable", "Non-Indexable",
      "Non-Indexable", "Non-Indexable", "Indexable", "Indexable",
      "Indexable", "Indexable", "Non-Indexable", "Indexable"
    ),
    `Indexability Status` = c(
      "", "Non-HTML", "Redirected", "Client Error", "noindex",
      "Blocked by robots.txt", "", "", "", "", "Redirected", ""
    ),
    `Canonical Link Element` = c(
      "https://example.com/", "", "", "", "", "",
      "https://example.com/", "", "", "", "", ""
    ),
    `Redirect URI` = c(
      "", "", "https://example.com/new", "", "", "", "", "", "", "",
      "", "https://example.com/not-used"
    ),
    `Redirect Type` = c(
      "", "", "Permanent", "", "", "", "", "", "", "", "Temporary", ""
    ),
    `Crawl Allowed` = c(
      "Allowed", "Allowed", "Allowed", "Allowed", "Allowed", "Not Allowed",
      "Allowed", "Allowed", "Allowed", "Allowed", "Allowed", "maybe"
    ),
    `Indexing Allowed` = c(
      "Allowed", "Allowed", "Allowed", "Allowed", "Not Allowed", "Allowed",
      "Allowed", "Allowed", "Allowed", "Allowed", "Allowed", "Allowed"
    ),
    `Meta Robots 1` = c(
      "", "", "", "", "noindex", "", "", "", "", "", "", ""
    ),
    `X-Robots-Tag 1` = c(
      "", "", "", "", "", "noindex", "", "", "", "", "", ""
    ),
    Language = rep("en", 12),
    `Crawl Timestamp` = rep("2026-06-18 10:00:00", 12),
    `Size (bytes)` = rep("1,024", 12),
    `Word Count` = rep("100", 12),
    Inlinks = rep("3", 12),
    Outlinks = rep("4", 12),
    `Response Time` = rep("0.125", 12),
    `Ignored Enrichment` = rep("extra", 12),
    check.names = FALSE
  )
}

describe("screaming_frog_internal()", {
  it("normalizes node facts while preserving raw URLs and input order", {
    result <- screaming_frog_internal(sf_internal_fixture())

    expect_s3_class(result, "screaming_frog_internal")
    expect_named(
      result,
      c(
        "nodes", "redirects", "canonicals", "indexability",
        "diagnostics", "provenance"
      )
    )
    expect_equal(nrow(result$nodes), 11L)
    expect_identical(result$nodes$url[1:3], c(
      "https://example.com/",
      "https://example.com/image.png",
      "https://example.com/old"
    ))
    expect_identical(result$nodes$http_status[1:4], c(200L, 200L, 301L, 404L))
    expect_identical(result$nodes$content_type[[2L]], "image/png")
    expect_identical(result$nodes$size_bytes[[1L]], 1024)
    expect_identical(result$nodes$word_count[[1L]], 100L)
    expect_identical(result$nodes$response_time_seconds[[1L]], 0.125)
  })

  it("derives only valid 3xx redirects and independent canonicals", {
    result <- screaming_frog_internal(sf_internal_fixture())

    expect_identical(result$redirects$from, "https://example.com/old")
    expect_identical(result$redirects$to, "https://example.com/new")
    expect_identical(result$redirects$status_code, 301L)
    expect_identical(
      result$canonicals,
      data.frame(
        from = c(
          "https://example.com/",
          "https://example.com/canonical"
        ),
        to = c(
          "https://example.com/",
          "https://example.com/"
        )
      )
    )
    expect_equal(result$diagnostics$self_canonical_rows, 1L)
    expect_equal(result$diagnostics$missing_3xx_destinations, 1L)
    expect_equal(result$diagnostics$ignored_non_3xx_destinations, 1L)
  })

  it("retains indexability and robots facts without inventing policy", {
    result <- screaming_frog_internal(sf_internal_fixture())
    private <- result$indexability$url == "https://example.com/private"
    blocked <- result$indexability$url == "https://example.com/blocked"

    expect_identical(
      result$indexability$indexability_status[private],
      "noindex"
    )
    expect_false(result$indexability$indexing_allowed[private])
    expect_identical(
      result$indexability$indexability_status[blocked],
      "Blocked by robots.txt"
    )
    expect_false(result$indexability$crawl_allowed[blocked])
    expect_identical(result$indexability$x_robots_tag[blocked], "noindex")
  })

  it("reports missing, duplicate, invalid, and ignored input facts", {
    result <- screaming_frog_internal(sf_internal_fixture())
    diagnostics <- result$diagnostics

    expect_equal(diagnostics$input_rows, 12L)
    expect_equal(diagnostics$dropped_missing_address, 1L)
    expect_equal(diagnostics$invalid_status_codes, 1L)
    expect_equal(diagnostics$duplicate_address_rows, 2L)
    expect_identical(
      diagnostics$duplicate_addresses,
      "https://example.com/"
    )
    expect_true("Ignored Enrichment" %in% diagnostics$ignored_columns)
    expect_true("unique_inlinks" %in% diagnostics$missing_optional_columns)
    expect_true("unique_outlinks" %in% diagnostics$missing_optional_columns)
    expect_true(any(
      diagnostics$issues$issue == "invalid_allowed_value" &
        diagnostics$issues$field == "crawl_allowed"
    ))
  })

  it("handles BOM files, aliases, extra columns, and absent optional fields", {
    result <- screaming_frog_internal(
      testthat::test_path(
        "fixtures", "screaming-frog", "internal-all-bom.csv"
      )
    )

    expect_equal(nrow(result$nodes), 3L)
    expect_identical(result$nodes$url[[1L]], "https://example.com/")
    expect_identical(result$redirects$to[[1L]], "https://example.com/new")
    expect_true("Ignored Enrichment" %in% result$diagnostics$ignored_columns)
    expect_identical(
      unname(result$provenance$detected_columns[["address"]]),
      "Address"
    )
    expect_true(is.na(result$nodes$language[[1L]]))
  })

  it("fails clearly when required structural columns are absent", {
    expect_error(
      screaming_frog_internal(data.frame(Address = "https://example.com/")),
      "missing required column.*status_code"
    )
  })
})
