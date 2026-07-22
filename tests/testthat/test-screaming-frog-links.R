sf_links_fixture <- function() {
  data.frame(
    `Link Type` = c(
      "Hyperlink", "Hyperlink", "Image", "HTML Canonical", "Hyperlink",
      "Hyperlink", "Hyperlink", "Hyperlink", "Hyperlink"
    ),
    From = c(
      "https://example.com/", "https://example.com/",
      "https://example.com/", "https://example.com/a",
      "https://example.com/a", "", "https://example.com/b",
      "https://example.com/c", "https://example.com/c"
    ),
    `Source Segments` = c("0", "0", "0", "1", "1", "0", "1", "1", "1"),
    To = c(
      "https://example.com/a", "https://example.com/a",
      "https://example.com/logo.png", "https://example.com/canonical", "",
      "https://example.com/x", "https://example.com/d",
      "https://example.com/e", "https://example.com/e"
    ),
    `Destination Segments` = c("1", "1", "1", "1", "", "1", "1", "1", "1"),
    `Size Bytes` = c("120", "120", "1,024", "", "", "", "", "", ""),
    Anchor = c("A", "A duplicate", "", "canonical", "", "", "D", "E", "E"),
    `HTTP Status Code` = c(
      "200", "200", "200", "200", "", "200", "200", "abc", "abc"
    ),
    Status = c("OK", "OK", "OK", "OK", "", "OK", "OK", "", ""),
    Crawlability = rep("Crawlable", 9),
    Follow = c(
      "TRUE", "false", "TRUE", "TRUE", "", "TRUE", "maybe", "TRUE", "TRUE"
    ),
    Rel = c(
      "", "nofollow", "", "canonical", "", "", "nofollow", "nofollow",
      "nofollow"
    ),
    `Path Type` = rep("Absolute", 9),
    # Paths agree with Link Position below, except row 5, which has none and so
    # exercises the fallback. Placement is derived from the path first.
    `Link Path` = c(
      "/html/body/nav/a[1]", "/html/body/main/a[2]", "/html/head/link[1]",
      "/html/head/link[2]", "", "/html/body/header/a[4]",
      "/html/body/main/aside/a[5]", "/html/body/footer/a[6]",
      "/html/body/footer/a[6]"
    ),
    `Link Position` = c(
      "Navigation", "Content", "Head", "Head", "", "Header", "Aside",
      "Footer", "Footer"
    ),
    `Link Origin` = c(
      "HTML & Rendered HTML", "Rendered HTML", "HTML", "HTML", "Unknown",
      "HTML", "HTML", "Rendered HTML", "Rendered HTML"
    ),
    `Ignored Extra` = rep("x", 9),
    check.names = FALSE
  )
}

describe("screaming_frog_links()", {
  it("keeps Source -> Destination orientation for both export kinds", {
    inlinks <- screaming_frog_links(sf_links_fixture(), "all_inlinks")
    outlinks <- screaming_frog_links(sf_links_fixture(), "all_outlinks")

    expect_identical(inlinks$observations, outlinks$observations)
    # The two exports describe the identical graph; only the reading-order
    # index differs, because document order lives in All Outlinks alone. All
    # Inlinks row order is destination-alphabetical, so its index stays NA.
    graph_cols <- setdiff(names(outlinks$edges), "position_index")
    expect_identical(inlinks$edges[graph_cols], outlinks$edges[graph_cols])
    expect_true(all(is.na(inlinks$edges$position_index)))
    expect_identical(outlinks$edges$position_index, c(NA, 1L, NA, NA, NA))
    expect_identical(inlinks$edges$from[[1L]], "https://example.com/")
    expect_identical(inlinks$edges$to[[1L]], "https://example.com/a")
    expect_identical(inlinks$provenance$export_kind, "all_inlinks")
    expect_identical(outlinks$provenance$export_kind, "all_outlinks")
  })

  it("preserves observations and emits only valid Hyperlink edges", {
    result <- screaming_frog_links(sf_links_fixture(), "all_outlinks")

    expect_s3_class(result, "screaming_frog_links")
    expect_named(
      result,
      c("observations", "edges", "diagnostics", "provenance")
    )
    expect_equal(nrow(result$observations), 9L)
    expect_equal(nrow(result$edges), 5L)
    expect_identical(result$edges$input_row, c(1L, 2L, 7L, 8L, 9L))
    expect_false(any(result$edges$to == "https://example.com/logo.png"))
    expect_equal(result$diagnostics$excluded_type_rows, 2L)
    expect_equal(result$diagnostics$dropped_invalid_endpoints, 2L)
    expect_identical(
      result$diagnostics$excluded_types,
      c("Image", "HTML Canonical")
    )
  })

  it("retains nofollow, Rel disagreement, placement, and origin", {
    result <- screaming_frog_links(sf_links_fixture(), "all_inlinks")

    expect_identical(
      result$observations$placement,
      c("nav", "content", NA, NA, NA, "header", "aside", "footer", "footer")
    )
    expect_identical(result$edges$nofollow[1:2], c(FALSE, TRUE))
    expect_identical(result$edges$link_origin[1:2], c(
      "HTML & Rendered HTML", "Rendered HTML"
    ))
    expect_identical(result$edges$link_path[[1L]], "/html/body/nav/a[1]")
    expect_equal(result$diagnostics$invalid_follow_values, 1L)
    expect_equal(result$diagnostics$follow_rel_disagreements, 2L)
    expect_true(any(
      result$diagnostics$issues$issue == "follow_rel_disagreement"
    ))
  })

  it("filters edge origins without changing observations", {
    all <- screaming_frog_links(sf_links_fixture(), "all_outlinks")
    html <- screaming_frog_links(
      sf_links_fixture(), "all_outlinks", origin_policy = "html"
    )
    rendered <- screaming_frog_links(
      sf_links_fixture(), "all_outlinks", origin_policy = "rendered"
    )

    expect_equal(nrow(all$observations), nrow(html$observations))
    expect_identical(html$edges$input_row, c(1L, 7L))
    expect_identical(rendered$edges$input_row, c(1L, 2L, 8L, 9L))
    expect_equal(html$diagnostics$excluded_origin_rows, 3L)
    expect_equal(rendered$diagnostics$excluded_origin_rows, 1L)
  })

  it("can fail instead of dropping graph rows with missing endpoints", {
    expect_error(
      screaming_frog_links(
        sf_links_fixture(), "all_outlinks", endpoint_action = "error"
      ),
      "2 graph-eligible row"
    )
  })

  it("reads BOM files selectively and reports schema facts", {
    result <- screaming_frog_links(
      testthat::test_path(
        "fixtures", "screaming-frog", "all-inlinks-bom.csv"
      ),
      "all_inlinks"
    )

    expect_equal(nrow(result$observations), 5L)
    expect_equal(nrow(result$edges), 2L)
    expect_identical(result$edges$from, c(
      "https://example.com/", "https://example.com/"
    ))
    expect_true("Ignored Extra" %in% result$diagnostics$ignored_columns)
    expect_identical(
      unname(result$provenance$detected_columns[["source"]]),
      "Source"
    )
  })

  it("preserves exact duplicate graph observations for later aggregation", {
    result <- screaming_frog_links(sf_links_fixture(), "all_outlinks")

    expect_identical(result$edges$input_row[4:5], c(8L, 9L))
    expect_equal(result$diagnostics$duplicate_observation_rows, 2L)
  })
})
