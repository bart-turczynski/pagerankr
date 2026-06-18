fixture_path <- function(name) {
  testthat::test_path("fixtures", "screaming-frog", name)
}

describe("Screaming Frog import contract", {
  it("freezes stable bundle fields and accepted export kinds", {
    contract <- pagerankr:::.sf_contract()

    expect_identical(
      contract$bundle_fields,
      c(
        "nodes", "observations", "edges", "redirects", "canonicals",
        "indexability", "diagnostics", "provenance"
      )
    )
    expect_identical(
      contract$export_kinds,
      c("internal_all", "all_inlinks", "all_outlinks")
    )
    expect_identical(contract$graph_eligible_types, "Hyperlink")
  })

  it("treats Internal: All as node-only and requires a link export for edges", {
    internal <- pagerankr:::.sf_read_input(
      fixture_path("internal-all-bom.csv"),
      "internal_all"
    )

    expect_true("address" %in% names(internal))
    expect_false(any(c("source", "destination") %in% names(internal)))
    expect_error(
      pagerankr:::.sf_read_input(internal, "all_inlinks"),
      "missing required column.*type, source, destination, follow"
    )
  })

  it("keeps Source -> Destination orientation for both link export kinds", {
    path <- fixture_path("all-inlinks-bom.csv")
    inlinks <- pagerankr:::.sf_read_input(path, "all_inlinks")
    outlinks <- pagerankr:::.sf_read_input(path, "all_outlinks")

    expect_identical(inlinks$source, outlinks$source)
    expect_identical(inlinks$destination, outlinks$destination)
    expect_identical(inlinks$source[[1L]], "https://example.com/")
    expect_identical(inlinks$destination[[1L]], "https://example.com/a")
    expect_identical(
      attr(inlinks, "sf_schema")$export_kind,
      "all_inlinks"
    )
    expect_identical(
      attr(outlinks, "sf_schema")$export_kind,
      "all_outlinks"
    )
  })

  it("preserves duplicate observations and filters graph types explicitly", {
    links <- pagerankr:::.sf_read_input(
      fixture_path("all-inlinks-bom.csv"),
      "all_inlinks"
    )
    eligible <- pagerankr:::.sf_graph_eligible(links$type)

    expect_equal(sum(eligible), 3L)
    expect_equal(
      sum(
        links$source == "https://example.com/" &
          links$destination == "https://example.com/a",
        na.rm = TRUE
      ),
      2L
    )
    expect_false(any(eligible[links$type %in% c("Image", "HTML Canonical")]))
  })

  it("parses Follow independently from Rel nofollow diagnostics", {
    links <- pagerankr:::.sf_read_input(
      fixture_path("all-inlinks-bom.csv"),
      "all_inlinks"
    )

    expect_identical(
      pagerankr:::.sf_parse_follow(links$follow)[1:3],
      c(TRUE, FALSE, TRUE)
    )
    expect_identical(
      pagerankr:::.sf_rel_nofollow(links$rel)[1:3],
      c(NA, TRUE, NA)
    )
    expect_identical(
      pagerankr:::.sf_parse_follow(c("yes", "0", "unknown", "")),
      c(TRUE, FALSE, NA, NA)
    )
  })

  it("normalizes placement losslessly and preserves origin/path provenance", {
    links <- pagerankr:::.sf_read_input(
      fixture_path("all-inlinks-bom.csv"),
      "all_inlinks"
    )

    expect_identical(
      pagerankr:::.sf_normalize_position(links$link_position),
      c("nav", "content", NA, NA, NA)
    )
    expect_identical(links$link_position[[3L]], "Head")
    expect_identical(links$link_origin[[1L]], "HTML & Rendered HTML")
    expect_identical(links$link_path[[1L]], "/html/body/a[1]")
  })

  it("handles BOM, blanks, aliases, stable ordering, and ignored extras", {
    internal <- pagerankr:::.sf_read_input(
      fixture_path("internal-all-bom.csv"),
      "internal_all"
    )
    expect_identical(
      names(internal),
      pagerankr:::.sf_contract()$internal$order[
        pagerankr:::.sf_contract()$internal$order %in% names(internal)
      ]
    )
    expect_identical(internal$address[[1L]], "https://example.com/")
    expect_true(is.na(internal$redirect_to[[1L]]))
    expect_true(
      "Ignored Enrichment" %in%
        attr(internal, "sf_schema")$ignored_columns
    )

    aliased <- data.frame(
      URL = "https://example.com/",
      `HTTP Status Code` = "200",
      extra = "ignored",
      check.names = FALSE
    )
    selected <- pagerankr:::.sf_read_input(
      aliased,
      "internal_all",
      fields = c("address", "status_code")
    )
    expect_identical(names(selected), c("address", "status_code"))
    expect_identical(selected$address, "https://example.com/")
    expect_true(
      "extra" %in% attr(selected, "sf_schema")$ignored_columns
    )
  })

  it("selectively reads requested fields with required validation fields", {
    links <- pagerankr:::.sf_read_input(
      fixture_path("all-inlinks-bom.csv"),
      "all_inlinks",
      fields = c("source", "destination", "link_position")
    )

    expect_identical(
      names(links),
      c("source", "destination", "link_position")
    )
    expect_false("anchor" %in% names(links))
    expect_equal(nrow(links), 5L)
  })
})
