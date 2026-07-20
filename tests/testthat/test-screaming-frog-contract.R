fixture_path <- function(name) {
  testthat::test_path("fixtures", "screaming-frog", name)
}

describe("Screaming Frog import contract", {
  it("freezes stable bundle fields and accepted export kinds", {
    contract <- pagerankr::sf_contract()

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
    internal <- pagerankr::sf_read_input(
      fixture_path("internal-all-bom.csv"),
      "internal_all"
    )

    expect_true("address" %in% names(internal))
    expect_false(any(c("source", "destination") %in% names(internal)))
    expect_error(
      pagerankr::sf_read_input(internal, "all_inlinks"),
      "missing required column.*type, source, destination, follow"
    )
  })

  it("keeps Source -> Destination orientation for both link export kinds", {
    path <- fixture_path("all-inlinks-bom.csv")
    inlinks <- pagerankr::sf_read_input(path, "all_inlinks")
    outlinks <- pagerankr::sf_read_input(path, "all_outlinks")

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
    links <- pagerankr::sf_read_input(
      fixture_path("all-inlinks-bom.csv"),
      "all_inlinks"
    )
    eligible <- pagerankr::sf_graph_eligible(links$type)

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
    links <- pagerankr::sf_read_input(
      fixture_path("all-inlinks-bom.csv"),
      "all_inlinks"
    )

    expect_identical(
      pagerankr::sf_parse_follow(links$follow)[1:3],
      c(TRUE, FALSE, TRUE)
    )
    expect_identical(
      pagerankr::sf_rel_nofollow(links$rel)[1:3],
      c(NA, TRUE, NA)
    )
    expect_identical(
      pagerankr::sf_parse_follow(c("yes", "0", "unknown", "")),
      c(TRUE, FALSE, NA, NA)
    )
  })

  it("normalizes placement losslessly and preserves origin/path provenance", {
    links <- pagerankr::sf_read_input(
      fixture_path("all-inlinks-bom.csv"),
      "all_inlinks"
    )

    expect_identical(
      pagerankr::sf_normalize_position(links$link_position),
      c("nav", "content", NA, NA, NA)
    )
    expect_identical(links$link_position[[3L]], "Head")
    expect_identical(links$link_origin[[1L]], "HTML & Rendered HTML")
    expect_identical(links$link_path[[1L]], "/html/body/a[1]")
  })

  it("derives the region from the DOM path, outermost container first", {
    expect_identical(
      pagerankr::sf_region_from_path(c(
        "//body/footer/nav/ul/li[1]/a",
        "//body/header/nav/ul/li[2]/a",
        "//body/nav/ul/li[1]/a",
        "//body/main/aside/nav/a",
        "//body/main/article/p[5]/a[1]"
      )),
      c("footer", "header", "nav", "aside", "content")
    )
  })

  it("matches element names, not class predicates", {
    # `div[@class='site-footer']` is a div. Reading the class would make the
    # region depend on a site's naming conventions rather than its markup.
    expect_identical(
      pagerankr::sf_region_from_path(c(
        "//body/div[@class='site-footer']/a",
        "//body/div[@class='header']/nav/a"
      )),
      c("content", "nav")
    )
  })

  it("returns NA for a missing path so the caller can fall back", {
    expect_identical(
      pagerankr::sf_region_from_path(c("", NA, "   ")),
      c(NA_character_, NA_character_, NA_character_)
    )
  })

  it("prefers the path over Link Position, which loses nested regions", {
    # The motivating case: a site whose footer is `footer > nav > a`. Screaming
    # Frog reports every such link as Navigation and emits no Footer bucket at
    # all, so `footer` is unreachable from Link Position alone.
    links <- data.frame(
      Type = c("Hyperlink", "Hyperlink"),
      Source = c("https://example.com/", "https://example.com/"),
      Destination = c("https://example.com/a", "https://example.com/b"),
      Follow = c("TRUE", "TRUE"),
      `Link Position` = c("Navigation", "Navigation"),
      `Link Path` = c("//body/footer/nav/ul/li[1]/a", ""),
      check.names = FALSE
    )
    imported <- pagerankr::screaming_frog_links(links, "all_outlinks")

    expect_identical(imported$edges$placement, c("footer", "nav"))
    # The second row had no path and fell back to Link Position.
    expect_identical(
      imported$diagnostics$placement_from_position_rows,
      1L
    )
  })

  it("handles BOM, blanks, aliases, stable ordering, and ignored extras", {
    internal <- pagerankr::sf_read_input(
      fixture_path("internal-all-bom.csv"),
      "internal_all"
    )
    expect_named(
      internal,
      pagerankr::sf_contract()$internal$order[
        pagerankr::sf_contract()$internal$order %in% names(internal)
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
    selected <- pagerankr::sf_read_input(
      aliased,
      "internal_all",
      fields = c("address", "status_code")
    )
    expect_named(selected, c("address", "status_code"))
    expect_identical(selected$address, "https://example.com/")
    expect_true(
      "extra" %in% attr(selected, "sf_schema")$ignored_columns
    )
  })

  it("selectively reads requested fields with required validation fields", {
    links <- pagerankr::sf_read_input(
      fixture_path("all-inlinks-bom.csv"),
      "all_inlinks",
      fields = c("source", "destination", "link_position")
    )

    expect_named(
      links,
      c("source", "destination", "link_position")
    )
    expect_false("anchor" %in% names(links))
    expect_equal(nrow(links), 5L)
  })
})

describe("sf_container_from_path()", {
  it("strips numeric predicates so unstable positions agree", {
    # The same recycled component lands at a different paragraph index
    # depending on how much prose precedes it. Both must be one container.
    expect_equal(
      sf_container_from_path("//body/main/article/p[5]/a[1]"),
      sf_container_from_path("//body/main/article/p[3]/a[1]")
    )
  })

  it("keeps class predicates as the component identity", {
    # Deliberately the OPPOSITE of sf_region_from_path(), which strips classes
    # so a div[@class='site-footer'] is not read as a <footer>. Different
    # questions: "which region is this" vs "is this the same component".
    expect_equal(
      sf_container_from_path("//body/div[@class='cta']/a"),
      "//body/div[@class='cta']"
    )
    expect_false(identical(
      sf_container_from_path("//body/div[@class='cta']/a"),
      sf_container_from_path("//body/div[@class='hero']/a")
    ))
  })

  it("drops the trailing anchor step whatever predicate it carries", {
    # The anchor's own class describes the link, not its container.
    expect_equal(
      sf_container_from_path("//body/div[@class='cta']/a[@class='btn']"),
      "//body/div[@class='cta']"
    )
    expect_equal(
      sf_container_from_path("//body/div[@class='cta']/a[@class='btn']"),
      sf_container_from_path("//body/div[@class='cta']/a")
    )
  })

  it("returns NA for blank and missing paths", {
    # No path means no component identity; the detector leaves the row
    # unscored rather than inventing one.
    expect_true(is.na(sf_container_from_path(NA_character_)))
    expect_true(is.na(sf_container_from_path("")))
    expect_true(is.na(sf_container_from_path("   ")))
  })

  it("is vectorized and length-preserving", {
    paths <- c("//body/main/p[1]/a", NA, "//body/nav/ul/li[2]/a")
    expect_length(sf_container_from_path(paths), 3L)
  })
})

describe("container vs region on the same path", {
  it("disagree about class predicates, on purpose", {
    path <- "//body/div[@class='site-footer']/a"
    # The region parser must not see a <footer> here...
    expect_equal(sf_region_from_path(path), "content")
    # ...but the container parser must keep the class as the identity.
    expect_equal(
      sf_container_from_path(path),
      "//body/div[@class='site-footer']"
    )
  })
})
