sf_bundle_internal_fixture <- function() {
  data.frame(
    Address = c(
      "https://example.com/",
      "https://example.com/a",
      "https://example.com/orphan",
      "https://example.com/old",
      "https://example.com/canonical",
      "https://cdn.example.net/asset.png"
    ),
    `Status Code` = c("200", "200", "200", "301", "200", "200"),
    `Content Type` = c(
      "text/html", "text/html", "text/html", "text/html", "text/html",
      "image/png"
    ),
    Indexability = c(
      "Indexable", "Indexable", "Indexable", "Non-Indexable",
      "Indexable", "Non-Indexable"
    ),
    `Indexability Status` = c("", "", "", "Redirected", "", "Non-HTML"),
    `Redirect URL` = c(
      "", "", "", "https://example.com/new", "", ""
    ),
    `Canonical Link Element` = c(
      "", "", "", "", "https://example.com/missing-canonical", ""
    ),
    check.names = FALSE
  )
}

sf_bundle_links_fixture <- function() {
  data.frame(
    Type = c(
      "Hyperlink", "Hyperlink", "Hyperlink", "Hyperlink",
      "Hyperlink", "Image", "Hyperlink"
    ),
    Source = c(
      "https://example.com/", "https://example.com/",
      "https://example.com/a", "https://external.test/page",
      "https://example.com/a", "https://example.com/",
      "not a url"
    ),
    Destination = c(
      "https://example.com/a", "https://external.test/page",
      "https://example.com/missing", "https://example.com/a",
      "", "https://example.com/logo.png", "https://example.com/a"
    ),
    Follow = c("TRUE", "FALSE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE"),
    Rel = c("", "nofollow", "", "", "", "", ""),
    `Link Position` = c(
      "Navigation", "Footer", "Content", "Header", "Aside", "Head",
      "Unknown"
    ),
    `Link Origin` = c(
      "HTML", "Rendered HTML", "HTML & Rendered HTML", "HTML",
      "HTML", "HTML", "HTML"
    ),
    check.names = FALSE
  )
}

describe("screaming_frog_bundle()", {
  it("composes stable top-level tables without aggregating observations", {
    bundle <- screaming_frog_bundle(
      sf_bundle_internal_fixture(),
      sf_bundle_links_fixture(),
      "all_outlinks"
    )

    expect_s3_class(bundle, "screaming_frog_bundle")
    expect_identical(
      names(bundle),
      c(
        "nodes", "observations", "edges", "redirects", "canonicals",
        "indexability", "diagnostics", "provenance"
      )
    )
    expect_equal(nrow(bundle$nodes), 6L)
    expect_equal(nrow(bundle$observations), 7L)
    expect_equal(nrow(bundle$edges), 5L)
    expect_equal(nrow(bundle$redirects), 1L)
    expect_equal(nrow(bundle$canonicals), 1L)
    expect_identical(bundle$edges$from[[1L]], "https://example.com/")
    expect_identical(bundle$edges$to[[1L]], "https://example.com/a")
  })

  it("reports deterministic link losses, counts, and distributions", {
    bundle <- screaming_frog_bundle(
      sf_bundle_internal_fixture(),
      sf_bundle_links_fixture(),
      "all_outlinks"
    )
    diagnostics <- bundle$diagnostics

    expect_equal(diagnostics$counts$observations, 7L)
    expect_equal(diagnostics$links$graph_eligible_rows, 6L)
    expect_equal(diagnostics$links$edge_rows, 5L)
    expect_equal(diagnostics$links$excluded_type_rows, 1L)
    expect_equal(diagnostics$links$dropped_invalid_endpoints, 1L)
    expect_equal(diagnostics$links$nofollow_edges, 1L)
    expect_equal(diagnostics$links$placement_mapped_observations, 5L)
    expect_equal(diagnostics$links$placement_unmapped_observations, 2L)
    expect_identical(
      diagnostics$inputs$observations_by_type,
      data.frame(
        type = c("Hyperlink", "Image"),
        n = c(6L, 1L)
      )
    )
    expect_true(any(
      diagnostics$distributions$hosts$host == "example.com" &
        diagnostics$distributions$hosts$n == 5L
    ))
  })

  it("distinguishes external, internal-host absent, and malformed joins", {
    bundle <- screaming_frog_bundle(
      sf_bundle_internal_fixture(),
      sf_bundle_links_fixture(),
      "all_outlinks"
    )

    absent <- bundle$diagnostics$cross_table$edge_endpoints_absent
    expect_true(any(
      absent$url == "https://external.test/page" &
        absent$classification == "external_endpoint"
    ))
    expect_true(any(
      absent$url == "https://example.com/missing" &
        absent$classification == "internal_host_absent"
    ))
    expect_true(any(
      absent$url == "not a url" &
        absent$classification == "malformed_url"
    ))

    expect_identical(
      bundle$diagnostics$cross_table$nodes_absent_from_graph$url,
      c(
        "https://example.com/orphan",
        "https://example.com/old",
        "https://example.com/canonical",
        "https://cdn.example.net/asset.png"
      )
    )
    expect_identical(
      bundle$diagnostics$cross_table$redirect_targets_absent$classification,
      "internal_host_absent"
    )
    expect_identical(
      bundle$diagnostics$cross_table$canonical_targets_absent$classification,
      "internal_host_absent"
    )
  })

  it("accepts pre-imported components and records stable provenance", {
    internal <- screaming_frog_internal(sf_bundle_internal_fixture())
    links <- screaming_frog_links(sf_bundle_links_fixture(), "all_inlinks")
    bundle <- screaming_frog_bundle(internal, links)

    expect_identical(bundle$provenance$contract_version, 1L)
    expect_identical(
      bundle$provenance$declared_export_kinds$internal,
      "internal_all"
    )
    expect_identical(
      bundle$provenance$declared_export_kinds$links,
      "all_inlinks"
    )
    expect_identical(bundle$provenance$input_rows$internal, 6L)
    expect_identical(bundle$provenance$input_rows$links, 7L)
  })

  it("has concise summary and print methods", {
    bundle <- screaming_frog_bundle(
      sf_bundle_internal_fixture(),
      sf_bundle_links_fixture(),
      "all_outlinks"
    )
    summary <- summary(bundle)

    expect_s3_class(summary, "summary.screaming_frog_bundle")
    expect_identical(summary$nodes, 6L)
    expect_identical(summary$observations, 7L)
    expect_identical(summary$edges, 5L)
    printed <- capture.output(print(bundle))
    expect_true(any(grepl("Screaming Frog Bundle", printed, fixed = TRUE)))
    expect_true(any(grepl("Observations:", printed, fixed = TRUE)))
  })
})
