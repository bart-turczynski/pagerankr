sf_pr_internal_fixture <- function() {
  data.frame(
    Address = c(
      "https://example.com/",
      "https://example.com/a",
      "https://example.com/b",
      "https://example.com/c",
      "https://example.com/noindex",
      "https://example.com/old",
      "https://example.com/canon"
    ),
    `Status Code` = c("200", "200", "200", "200", "200", "301", "200"),
    Indexability = c(
      "Indexable", "Indexable", "Indexable", "Indexable", "Non-Indexable",
      "Non-Indexable", "Indexable"
    ),
    `Indexability Status` = c("", "", "", "", "noindex", "Redirected", ""),
    `Redirect URL` = c(
      "", "", "", "", "", "https://example.com/b", ""
    ),
    `Canonical Link Element` = c(
      "", "", "", "", "", "", "https://example.com/c"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

sf_pr_links_fixture <- function() {
  data.frame(
    Type = c(
      "Hyperlink", "Hyperlink", "Image", "HTML Canonical",
      "Hyperlink", "Hyperlink", "Hyperlink"
    ),
    Source = c(
      "https://example.com/",
      "https://example.com/",
      "https://example.com/",
      "https://example.com/canon",
      "https://example.com/noindex",
      "https://example.com/old",
      "https://example.com/a"
    ),
    Destination = c(
      "https://example.com/a",
      "https://example.com/canon",
      "https://example.com/logo.png",
      "https://example.com/c",
      "https://example.com/b",
      "https://example.com/c",
      "https://example.com/b"
    ),
    Follow = c("TRUE", "FALSE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE"),
    Rel = c("", "nofollow", "", "canonical", "", "", ""),
    `Link Position` = c(
      "Navigation", "Footer", "Head", "Head", "Content", "Content",
      "Navigation"
    ),
    `Link Origin` = c(
      "HTML", "Rendered HTML", "HTML", "HTML", "HTML & Rendered HTML",
      "Rendered HTML", "HTML"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

sf_pr_bundle_fixture <- function() {
  screaming_frog_bundle(
    sf_pr_internal_fixture(),
    sf_pr_links_fixture(),
    "all_outlinks"
  )
}

describe("pagerank_screaming_frog()", {
  it("matches manual pagerank() over normalized bundle component tables", {
    bundle <- sf_pr_bundle_fixture()

    wrapped <- pagerank_screaming_frog(
      bundle,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      self_loops = "drop",
      nofollow_action = "drop",
      prior_verbose = FALSE
    )
    manual <- pagerank(
      bundle$edges,
      redirects_df = bundle$redirects,
      canonicals_df = bundle$canonicals,
      indexability_df = bundle$indexability,
      edge_from_col = "from",
      edge_to_col = "to",
      redirect_from_col = "from",
      redirect_to_col = "to",
      canonical_from_col = "from",
      canonical_to_col = "to",
      indexability_url_col = "url",
      indexability_status_col = "indexability_status",
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      self_loops = "drop",
      nofollow_action = "drop",
      prior_verbose = FALSE
    )

    transition <- attr(wrapped, "transition_audit")
    import <- attr(wrapped, "screaming_frog_import")
    attr(wrapped, "transition_audit") <- NULL
    attr(wrapped, "screaming_frog_import") <- NULL
    attr(manual, "transition_audit") <- NULL
    expect_equal(wrapped, manual)
    expect_s3_class(transition, "transition_audit")
    expect_s3_class(import, "screaming_frog_import_audit")
  })

  it("scores only bundle graph edges and carries import diagnostics", {
    bundle <- sf_pr_bundle_fixture()
    result <- pagerank_screaming_frog(
      bundle,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      nofollow_action = "drop",
      prior_verbose = FALSE
    )

    transition <- attr(result, "transition_audit")
    import <- attr(result, "screaming_frog_import")

    expect_identical(transition$counts$n_input_rows, nrow(bundle$edges))
    expect_lt(nrow(bundle$edges), nrow(bundle$observations))
    expect_identical(import$diagnostics$links$excluded_type_rows, 2L)
    expect_identical(import$scoring$input_edges, nrow(bundle$edges))
    expect_identical(import$scoring$nofollow_col, "nofollow")
    expect_equal(import$diagnostics$links$follow_rel_disagreements, 0L)
  })

  it("requires explicit placement and origin filters", {
    bundle <- sf_pr_bundle_fixture()
    result <- pagerank_screaming_frog(
      bundle,
      accepted_placements = c("nav", "content"),
      link_origins = c("html", "html_rendered"),
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      nofollow_action = "drop",
      prior_verbose = FALSE
    )

    import <- attr(result, "screaming_frog_import")
    expect_identical(import$scoring$accepted_placements, c("nav", "content"))
    expect_identical(import$scoring$link_origins, c("html", "html_rendered"))
    expect_identical(import$scoring$scored_edge_rows, 3L)
  })

  it("supports opt-in placement weighting", {
    bundle <- sf_pr_bundle_fixture()
    result <- pagerank_screaming_frog(
      bundle,
      placement_weights = c(nav = 3, content = 2, footer = 0.5),
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      nofollow_action = "keep",
      prior_verbose = FALSE
    )

    transition <- attr(result, "transition_audit")
    import <- attr(result, "screaming_frog_import")
    expect_true(transition$coverage$weighted)
    expect_identical(
      transition$coverage$weight_col,
      ".__sf_placement_weight__"
    )
    expect_identical(import$scoring$placement_weights[["nav"]], 3)
    expect_identical(import$scoring$weight_col, NULL)
  })

  it("rejects node-only bundles clearly", {
    bundle <- sf_pr_bundle_fixture()
    bundle$edges <- bundle$edges[0, , drop = FALSE]

    expect_error(
      pagerank_screaming_frog(bundle),
      "node-only bundle"
    )
  })

  it("rejects ambiguous reserved pagerank arguments", {
    bundle <- sf_pr_bundle_fixture()

    expect_error(
      pagerank_screaming_frog(bundle, edge_from_col = "source"),
      "reserved pagerank argument"
    )
    expect_error(
      pagerank_screaming_frog(bundle, placement_weights = c(nav = 2),
        weight_col = "weight"
      ),
      "cannot be combined"
    )
  })
})
