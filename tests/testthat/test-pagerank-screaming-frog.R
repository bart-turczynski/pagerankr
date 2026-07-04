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
    check.names = FALSE
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
    check.names = FALSE
  )
}

sf_pr_bundle_fixture <- function() {
  screaming_frog_bundle(
    sf_pr_internal_fixture(),
    sf_pr_links_fixture(),
    "all_outlinks"
  )
}

sf_pr_crossdomain_internal_fixture <- function() {
  data.frame(
    Address = c(
      "https://example.com/",
      "https://example.com/a",
      "https://example.com/b"
    ),
    `Status Code` = c("200", "200", "200"),
    Indexability = c("Indexable", "Indexable", "Indexable"),
    `Indexability Status` = c("", "", ""),
    `Redirect URL` = c("", "", ""),
    `Canonical Link Element` = c("", "https://mirror.example.net/a", ""),
    check.names = FALSE
  )
}

sf_pr_crossdomain_links_fixture <- function() {
  data.frame(
    Type = c("Hyperlink", "Hyperlink", "Hyperlink"),
    Source = c(
      "https://example.com/",
      "https://example.com/",
      "https://example.com/a"
    ),
    Destination = c(
      "https://example.com/a",
      "https://example.com/b",
      "https://example.com/b"
    ),
    Follow = c("TRUE", "TRUE", "TRUE"),
    Rel = c("", "", ""),
    `Link Position` = c("Navigation", "Navigation", "Content"),
    `Link Origin` = c("HTML", "HTML", "HTML"),
    check.names = FALSE
  )
}

sf_pr_crossdomain_bundle_fixture <- function() {
  screaming_frog_bundle(
    sf_pr_crossdomain_internal_fixture(),
    sf_pr_crossdomain_links_fixture(),
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
    expect_null(import$scoring$weight_col)
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

  it("still blocks the reserved raw canonicals_df / redirects_df args", {
    bundle <- sf_pr_bundle_fixture()

    expect_error(
      pagerank_screaming_frog(bundle, canonicals_df = bundle$canonicals),
      "reserved pagerank argument"
    )
    expect_error(
      pagerank_screaming_frog(bundle, redirects_df = bundle$redirects),
      "reserved pagerank argument"
    )
  })

  it("apply_canonicals = FALSE keeps the as-crawled node identities", {
    bundle <- sf_pr_crossdomain_bundle_fixture()

    as_crawled <- pagerank_screaming_frog(
      bundle,
      apply_canonicals = FALSE,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      nofollow_action = "drop",
      prior_verbose = FALSE
    )
    manual <- pagerank(
      bundle$edges,
      redirects_df = NULL,
      canonicals_df = NULL,
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
      nofollow_action = "drop",
      prior_verbose = FALSE
    )

    attr(as_crawled, "transition_audit") <- NULL
    attr(as_crawled, "screaming_frog_import") <- NULL
    attr(manual, "transition_audit") <- NULL
    expect_equal(as_crawled, manual)
    expect_true("https://example.com/a" %in% as_crawled[[1]])
    expect_false("https://mirror.example.net/a" %in% as_crawled[[1]])
  })

  it("default folds off-domain canonicals, relabeling crawled pages", {
    bundle <- sf_pr_crossdomain_bundle_fixture()

    relabeled <- pagerank_screaming_frog(
      bundle,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      nofollow_action = "drop",
      prior_verbose = FALSE
    )

    import <- attr(relabeled, "screaming_frog_import")
    expect_true(import$scoring$apply_canonicals)
    expect_true("https://mirror.example.net/a" %in% relabeled[[1]])
    expect_false("https://example.com/a" %in% relabeled[[1]])
  })

  it("apply_redirects = FALSE skips redirect folding", {
    bundle <- sf_pr_bundle_fixture()

    as_crawled <- pagerank_screaming_frog(
      bundle,
      apply_redirects = FALSE,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      nofollow_action = "drop",
      prior_verbose = FALSE
    )
    manual <- pagerank(
      bundle$edges,
      redirects_df = NULL,
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
      nofollow_action = "drop",
      prior_verbose = FALSE
    )

    import <- attr(as_crawled, "screaming_frog_import")
    expect_false(import$scoring$apply_redirects)
    expect_true("https://example.com/old" %in% as_crawled[[1]])
    attr(as_crawled, "transition_audit") <- NULL
    attr(as_crawled, "screaming_frog_import") <- NULL
    attr(manual, "transition_audit") <- NULL
    expect_equal(as_crawled, manual)
  })

  it("reports the off-domain canonical count at import and scoring time", {
    cross <- sf_pr_crossdomain_bundle_fixture()
    expect_identical(cross$diagnostics$counts$canonicals_off_domain, 1L)
    expect_identical(summary(cross)$canonicals_off_domain, 1L)

    cross_result <- pagerank_screaming_frog(
      cross,
      apply_canonicals = FALSE,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      prior_verbose = FALSE
    )
    expect_identical(
      attr(cross_result, "screaming_frog_import")$scoring$canonicals_off_domain,
      1L
    )

    self <- sf_pr_bundle_fixture()
    expect_identical(self$diagnostics$counts$canonicals_off_domain, 0L)
    self_result <- pagerank_screaming_frog(
      self,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      clean_canonical_urls = FALSE,
      prior_verbose = FALSE
    )
    expect_identical(
      attr(self_result, "screaming_frog_import")$scoring$canonicals_off_domain,
      0L
    )
  })

  it("rejects non-scalar-logical fold flags", {
    bundle <- sf_pr_bundle_fixture()

    expect_error(
      pagerank_screaming_frog(bundle, apply_canonicals = "yes"),
      "single `TRUE` or `FALSE`"
    )
    expect_error(
      pagerank_screaming_frog(bundle, apply_redirects = c(TRUE, FALSE)),
      "single `TRUE` or `FALSE`"
    )
  })
})
