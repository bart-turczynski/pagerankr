context("filter_links_by_domain")

describe("filter_links_by_domain basic functionality", {
  it("keeps all rows when no filters are specified", {
    links <- data.frame(
      from = c("http://a.com/1", "http://b.com/2"),
      to = c("http://b.com/3", "http://a.com/4"),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links)
    expect_equal(nrow(result), 2)
  })

  it("filters by keep_domains", {
    links <- data.frame(
      from = c(
        "http://www.example.com/a", "http://example.com/b",
        "http://google.com/c"
      ),
      to = c(
        "http://example.com/d", "http://help.example.com/e",
        "http://www.example.com/f"
      ),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links, keep_domains = "example.com")
    # Row 1: example.com -> example.com (keep)
    # Row 2: example.com -> example.com (keep)
    # Row 3: google.com -> example.com (google.com dropped by third-party)
    expect_equal(nrow(result), 2)
  })

  it("filters by keep_hosts (exact match)", {
    links <- data.frame(
      from = c("http://www.example.com/a", "http://blog.example.com/b"),
      to = c("http://www.example.com/c", "http://www.example.com/d"),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links, keep_hosts = "www.example.com")
    # Row 1: www.example.com -> www.example.com (keep)
    # Row 2: blog.example.com -> www.example.com (blog dropped as third-party)
    expect_equal(nrow(result), 1)
  })

  it("ignore overrides keep", {
    links <- data.frame(
      from = c("http://www.example.com/a", "http://cdn.example.com/b"),
      to = c("http://example.com/c", "http://example.com/d"),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links,
      keep_domains = "example.com",
      ignore_hosts = "cdn.example.com"
    )
    # Row 1: www.example.com -> example.com (keep)
    # Row 2: cdn.example.com -> example.com
    #        (cdn ignored even though domain matches)
    expect_equal(nrow(result), 1)
  })

  it("ignore_domains works without keep lists", {
    links <- data.frame(
      from = c("http://good.com/a", "http://bad.com/b", "http://good.com/c"),
      to = c("http://good.com/d", "http://good.com/e", "http://bad.com/f"),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links, ignore_domains = "bad.com")
    # Row 1: good -> good (keep)
    # Row 2: bad -> good (drop, from is ignored)
    # Row 3: good -> bad (drop, to is ignored)
    expect_equal(nrow(result), 1)
  })

  it("drop_third_party = FALSE keeps non-listed URLs", {
    links <- data.frame(
      from = c("http://example.com/a", "http://other.com/b"),
      to = c("http://example.com/c", "http://example.com/d"),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links,
      keep_domains = "example.com",
      drop_third_party = FALSE
    )
    # Both rows kept: other.com is not in keep list but third-party not dropped
    expect_equal(nrow(result), 2)
  })

  it("both endpoints must pass for row to survive", {
    links <- data.frame(
      from = c("http://example.com/a", "http://external.com/b"),
      to = c("http://external.com/c", "http://example.com/d"),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links, keep_domains = "example.com")
    # Row 1: example -> external (external dropped)
    # Row 2: external -> example (external dropped)
    expect_equal(nrow(result), 0)
  })
})

describe("filter_links_by_domain report and edge cases", {
  it("returns a report when requested", {
    links <- data.frame(
      from = c("http://a.com/1", "http://b.com/2"),
      to = c("http://a.com/3", "http://a.com/4"),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links,
      keep_domains = "a.com",
      return_report = TRUE
    )
    expect_true(is.list(result))
    expect_true("filtered_df" %in% names(result))
    expect_true("report" %in% names(result))
    expect_equal(result$report$rows_before, 2)
    expect_equal(result$report$rows_after, 1)
    expect_equal(result$report$rows_dropped, 1)
  })

  it("handles empty data frame", {
    empty <- data.frame(
      from = character(0), to = character(0),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(empty, keep_domains = "example.com")
    expect_equal(nrow(result), 0)
  })

  it("preserves extra columns", {
    links <- data.frame(
      from = c("http://a.com/1", "http://a.com/2"),
      to = c("http://a.com/3", "http://a.com/4"),
      weight = c(5, 10),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links, keep_domains = "a.com")
    expect_true("weight" %in% names(result))
  })

  it("handles schemeless URLs", {
    links <- data.frame(
      from = c("www.example.com/a", "other.com/b"),
      to = c("example.com/c", "example.com/d"),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links, keep_domains = "example.com")
    # www.example.com and example.com both have registrable domain example.com
    expect_equal(nrow(result), 1)
  })

  it("resolves bare (scheme-less) keep_domains and keep_hosts via rurl", {
    # Regression: rurl's default protocol_handling = "keep" prepends a scheme
    # to scheme-less filter values, so .ensure_scheme() is no longer needed.
    links <- data.frame(
      from = c("http://www.example.com/a", "http://other.com/b"),
      to = c("http://www.example.com/c", "http://other.com/d"),
      stringsAsFactors = FALSE
    )
    # Bare registrable domain (no scheme) keeps example.com, drops other.com
    by_domain <- filter_links_by_domain(links, keep_domains = "example.com")
    expect_equal(nrow(by_domain), 1)
    # Bare host with subdomain (no scheme) resolves to the exact host
    by_host <- filter_links_by_domain(links, keep_hosts = "www.example.com")
    expect_equal(nrow(by_host), 1)
  })

  it("psl_section selects the registrable-domain PSL section", {
    # github.io is a PRIVATE suffix: under "all" the registrable domain of
    # user.github.io is user.github.io, but under "icann" (suffix "io") it is
    # github.io. So a keep on user.github.io scopes to that one subdomain under
    # "all" but folds all *.github.io together under "icann".
    links <- data.frame(
      from = "https://user.github.io/a",
      to = "https://other.github.io/b",
      stringsAsFactors = FALSE
    )
    all_res <- filter_links_by_domain(
      links,
      keep_domains = "user.github.io", psl_section = "all"
    )
    expect_equal(nrow(all_res), 0)
    icann_res <- filter_links_by_domain(
      links,
      keep_domains = "user.github.io", psl_section = "icann"
    )
    expect_equal(nrow(icann_res), 1)
  })

  it("rejects an invalid psl_section", {
    links <- data.frame(from = "http://a.com", to = "http://b.com")
    expect_error(
      filter_links_by_domain(links, keep_domains = "a.com",
                             psl_section = "bogus")
    )
  })

  it("errors on invalid inputs", {
    expect_error(filter_links_by_domain(list()), "data frame")
    expect_error(
      filter_links_by_domain(data.frame(x = 1), from_col = "from"),
      "columns"
    )
    expect_error(
      filter_links_by_domain(data.frame(from = "a", to = "b"),
        keep_domains = 123
      ),
      "character"
    )
  })

  it("errors on invalid keep_hosts type", {
    df <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
    expect_error(filter_links_by_domain(df, keep_hosts = 42), "character")
  })

  it("errors on invalid ignore_domains type", {
    df <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
    expect_error(filter_links_by_domain(df, ignore_domains = TRUE), "character")
  })

  it("errors on invalid ignore_hosts type", {
    df <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
    expect_error(
      filter_links_by_domain(df, ignore_hosts = list("x")),
      "character"
    )
  })

  it("returns report for empty df with return_report=TRUE", {
    empty <- data.frame(
      from = character(0), to = character(0),
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(empty,
      keep_domains = "x.com",
      return_report = TRUE
    )
    expect_true(is.list(result))
    expect_equal(result$report$rows_before, 0)
  })

  it("returns report when no filters are active", {
    links <- data.frame(
      from = "http://a.com/1", to = "http://b.com/2",
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links, return_report = TRUE)
    expect_equal(result$report$rows_before, 1)
    expect_equal(result$report$rows_after, 1)
    expect_equal(result$report$rows_dropped, 0)
  })

  it("handles domains that resolve to empty after trim/NA removal", {
    links <- data.frame(
      from = "http://a.com/1", to = "http://a.com/2",
      stringsAsFactors = FALSE
    )
    # Whitespace-only and NA values should be treated as no filter
    result <- filter_links_by_domain(links, keep_domains = c("  ", NA))
    expect_equal(nrow(result), 1) # No effective filter
  })

  it("handles hosts that resolve to empty after trim/NA removal", {
    links <- data.frame(
      from = "http://a.com/1", to = "http://a.com/2",
      stringsAsFactors = FALSE
    )
    result <- filter_links_by_domain(links, keep_hosts = c("", NA))
    expect_equal(nrow(result), 1)
  })

  it(
    "handles edge list where all URLs are NA/empty (build_url_maps empty path)",
    {
      links <- data.frame(
        from = c(NA_character_), to = c(NA_character_),
        stringsAsFactors = FALSE
      )
      # With keep_domains set, the function will try to map URLs. All NAs means
      # the map is empty. After filtering, no rows should remain.
      result <- filter_links_by_domain(links, keep_domains = "example.com")
      expect_equal(nrow(result), 0)
    }
  )
})
