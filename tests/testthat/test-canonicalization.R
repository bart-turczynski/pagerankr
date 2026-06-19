context("canonicalization profile")

describe(".canonical_profile", {
  it("pins every rurl::get_clean_url canonicalization knob explicitly", {
    profile <- pagerankr:::.canonical_profile()
    expect_named(
      profile,
      c(
        "protocol_handling", "case_handling", "www_handling",
        "trailing_slash_handling", "index_page_handling", "path_normalization",
        "scheme_relative_handling", "subdomain_levels_to_keep",
        "host_encoding", "path_encoding"
      )
    )
    # The contract anchors: keep scheme, lower the host.
    expect_equal(profile$protocol_handling, "keep")
    expect_equal(profile$case_handling, "lower_host")
  })

  it("mirrors rurl's current defaults (so pinning is behavior-preserving)", {
    profile <- pagerankr:::.canonical_profile()
    defaults <- formals(rurl::get_clean_url)
    for (k in names(profile)) {
      expect_identical(
        profile[[k]], eval(defaults[[k]]),
        info = paste("profile diverges from rurl default for", k)
      )
    }
  })
})

describe(".resolve_rurl_params", {
  it("returns the bare profile for empty/NULL input", {
    expect_identical(
      pagerankr:::.resolve_rurl_params(list()),
      pagerankr:::.canonical_profile()
    )
    expect_identical(
      pagerankr:::.resolve_rurl_params(NULL),
      pagerankr:::.canonical_profile()
    )
  })

  it("overrides individual knobs per key", {
    res <- pagerankr:::.resolve_rurl_params(list(case_handling = "keep"))
    expect_equal(res$case_handling, "keep")
    expect_equal(res$protocol_handling, "keep") # untouched
  })

  it("passes through unknown keys (forward-compatible with new rurl args)", {
    res <- pagerankr:::.resolve_rurl_params(list(some_future_arg = "x"))
    expect_equal(res$some_future_arg, "x")
  })

  it("errors on a non-list", {
    expect_error(pagerankr:::.resolve_rurl_params("nope"), "must be a list")
  })
})

describe("profile is behavior-preserving end to end", {
  it("clean_url_columns matches rurl defaults when no overrides are given", {
    urls <- c(
      "HTTP://WWW.Example.COM/Path/", "https://sub.example.co.uk/a?b=1#f",
      "example.org/index.html"
    )
    df <- data.frame(from = urls, to = urls)
    cleaned <- clean_url_columns(df, columns = "from")
    expect_equal(unname(cleaned$from), unname(rurl::get_clean_url(urls)))
  })
})
