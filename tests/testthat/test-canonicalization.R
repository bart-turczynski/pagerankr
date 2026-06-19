context("canonicalization profile")

describe("canonical_profile", {
  it("pins every rurl::get_clean_url canonicalization knob explicitly", {
    profile <- canonical_profile()
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
    profile <- canonical_profile()
    defaults <- formals(rurl::get_clean_url)
    for (k in names(profile)) {
      expect_identical(
        profile[[k]], eval(defaults[[k]]),
        info = paste("profile diverges from rurl default for", k)
      )
    }
  })
})

describe("resolve_rurl_params (via clean_url_columns)", {
  it("applies the canonical profile by default (no overrides)", {
    urls <- c("HTTP://EXAMPLE.COM/Path/", "https://Sub.Example.CO.UK/a")
    df <- data.frame(url = urls)
    cleaned <- clean_url_columns(df, columns = "url")
    expect_equal(
      cleaned$url,
      unname(rurl::get_clean_url(urls))
    )
  })

  it("applies a case_handling override when supplied", {
    df <- data.frame(url = "HTTP://EXAMPLE.COM/path")
    default_result <- clean_url_columns(df, columns = "url")
    expect_true(grepl("example.com", default_result$url, fixed = TRUE))

    override_result <- clean_url_columns(df, columns = "url", case_handling = "keep")
    expect_true(grepl("EXAMPLE.COM", override_result$url, fixed = TRUE))
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
