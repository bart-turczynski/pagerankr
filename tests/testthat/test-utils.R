describe("pagerank warns on edge list query parameters (clean_edge_urls = FALSE)", {
  it("fires the warning when a URL contains '?'", {
    df <- data.frame(
      from = c("http://example.com/page?q=test", "http://example.com/other"),
      to = c("http://example.com/other", "http://example.com/end")
    )
    expect_warning(
      pagerank(df, clean_edge_urls = FALSE),
      "query parameters"
    )
  })

  it("fires the warning when a URL contains '&'", {
    df <- data.frame(
      from = "http://example.com/page&extra",
      to = "http://example.com/other"
    )
    expect_warning(
      pagerank(df, clean_edge_urls = FALSE),
      "query parameters"
    )
  })

  it("does not warn when URLs have no query parameters", {
    df <- data.frame(
      from = "http://example.com/page",
      to = "http://example.com/other"
    )
    expect_warning(
      pagerank(df, clean_edge_urls = FALSE),
      regexp = NA
    )
  })

  it("handles NA values alongside query-param URLs (still warns)", {
    df <- data.frame(
      from = c(NA, "http://example.com/page?q=test"),
      to = c("http://example.com/other", "http://example.com/end")
    )
    expect_warning(
      pagerank(df, clean_edge_urls = FALSE),
      "query parameters"
    )
  })
})
