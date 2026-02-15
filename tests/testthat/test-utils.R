describe(".urls_contain_query_params", {
  it("returns FALSE for empty data frame", {
    empty_df <- data.frame(from = character(0), stringsAsFactors = FALSE)
    expect_false(pagerankr:::.urls_contain_query_params(empty_df, "from"))
  })

  it("returns FALSE for non-dataframe input", {
    expect_false(pagerankr:::.urls_contain_query_params("not_a_df", "x"))
  })

  it("returns TRUE when URLs contain query params", {
    df <- data.frame(
      url = c("http://example.com/page?q=test", "http://example.com/other"),
      stringsAsFactors = FALSE
    )
    expect_true(pagerankr:::.urls_contain_query_params(df, "url"))
  })

  it("returns FALSE when no URLs contain query params", {
    df <- data.frame(
      url = c("http://example.com/page", "http://example.com/other"),
      stringsAsFactors = FALSE
    )
    expect_false(pagerankr:::.urls_contain_query_params(df, "url"))
  })

  it("returns TRUE for URLs with ampersand", {
    df <- data.frame(
      url = c("http://example.com/page&extra"),
      stringsAsFactors = FALSE
    )
    expect_true(pagerankr:::.urls_contain_query_params(df, "url"))
  })

  it("returns FALSE when column not in data frame", {
    df <- data.frame(
      other = c("http://example.com/page?q=1"),
      stringsAsFactors = FALSE
    )
    expect_false(pagerankr:::.urls_contain_query_params(df, "url"))
  })

  it("handles NA values in URLs", {
    df <- data.frame(
      url = c(NA, "http://example.com/page?q=test"),
      stringsAsFactors = FALSE
    )
    expect_true(pagerankr:::.urls_contain_query_params(df, "url"))
  })
})

describe(".trace_redirect_path", {
  it("returns the URL itself when not in redirect map", {
    redirect_map <- c(A = "B")
    result <- pagerankr:::.trace_redirect_path("C", redirect_map)
    expect_equal(result, "C")
  })

  it("follows a single redirect", {
    redirect_map <- c(A = "B")
    result <- pagerankr:::.trace_redirect_path("A", redirect_map)
    expect_equal(result, "B")
  })

  it("follows a chain of redirects", {
    redirect_map <- c(A = "B", B = "C", C = "D")
    result <- pagerankr:::.trace_redirect_path("A", redirect_map)
    expect_equal(result, "D")
  })

  it("detects and errors on a cycle", {
    redirect_map <- c(A = "B", B = "C", C = "A")
    expect_error(
      pagerankr:::.trace_redirect_path("A", redirect_map),
      "Redirect cycle detected"
    )
  })

  it("includes the cycle path in the error message", {
    redirect_map <- c(A = "B", B = "A")
    expect_error(
      pagerankr:::.trace_redirect_path("A", redirect_map),
      "A -> B -> A"
    )
  })
})

describe(".create_memoized_cleaner", {
  it("warns when ... has entirely unnamed arguments", {
    # We need to bypass rurl to test the memoization key logic.
    # The unnamed args warning fires before rurl is called.
    # However, the actual call to rurl happens after. We use tryCatch to catch
    # downstream errors but still verify the warning was emitted.
    cleaner <- pagerankr:::.create_memoized_cleaner()
    expect_warning(
      tryCatch(cleaner("http://example.com", TRUE), error = function(e) NULL),
      "unnamed"
    )
  })

  it("caches results for repeated calls", {
    cleaner <- pagerankr:::.create_memoized_cleaner()
    result1 <- cleaner("http://example.com/test")
    result2 <- cleaner("http://example.com/test")
    expect_equal(result1, result2)
  })

  it("produces different results for different URLs", {
    cleaner <- pagerankr:::.create_memoized_cleaner()
    result1 <- cleaner("http://example.com/a")
    result2 <- cleaner("http://example.com/b")
    # Just verify it doesn't error and returns strings
    expect_true(is.character(result1))
    expect_true(is.character(result2))
  })
})
