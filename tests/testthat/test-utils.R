describe(".urls_contain_query_params", {
  it("returns FALSE for empty data frame", {
    empty_df <- data.frame(from = character(0), stringsAsFactors = FALSE)
    expect_false(pagerankr:::.urls_contain_query_params(empty_df, "from"))
  })

  it("returns FALSE for non-dataframe input", {
    expect_false(pagerankr:::.urls_contain_query_params("not_a_df", "x"))
  })
})

describe(".create_memoized_cleaner unnamed args", {
  it("warns when ... has entirely unnamed arguments", {
    # We need to bypass rurl to test the memoization key logic.
    # The unnamed args warning fires before rurl is called (in the key construction).
    # However, the actual call to rurl happens after. We use tryCatch to catch
    # downstream errors but still verify the warning was emitted.
    cleaner <- pagerankr:::.create_memoized_cleaner()
    expect_warning(
      tryCatch(cleaner("http://example.com", TRUE), error = function(e) NULL),
      "unnamed"
    )
  })
})
