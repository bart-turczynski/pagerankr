# Exported names are US English; each British spelling is also exported as an
# alias bound to the very same function object (SEOR-qwomlgjd).

describe("British-spelling aliases", {
  exports <- getNamespaceExports("pagerankr")

  it("binds analyse_pagerank_grid to analyze_pagerank_grid", {
    expect_identical(analyse_pagerank_grid, analyze_pagerank_grid)
    expect_true("analyse_pagerank_grid" %in% exports)
  })

  it("binds sf_normalise_position to sf_normalize_position", {
    expect_identical(sf_normalise_position, sf_normalize_position)
    expect_true("sf_normalise_position" %in% exports)
  })
})
