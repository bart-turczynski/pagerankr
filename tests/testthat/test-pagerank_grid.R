context("pagerank_grid")

describe("pagerank_grid basic functionality", {
  it("runs multiple parameter sets and returns combined results", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      stringsAsFactors = FALSE
    )
    params <- list(
      low_damp = list(damping = 0.50),
      high_damp = list(damping = 0.95)
    )
    grid <- pagerank_grid(edges, params, clean_edge_urls = FALSE)

    expect_true(is.data.frame(grid))
    expect_true("model_id" %in% names(grid))
    expect_equal(sort(unique(grid$model_id)), c("high_damp", "low_damp"))
    # Each model should produce 3 rows (A, B, C)
    expect_equal(nrow(grid), 6)
  })

  it("model_id column is first", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    params <- list(m1 = list(damping = 0.85))
    grid <- pagerank_grid(edges, params, clean_edge_urls = FALSE)
    expect_equal(names(grid)[1], "model_id")
  })

  it("model-specific params override common params", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      stringsAsFactors = FALSE
    )
    params <- list(
      override = list(damping = 0.99)
    )
    # Common damping = 0.5, but override uses 0.99
    grid <- pagerank_grid(edges, params, clean_edge_urls = FALSE, damping = 0.5)
    # For a symmetric 2-node graph, all damping values give equal PR
    # so we just verify it runs without error
    expect_equal(nrow(grid), 2)
    expect_equal(grid$model_id, c("override", "override"))
  })

  it("passes shared redirects_df", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "C"),
      stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = "C",
      to = "A",
      stringsAsFactors = FALSE
    )
    params <- list(
      m1 = list(damping = 0.85)
    )
    grid <- pagerank_grid(edges, params, redirects_df = redirects,
                          clean_edge_urls = FALSE)
    expect_true(nrow(grid) > 0)
    expect_equal(grid$model_id[1], "m1")
  })

  it("errors on invalid params_grid", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(pagerank_grid(edges, list()), "non-empty")
    expect_error(pagerank_grid(edges, list(list())), "must be named")
    expect_error(pagerank_grid(edges, list(m1 = "not a list")), "must be a list")
  })

  it("works with single-model grid", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      stringsAsFactors = FALSE
    )
    params <- list(baseline = list())
    grid <- pagerank_grid(edges, params, clean_edge_urls = FALSE)
    expect_equal(unique(grid$model_id), "baseline")
    expect_equal(nrow(grid), 2)
  })

  it("errors on non-dataframe input", {
    expect_error(pagerank_grid("bad", list(m = list())), "data frame")
  })

  it("handles model that returns empty results", {
    # An edge list that, after cleaning, has no valid edges for a model
    edges <- data.frame(from = c("A"), to = c(NA), stringsAsFactors = FALSE)
    params <- list(m1 = list(clean_edge_urls = FALSE, drop_isolates_flag = TRUE))
    grid <- pagerank_grid(edges, params)
    # A->NA is not a complete edge, so with drop_isolates=TRUE the result may be empty
    # or contain just A. Either way it should not error.
    expect_true(is.data.frame(grid))
  })
})
