context("auto_grid")

describe("auto_grid", {
  it("generates all combinations of parameters", {
    grid <- auto_grid(damping = c(0.85, 0.95), self_loops = c("drop", "keep"))
    expect_equal(length(grid), 4)
    expect_true(all(vapply(grid, is.list, logical(1))))
    # Check all combinations exist
    dampings <- vapply(grid, function(x) x$damping, numeric(1))
    loops <- vapply(grid, function(x) x$self_loops, character(1))
    expect_equal(sort(unique(dampings)), c(0.85, 0.95))
    expect_equal(sort(unique(loops)), c("drop", "keep"))
  })

  it("generates descriptive model IDs", {
    grid <- auto_grid(damping = c(0.5, 0.9))
    expect_true("damping=0.5" %in% names(grid))
    expect_true("damping=0.9" %in% names(grid))
  })

  it("works with a single parameter", {
    grid <- auto_grid(damping = c(0.7, 0.85, 0.95))
    expect_equal(length(grid), 3)
  })

  it("integrates with pagerank_grid()", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      stringsAsFactors = FALSE
    )
    grid_params <- auto_grid(damping = c(0.5, 0.85))
    result <- pagerank_grid(edges, grid_params, clean_edge_urls = FALSE)
    expect_equal(length(unique(result$model_id)), 2)
    expect_equal(nrow(result), 4) # 2 models x 2 nodes
  })

  it("errors on unnamed or empty arguments", {
    expect_error(auto_grid(), "At least one")
    expect_error(auto_grid(c(0.5, 0.85)), "must be named")
  })
})
