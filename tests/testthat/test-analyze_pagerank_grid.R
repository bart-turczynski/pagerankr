context("analyze_pagerank_grid")

describe("analyze_pagerank_grid", {
  it("computes metrics for each model in a grid result", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      stringsAsFactors = FALSE
    )
    params <- list(
      low = list(damping = 0.5),
      high = list(damping = 0.95)
    )
    grid <- pagerank_grid(edges, params, clean_edge_urls = FALSE)
    analysis <- analyze_pagerank_grid(grid)

    expect_true(is.data.frame(analysis))
    expect_equal(nrow(analysis), 2)
    expect_true(all(c("model_id", "num_nodes", "pr_sum", "pr_max",
                       "pr_gini", "pr_entropy", "pr_top10_share") %in%
                    names(analysis)))
    expect_equal(sort(analysis$model_id), c("high", "low"))
    # Each model has 3 nodes
    expect_true(all(analysis$num_nodes == 3))
    # PR sums to ~1 for standard graphs
    expect_true(all(abs(analysis$pr_sum - 1) < 0.01))
  })

  it("handles single-model grids", {
    grid <- data.frame(
      model_id = rep("only", 3),
      node_name = c("A", "B", "C"),
      pagerank = c(0.5, 0.3, 0.2),
      stringsAsFactors = FALSE
    )
    analysis <- analyze_pagerank_grid(grid)
    expect_equal(nrow(analysis), 1)
    expect_equal(analysis$model_id, "only")
    expect_equal(analysis$num_nodes, 3)
    expect_equal(analysis$pr_max, 0.5)
  })

  it("errors on missing columns", {
    bad <- data.frame(x = 1)
    expect_error(analyze_pagerank_grid(bad), "not found")
  })

  it("errors on non-dataframe input", {
    expect_error(analyze_pagerank_grid("not_a_df"), "data frame")
  })

  it("errors when model_id_col is missing", {
    df <- data.frame(pagerank = 0.5, stringsAsFactors = FALSE)
    expect_error(analyze_pagerank_grid(df), "not found")
  })

  it("errors when pr_col is missing", {
    df <- data.frame(model_id = "m1", wrong = 0.5, stringsAsFactors = FALSE)
    expect_error(analyze_pagerank_grid(df), "not found")
  })
})
