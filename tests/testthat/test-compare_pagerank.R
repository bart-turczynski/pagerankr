context("compare_pagerank")

describe("compare_pagerank basic functionality", {
  it("performs full outer join and computes deltas", {
    pr_a <- data.frame(
      node_name = c("A", "B", "C"),
      pagerank = c(0.5, 0.3, 0.2)
    )
    pr_b <- data.frame(
      node_name = c("A", "B", "D"),
      pagerank = c(0.4, 0.35, 0.25)
    )
    result <- compare_pagerank(pr_a, pr_b)

    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 4) # A, B, C, D
    expect_true(all(c(
      "node_name", "pagerank_a", "pagerank_b",
      "delta", "pct_change", "rank_a", "rank_b",
      "rank_delta"
    ) %in% names(result)))

    # A is in both: delta = 0.4 - 0.5 = -0.1
    a_row <- result[result$node_name == "A", ]
    expect_equal(a_row$pagerank_a, 0.5)
    expect_equal(a_row$pagerank_b, 0.4)
    expect_equal(a_row$delta, -0.1, tolerance = 1e-9)
    expect_equal(a_row$pct_change, -20, tolerance = 1e-9)

    # C is only in a
    c_row <- result[result$node_name == "C", ]
    expect_equal(c_row$pagerank_a, 0.2)
    expect_true(is.na(c_row$pagerank_b))

    # D is only in b
    d_row <- result[result$node_name == "D", ]
    expect_true(is.na(d_row$pagerank_a))
    expect_equal(d_row$pagerank_b, 0.25)
  })

  it("computes ranks correctly (1 = highest)", {
    pr_a <- data.frame(
      node_name = c("X", "Y", "Z"),
      pagerank = c(0.5, 0.3, 0.2)
    )
    pr_b <- data.frame(
      node_name = c("X", "Y", "Z"),
      pagerank = c(0.2, 0.5, 0.3)
    )
    result <- compare_pagerank(pr_a, pr_b)

    x_row <- result[result$node_name == "X", ]
    expect_equal(x_row$rank_a, 1) # highest PR in a
    expect_equal(x_row$rank_b, 3) # lowest PR in b

    y_row <- result[result$node_name == "Y", ]
    expect_equal(y_row$rank_a, 2)
    expect_equal(y_row$rank_b, 1)

    # rank_delta: rank_a - rank_b, positive = improved in b
    # For Y, rank_a minus rank_b yields one, meaning improved.
    expect_equal(y_row$rank_delta, 1)
    # For X, rank_a minus rank_b yields negative two, meaning worsened.
    expect_equal(x_row$rank_delta, -2)
  })

  it("attaches summary statistics as attribute", {
    pr_a <- data.frame(
      node_name = c("A", "B", "C"),
      pagerank = c(0.5, 0.3, 0.2)
    )
    pr_b <- data.frame(
      node_name = c("A", "B", "D"),
      pagerank = c(0.4, 0.35, 0.25)
    )
    result <- compare_pagerank(pr_a, pr_b)
    summary <- attr(result, "summary")

    expect_true(is.list(summary))
    expect_true(all(c(
      "spearman_rho", "mean_abs_delta",
      "nodes_gained", "nodes_lost"
    ) %in% names(summary)))
    expect_equal(summary$nodes_gained, 1) # D
    expect_equal(summary$nodes_lost, 1) # C
    expect_true(is.numeric(summary$spearman_rho))
    expect_true(is.numeric(summary$mean_abs_delta))
    expect_gt(summary$mean_abs_delta, 0)
  })

  it("handles custom column names and labels", {
    pr_a <- data.frame(url = c("X"), pr = c(0.5))
    pr_b <- data.frame(url = c("X"), pr = c(0.8))
    result <- compare_pagerank(pr_a, pr_b,
      node_col = "url", pr_col = "pr",
      label_a = "base", label_b = "new"
    )
    expect_true("pr_base" %in% names(result))
    expect_true("pr_new" %in% names(result))
    expect_true("rank_base" %in% names(result))
    expect_true("rank_new" %in% names(result))
  })

  it("handles identical data frames", {
    pr <- data.frame(
      node_name = c("A", "B"),
      pagerank = c(0.6, 0.4)
    )
    result <- compare_pagerank(pr, pr)
    expect_true(all(result$delta == 0))
    expect_true(all(result$pct_change == 0))
    summary <- attr(result, "summary")
    expect_equal(summary$nodes_gained, 0)
    expect_equal(summary$nodes_lost, 0)
    expect_equal(summary$mean_abs_delta, 0, tolerance = 1e-9)
  })

  it("errors on missing columns", {
    bad <- data.frame(x = 1)
    good <- data.frame(
      node_name = "A",
      pagerank = 0.5
    )
    expect_error(compare_pagerank(bad, good), "not found")
    expect_error(compare_pagerank(good, bad), "not found")
  })

  it("errors on non-dataframe inputs", {
    expect_error(compare_pagerank("not_a_df", data.frame()), "data frame")
    good <- data.frame(
      node_name = "A",
      pagerank = 0.5
    )
    expect_error(compare_pagerank(good, "not_a_df"), "data frame")
  })

  it("errors when pr_col is missing from pr_a", {
    pr_a <- data.frame(node_name = "A", wrong = 0.5)
    pr_b <- data.frame(
      node_name = "A",
      pagerank = 0.5
    )
    expect_error(compare_pagerank(pr_a, pr_b), "not found")
  })

  it("errors when pr_col is missing from pr_b", {
    pr_a <- data.frame(
      node_name = "A",
      pagerank = 0.5
    )
    pr_b <- data.frame(node_name = "A", wrong = 0.5)
    expect_error(compare_pagerank(pr_a, pr_b), "not found")
  })
})
