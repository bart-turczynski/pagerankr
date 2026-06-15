context("pagerank_metrics")

describe("pr_gini", {
  it("returns 0 for perfectly equal distribution", {
    # All values equal -> Gini = 0
    expect_equal(pr_gini(rep(0.25, 4)), 0, tolerance = 1e-9)
  })

  it("approaches 1 for maximally concentrated distribution", {
    # One node has everything
    x <- c(1, rep(0, 99))
    g <- pr_gini(x)
    expect_gt(g, 0.95)
  })

  it("returns NA for empty or all-zero input", {
    expect_true(is.na(pr_gini(numeric(0))))
    expect_true(is.na(pr_gini(c(0, 0, 0))))
  })

  it("handles NAs in input", {
    # NAs are dropped
    expect_equal(pr_gini(c(0.5, 0.5, NA)), pr_gini(c(0.5, 0.5)))
  })

  it("increases with inequality", {
    equal <- pr_gini(c(0.25, 0.25, 0.25, 0.25))
    unequal <- pr_gini(c(0.7, 0.1, 0.1, 0.1))
    expect_gt(unequal, equal)
  })
})

describe("pr_entropy", {
  it("is maximised for uniform distribution", {
    n <- 10
    uniform <- rep(1 / n, n)
    max_entropy <- log(n) # theoretical maximum
    expect_equal(pr_entropy(uniform), max_entropy, tolerance = 1e-9)
  })

  it("is 0 for maximally concentrated distribution", {
    expect_equal(pr_entropy(c(1, 0, 0)), 0, tolerance = 1e-9)
  })

  it("returns NA for empty or all-zero input", {
    expect_true(is.na(pr_entropy(numeric(0))))
    expect_true(is.na(pr_entropy(c(0, 0, 0))))
  })

  it("normalises input to sum to 1", {
    # c(2, 2, 2) and c(1/3, 1/3, 1/3) should give the same entropy
    expect_equal(pr_entropy(c(2, 2, 2)), pr_entropy(c(1 / 3, 1 / 3, 1 / 3)),
      tolerance = 1e-9
    )
  })
})

describe("pr_top_k_share", {
  it("top 100% share is always 1", {
    expect_equal(pr_top_k_share(c(0.5, 0.3, 0.2), k = 1.0), 1, tolerance = 1e-9)
  })

  it("top 10% of 10 nodes is the max value share", {
    x <- c(0.5, rep(0.5 / 9, 9))
    share <- pr_top_k_share(x, k = 0.1)
    expect_equal(share, 0.5, tolerance = 1e-9)
  })

  it("returns NA for empty input", {
    expect_true(is.na(pr_top_k_share(numeric(0))))
  })

  it("errors on invalid k", {
    expect_error(pr_top_k_share(c(1), k = 0), "between 0")
    expect_error(pr_top_k_share(c(1), k = 1.5), "between 0")
  })

  it("always returns at least 1 node even for small k", {
    # k = 0.01 on 3 nodes -> ceiling(0.03) = 1 node
    x <- c(0.5, 0.3, 0.2)
    share <- pr_top_k_share(x, k = 0.01)
    expect_equal(share, 0.5, tolerance = 1e-9)
  })
})
