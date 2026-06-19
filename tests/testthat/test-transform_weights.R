# Tests for transform_weights()

describe("transform_weights: none", {
  it("returns input unchanged", {
    x <- c(10, 20, 30)
    expect_equal(transform_weights(x, "none"), x)
  })

  it("preserves NAs", {
    x <- c(10, NA, 30)
    expect_equal(transform_weights(x, "none"), x)
  })
})


describe("transform_weights: rank_linear", {
  it("assigns linearly decreasing weights by rank (descending)", {
    # Higher value = higher weight (rank 1)
    x <- c(100, 50, 10)
    result <- transform_weights(x, "rank_linear", descending = TRUE)
    expect_equal(result[1], 1.0) # rank 1 (highest value)
    expect_gt(result[1], result[2])
    expect_gt(result[2], result[3])
  })

  it("assigns weights for ascending (link position)", {
    # Position 1 is most valuable (smallest number = rank 1)
    positions <- c(1, 2, 3, 4, 5)
    result <- transform_weights(positions, "rank_linear", descending = FALSE)
    expect_equal(result[1], 1.0) # position 1 = most valuable
    expect_gt(result[1], result[5])
    expect_length(result, 5)
  })

  it("handles ties with average rank", {
    x <- c(10, 10, 5)
    result <- transform_weights(x, "rank_linear", descending = TRUE)
    # 10 and 10 tie for rank 1-2, average rank = 1.5
    expect_equal(result[1], result[2])
  })

  it("handles single value", {
    result <- transform_weights(1, "rank_linear")
    expect_equal(result, 1.0)
  })
})


describe("transform_weights: zipf", {
  it("applies Zipf's law with alpha = 1", {
    x <- c(100, 50, 10) # descending: 100 is rank 1
    result <- transform_weights(x, "zipf", alpha = 1, descending = TRUE)
    expect_equal(result[1], 1.0) # rank 1: weight is 1
    expect_equal(result[2], 0.5) # rank 2: weight is 0.5
    expect_lt(result[3], result[2]) # rank 3: even smaller
  })

  it("steeper drop-off with higher alpha", {
    x <- c(100, 50, 10)
    r1 <- transform_weights(x, "zipf", alpha = 1, descending = TRUE)
    r2 <- transform_weights(x, "zipf", alpha = 2, descending = TRUE)
    # Higher alpha = steeper, so ratio between rank 1 and rank 2 is larger
    expect_lt(r1[1] / r1[2], r2[1] / r2[2])
  })

  it("works with descending = FALSE for positions", {
    positions <- c(1, 2, 3)
    result <- transform_weights(positions, "zipf",
      alpha = 1,
      descending = FALSE
    )
    # Position 1 (smallest) should get rank 1 (highest weight)
    expect_equal(result[1], 1.0)
    expect_equal(result[2], 0.5)
  })
})


describe("transform_weights: log", {
  it("compresses large ranges", {
    clicks <- c(50000, 100, 1)
    result <- transform_weights(clicks, "log")
    # log(50001) ~ 10.8, log(101) ~ 4.6, log(2) ~ 0.69
    expect_gt(result[1], result[2])
    expect_gt(result[2], result[3])
    # But the ratio is much less extreme than raw
    expect_lt(result[1] / result[3], clicks[1] / clicks[3])
  })

  it("handles zeros with default offset", {
    x <- c(0, 1, 10)
    result <- transform_weights(x, "log", offset = 1)
    expect_equal(result[1], 0) # log of (0 + offset) equals 0
    expect_gt(result[2], 0)
  })

  it("custom offset works", {
    x <- c(0, 1)
    r1 <- transform_weights(x, "log", offset = 1)
    r2 <- transform_weights(x, "log", offset = 10)
    expect_gt(r2[1], r1[1]) # larger offset -> larger result
  })
})


describe("transform_weights: minmax", {
  it("scales to [floor, 1]", {
    x <- c(0, 50, 100)
    result <- transform_weights(x, "minmax", floor_value = 0.01)
    expect_equal(result[3], 1.0)
    expect_equal(result[1], 0.01)
    expect_gt(result[2], 0.01)
    expect_lt(result[2], 1.0)
  })

  it("handles identical values", {
    x <- c(5, 5, 5)
    result <- transform_weights(x, "minmax")
    expect_true(all(result == 1.0))
  })

  it("custom floor_value", {
    x <- c(0, 100)
    result <- transform_weights(x, "minmax", floor_value = 0.1)
    expect_equal(result[1], 0.1)
    expect_equal(result[2], 1.0)
  })
})


describe("transform_weights: percentile", {
  it("maps to percentiles", {
    x <- c(10, 20, 30, 40, 50)
    result <- transform_weights(x, "percentile", descending = TRUE)
    # Ascending ranks: 10=rank1, 50=rank5
    # Percentile equals rank divided by n
    expect_equal(result[5], 1.0) # 50 is highest = percentile 1.0
    expect_equal(result[1], 0.2) # 10 is lowest = percentile 0.2
  })

  it("handles ties", {
    x <- c(10, 10, 30)
    result <- transform_weights(x, "percentile", descending = TRUE)
    expect_equal(result[1], result[2])
  })
})


describe("transform_weights: NA handling", {
  it("preserves NAs across all methods", {
    x <- c(10, NA, 30, NA, 50)
    for (m in c("rank_linear", "zipf", "log", "minmax", "percentile")) {
      result <- transform_weights(x, m)
      expect_true(is.na(result[2]), info = paste("Method:", m))
      expect_true(is.na(result[4]), info = paste("Method:", m))
      expect_false(is.na(result[1]), info = paste("Method:", m))
    }
  })

  it("returns all-NA for all-NA input", {
    x <- c(NA_real_, NA_real_)
    for (m in c("rank_linear", "zipf", "log", "minmax", "percentile")) {
      result <- transform_weights(x, m)
      expect_true(all(is.na(result)), info = paste("Method:", m))
    }
  })
})


describe("transform_weights: input validation", {
  it("errors on non-numeric input", {
    expect_error(transform_weights("abc", "log"), "must be a numeric")
  })

  it("errors on invalid alpha", {
    expect_error(transform_weights(1:3, "zipf", alpha = -1), "positive number")
  })

  it("errors on invalid offset", {
    expect_error(transform_weights(1:3, "log", offset = "bad"), "single number")
  })

  it("errors on invalid floor_value", {
    expect_error(
      transform_weights(1:3, "minmax", floor_value = -0.1),
      "non-negative"
    )
  })

  it("errors on invalid descending", {
    expect_error(
      transform_weights(1:3, "zipf", descending = "yes"),
      "must be TRUE or FALSE"
    )
  })
})


describe("transform_weights integration with pagerank", {
  it("can be used with weight_col in pagerank()", {
    edges <- data.frame(
      from = c("A", "A", "B", "C"),
      to = c("B", "C", "C", "A"),
      clicks = c(1000, 50, 200, 300)
    )
    edges$weight <- transform_weights(edges$clicks, "zipf")
    pr <- pagerank(edges, weight_col = "weight", clean_edge_urls = FALSE)
    expect_true(is.data.frame(pr))
    expect_gt(nrow(pr), 0)
  })
})
