# Tests for transform_edge_weights()

# A multi-source fixture: two source pages, each with its own choice set.
multi_source_edges <- function() {
  data.frame(
    from = c("A", "A", "A", "B", "B"),
    to = c("B", "C", "D", "C", "D"),
    position = c(1, 2, 3, 1, 2),
    stringsAsFactors = FALSE
  )
}

describe("transform_edge_weights: structure", {
  it("adds weight and transition_probability columns, preserving rows", {
    edges <- multi_source_edges()
    out <- transform_edge_weights(edges, "position",
      method = "zipf", descending = FALSE
    )
    expect_true(is.data.frame(out))
    expect_equal(nrow(out), nrow(edges))
    expect_true(all(c("weight", "transition_probability") %in% names(out)))
    # Original columns preserved unchanged
    expect_equal(out$from, edges$from)
    expect_equal(out$to, edges$to)
    expect_equal(out$position, edges$position)
  })

  it("honours custom output column names", {
    edges <- multi_source_edges()
    out <- transform_edge_weights(edges, "position",
      method = "zipf", descending = FALSE,
      weight_col = "w", prob_col = "p"
    )
    expect_true(all(c("w", "p") %in% names(out)))
  })
})


describe("transform_edge_weights: per-source grouping", {
  it("computes the transform within each source's choice set", {
    edges <- multi_source_edges()
    out <- transform_edge_weights(edges, "position",
      method = "zipf", alpha = 1, descending = FALSE
    )

    # Source A: positions 1,2,3 -> zipf ranks 1,2,3 -> 1, 0.5, 1/3
    a <- out[out$from == "A", ]
    expect_equal(a$weight, c(1, 0.5, 1 / 3))

    # Source B: positions 1,2 -> zipf ranks 1,2 -> 1, 0.5
    # (rank is local to B, NOT global)
    b <- out[out$from == "B", ]
    expect_equal(b$weight, c(1, 0.5))
  })

  it("position 1 is top-of-choice-set for each source independently", {
    edges <- multi_source_edges()
    out <- transform_edge_weights(edges, "position",
      method = "rank_linear", descending = FALSE
    )
    # Each source's position-1 link gets weight 1.0
    expect_equal(out$weight[out$from == "A"][1], 1.0)
    expect_equal(out$weight[out$from == "B"][1], 1.0)
  })
})


describe("transform_edge_weights: transition_probability", {
  it("sums to 1 within each source group", {
    edges <- multi_source_edges()
    out <- transform_edge_weights(edges, "position",
      method = "zipf", descending = FALSE
    )
    sums <- tapply(
      out$transition_probability, out$from,
      function(p) sum(p, na.rm = TRUE)
    )
    expect_equal(as.numeric(sums), rep(1, length(sums)))
  })

  it("sums to 1 within each source for every method", {
    edges <- multi_source_edges()
    for (m in c("rank_linear", "zipf", "log", "minmax", "percentile")) {
      out <- transform_edge_weights(edges, "position",
        method = m, descending = FALSE
      )
      sums <- tapply(
        out$transition_probability, out$from,
        function(p) sum(p, na.rm = TRUE)
      )
      expect_equal(as.numeric(sums), rep(1, length(sums)),
        info = paste("method:", m)
      )
    }
  })

  it("yields NA probabilities for an all-NA choice set", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      position = c(NA_real_, NA_real_, 1),
      stringsAsFactors = FALSE
    )
    out <- transform_edge_weights(edges, "position",
      method = "zipf", descending = FALSE
    )
    expect_true(all(is.na(out$transition_probability[out$from == "A"])))
    # B still normalizes to 1
    expect_equal(sum(out$transition_probability[out$from == "B"]), 1)
  })
})


describe("transform_edge_weights: single-group equivalence", {
  it("matches transform_weights() when one source owns the whole graph", {
    # Every edge shares the same source -> a single choice set spanning
    # the whole graph -> grouped transform == global transform_weights().
    edges <- data.frame(
      from = rep("A", 5),
      to = c("B", "C", "D", "E", "F"),
      clicks = c(50000, 12000, 800, 150, 3),
      stringsAsFactors = FALSE
    )
    for (m in c("none", "rank_linear", "zipf", "log", "minmax", "percentile")) {
      out <- transform_edge_weights(edges, "clicks", method = m)
      expected <- transform_weights(edges$clicks, method = m)
      expect_equal(out$weight, expected, info = paste("method:", m))
    }
  })

  it("forwards ... arguments to transform_weights()", {
    edges <- data.frame(
      from = rep("A", 3),
      to = c("B", "C", "D"),
      position = c(1, 2, 3),
      stringsAsFactors = FALSE
    )
    out <- transform_edge_weights(edges, "position",
      method = "zipf", alpha = 2, descending = FALSE
    )
    expected <- transform_weights(edges$position,
      method = "zipf", alpha = 2, descending = FALSE
    )
    expect_equal(out$weight, expected)
  })
})


describe("transform_edge_weights: validation", {
  it("errors when value_col is missing", {
    edges <- multi_source_edges()
    expect_error(
      transform_edge_weights(edges, "nope"),
      "not found"
    )
  })

  it("errors when by column is missing", {
    edges <- multi_source_edges()
    expect_error(
      transform_edge_weights(edges, "position", by = "source"),
      "not found"
    )
  })

  it("errors when value_col is not numeric", {
    edges <- multi_source_edges()
    edges$label <- letters[1:5]
    expect_error(
      transform_edge_weights(edges, "label"),
      "must be numeric"
    )
  })

  it("errors when edge_list_df is not a data frame", {
    expect_error(
      transform_edge_weights(1:5, "position"),
      "must be a data frame"
    )
  })

  it("handles a zero-row data frame", {
    edges <- multi_source_edges()[0, ]
    out <- transform_edge_weights(edges, "position")
    expect_equal(nrow(out), 0)
    expect_true(all(c("weight", "transition_probability") %in% names(out)))
  })
})


describe("transform_edge_weights: integration with pagerank", {
  it("produces weights usable as weight_col in pagerank()", {
    edges <- multi_source_edges()
    out <- transform_edge_weights(edges, "position",
      method = "zipf", descending = FALSE
    )
    pr <- pagerank(out, weight_col = "weight", clean_edge_urls = FALSE)
    expect_true(is.data.frame(pr))
    expect_true(nrow(pr) > 0)
  })
})
