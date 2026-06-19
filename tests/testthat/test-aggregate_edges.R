context("aggregate_edges")

describe("aggregate_edges backward compatibility (unweighted)", {
  it("matches get_unique_edges for a plain from/to edge list (drop)", {
    edges <- data.frame(
      from = c("A", "B", "A", "C"),
      to = c("B", "C", "B", "C")
    )
    agg <- aggregate_edges(edges, self_loops = "drop")
    uniq <- get_unique_edges(edges, self_loops = "drop")
    expect_equal(
      agg[order(agg$from, agg$to), ],
      uniq[order(uniq$from, uniq$to), ],
      check.attributes = FALSE
    )
  })

  it("handles self-loops drop/keep like get_unique_edges", {
    edges <- data.frame(
      from = c("A", "B", "B", "C", "D"),
      to = c("B", "B", "C", "D", "D")
    )
    drop_res <- aggregate_edges(edges, self_loops = "drop")
    keep_res <- aggregate_edges(edges, self_loops = "keep")
    expect_false(any(drop_res$from == drop_res$to))
    expect_true(any(keep_res$from == "B" & keep_res$to == "B"))
    expect_equal(nrow(keep_res), 5)
  })

  it("drops NA edges and coerces factors to character", {
    edges <- data.frame(
      from = factor(c("A", NA, "A")),
      to = factor(c("B", "C", "B"))
    )
    res <- aggregate_edges(edges, self_loops = "keep")
    expect_equal(nrow(res), 1)
    expect_true(is.character(res$from))
    expect_true(is.character(res$to))
  })

  it("handles empty data frames", {
    df_empty <- data.frame(
      from = character(0), to = character(0)
    )
    res <- aggregate_edges(df_empty)
    expect_equal(nrow(res), 0)
    expect_equal(names(res), c("from", "to"))

    bare <- data.frame()
    res2 <- aggregate_edges(bare, from_col = "src", to_col = "dst")
    expect_equal(nrow(res2), 0)
    expect_true(all(c("src", "dst") %in% names(res2)))
  })

  it("works with custom column names", {
    edges <- data.frame(
      source = c("X", "Y", "X"),
      target = c("Y", "Y", "Y"),
      clicks = c(1, 2, 4)
    )
    res <- aggregate_edges(
      edges, from_col = "source", to_col = "target"
    )
    xy <- res[res$source == "X" & res$target == "Y", ]
    expect_equal(xy$clicks, 5)
  })
})

describe("aggregate_edges numeric defaults sum", {
  it("sums additive count columns across folded variants", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "B", "C"),
      clicks = c(3, 5, 2)
    )
    res <- aggregate_edges(edges)
    expect_equal(res$clicks[res$from == "A"], 8)
    expect_equal(res$clicks[res$from == "B"], 2)
  })

  it("sums repeated link instances' propensities to one destination", {
    edges <- data.frame(
      from = rep("A", 3),
      to = rep("B", 3),
      propensity = c(0.1, 0.2, 0.4)
    )
    res <- aggregate_edges(edges)
    expect_equal(nrow(res), 1)
    expect_equal(res$propensity, 0.7)
  })
})

describe("aggregate_edges round-trip mass conservation", {
  it("conserves total click counts (no silent loss)", {
    set.seed(1)
    n <- 200
    edges <- data.frame(
      from = sample(c("A", "B", "C"), n, replace = TRUE),
      to = sample(c("X", "Y", "Z"), n, replace = TRUE),
      clicks = sample(1:10, n, replace = TRUE)
    )
    # Keep only non-self-loops so totals are comparable.
    edges <- edges[edges$from != edges$to, ]
    res <- aggregate_edges(edges, self_loops = "keep")
    expect_equal(sum(res$clicks), sum(edges$clicks))
    # And no row was actually dropped by collapsing.
    expect_equal(nrow(res), nrow(unique(edges[, c("from", "to")])))
  })
})

describe("aggregate_edges nofollow conflict policy", {
  make <- function(vals) {
    data.frame(
      from = rep("A", length(vals)),
      to = rep("B", length(vals)),
      nofollow = vals
    )
  }

  it("policy any: TRUE if any TRUE", {
    res <- aggregate_edges(make(c(FALSE, TRUE, FALSE)),
      nofollow_policy = "any"
    )
    expect_true(res$nofollow)
    res2 <- aggregate_edges(make(c(FALSE, FALSE)), nofollow_policy = "any")
    expect_false(res2$nofollow)
  })

  it("policy all: TRUE only if all TRUE", {
    res <- aggregate_edges(make(c(TRUE, TRUE)), nofollow_policy = "all")
    expect_true(res$nofollow)
    res2 <- aggregate_edges(make(c(TRUE, FALSE)), nofollow_policy = "all")
    expect_false(res2$nofollow)
  })

  it("policy majority: majority wins, ties -> TRUE", {
    res <- aggregate_edges(make(c(TRUE, TRUE, FALSE)),
      nofollow_policy = "majority"
    )
    expect_true(res$nofollow)
    res2 <- aggregate_edges(make(c(TRUE, FALSE, FALSE)),
      nofollow_policy = "majority"
    )
    expect_false(res2$nofollow)
    res_tie <- aggregate_edges(make(c(TRUE, FALSE)),
      nofollow_policy = "majority"
    )
    expect_true(res_tie$nofollow)
  })

  it("policy error: errors on conflicting values, ok when uniform", {
    expect_error(
      aggregate_edges(make(c(TRUE, FALSE)), nofollow_policy = "error"),
      "Conflicting"
    )
    res <- aggregate_edges(make(c(TRUE, TRUE)), nofollow_policy = "error")
    expect_true(res$nofollow)
    res2 <- aggregate_edges(make(c(FALSE, FALSE)), nofollow_policy = "error")
    expect_false(res2$nofollow)
  })

  it("policy can be set per-column via agg, overriding default", {
    edges <- data.frame(
      from = rep("A", 3),
      to = rep("B", 3),
      nofollow = c(TRUE, TRUE, FALSE),
      sponsored = c(TRUE, FALSE, FALSE)
    )
    res <- aggregate_edges(
      edges,
      nofollow_policy = "any",
      agg = list(sponsored = "all")
    )
    expect_true(res$nofollow) # any
    expect_false(res$sponsored) # all
  })
})

describe("aggregate_edges configurable per-column aggregation", {
  it("accepts built-in string rules", {
    edges <- data.frame(
      from = rep("A", 3),
      to = rep("B", 3),
      v_sum = c(1, 2, 3),
      v_max = c(1, 9, 3),
      v_first = c(10, 20, 30)
    )
    res <- aggregate_edges(
      edges,
      agg = list(v_max = "max", v_first = "first")
    )
    expect_equal(res$v_sum, 6) # default numeric -> sum
    expect_equal(res$v_max, 9)
    expect_equal(res$v_first, 10)
  })

  it("accepts a custom function", {
    edges <- data.frame(
      from = rep("A", 3),
      to = rep("B", 3),
      v = c(1, 2, 3)
    )
    res <- aggregate_edges(
      edges,
      agg = list(v = function(x) paste(x, collapse = "-"))
    )
    expect_equal(res$v, "1-2-3")
  })

  it("character columns default to first (legacy behavior)", {
    edges <- data.frame(
      from = rep("A", 2),
      to = rep("B", 2),
      anchor = c("home", "homepage")
    )
    res <- aggregate_edges(edges)
    expect_equal(res$anchor, "home")
  })

  it("errors on unknown agg column or rule", {
    edges <- data.frame(
      from = "A", to = "B", v = 1
    )
    expect_error(aggregate_edges(edges, agg = list(nope = "sum")), "not found")
    expect_error(aggregate_edges(edges, agg = list(v = "bogus")), "Unknown")
  })

  it("preserves original column order", {
    edges <- data.frame(
      from = c("A", "A"),
      to = c("B", "B"),
      clicks = c(1, 2),
      nofollow = c(FALSE, TRUE)
    )
    res <- aggregate_edges(edges)
    expect_equal(names(res), c("from", "to", "clicks", "nofollow"))
  })
})

describe("aggregate_edges preserve_cols placement features", {
  it("keeps individual instances as a list-column", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "B", "C"),
      position = c(1, 7, 3)
    )
    res <- aggregate_edges(edges, preserve_cols = "position")
    expect_true(is.list(res$position))
    ab <- res$position[[which(res$from == "A" & res$to == "B")]]
    expect_equal(sort(ab), c(1, 7))
    bc <- res$position[[which(res$from == "B" & res$to == "C")]]
    expect_equal(bc, 3)
  })

  it("preserves while still collapsing other columns", {
    edges <- data.frame(
      from = c("A", "A"),
      to = c("B", "B"),
      clicks = c(2, 5),
      position = c(1, 9)
    )
    res <- aggregate_edges(edges, preserve_cols = "position")
    expect_equal(res$clicks, 7)
    expect_equal(sort(res$position[[1]]), c(1, 9))
  })

  it("rejects a column in both agg and preserve_cols", {
    edges <- data.frame(
      from = "A", to = "B", v = 1
    )
    expect_error(
      aggregate_edges(edges, agg = list(v = "sum"), preserve_cols = "v"),
      "both"
    )
  })

  it("errors on unknown preserve column", {
    edges <- data.frame(
      from = "A", to = "B", v = 1
    )
    expect_error(aggregate_edges(edges, preserve_cols = "nope"), "not found")
  })
})

describe("aggregate_edges input validation", {
  it("errors on non-data-frame input", {
    expect_error(aggregate_edges(list()), "data frame")
  })

  it("errors on missing from/to columns", {
    df <- data.frame(fcol = "a", tcol = "b")
    expect_error(aggregate_edges(df))
  })

  it("errors on non-named agg list", {
    edges <- data.frame(
      from = "A", to = "B", v = 1
    )
    expect_error(aggregate_edges(edges, agg = list("sum")), "named list")
  })
})
