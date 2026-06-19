# Tests for smooth_transitions()

# A sparse fixture: source A links (per crawl) to B, C, D, but only A->B was
# ever observed behaviorally. Source B links to C and D, both observed.
sparse_empirical <- function() {
  data.frame(
    from = c("A", "B", "B"),
    to = c("B", "C", "D"),
    n = c(8, 3, 1)
  )
}

sparse_structural <- function() {
  data.frame(
    from = c("A", "A", "A", "B", "B"),
    to = c("B", "C", "D", "C", "D")
  )
}

describe("smooth_transitions: structure", {
  it("returns the documented schema with one row per surviving edge", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural())
    expect_true(is.data.frame(out))
    expect_equal(
      names(out),
      c(
        "from", "to", "transition_probability", "empirical_count",
        "empirical_share", "structural_prior", "support", "lambda", "origin"
      )
    )
    # A's union of edges is {B, C, D}; B's is {C, D}: 5 edges total.
    expect_equal(nrow(out), 5L)
  })

  it("honors custom column names", {
    emp <- sparse_empirical()
    names(emp) <- c("src", "dst", "clicks")
    struct <- sparse_structural()
    names(struct) <- c("src", "dst")
    out <- smooth_transitions(
      emp, struct,
      count_col = "clicks", from_col = "src", to_col = "dst", prob_col = "p"
    )
    expect_true(all(c("src", "dst", "p") %in% names(out)))
  })

  it("is ordered by from then to", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural())
    expect_equal(out$from, out$from[order(out$from, out$to)])
  })
})

describe("smooth_transitions: acceptance - no crawled link gets zero", {
  it("assigns non-zero probability to an unobserved crawled link", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural(), k = 5)
    # A->C and A->D were never observed but are crawled links.
    ac <- out[out$from == "A" & out$to == "C", ]
    ad <- out[out$from == "A" & out$to == "D", ]
    expect_equal(ac$origin, "structural_only")
    expect_equal(ad$origin, "structural_only")
    expect_gt(ac$transition_probability, 0)
    expect_gt(ad$transition_probability, 0)
  })

  it("keeps every crawled link strictly positive across all sources", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural(), k = 5)
    crawled <- out[out$origin %in% c("both", "structural_only"), ]
    expect_true(all(crawled$transition_probability > 0))
  })

  it("holds even with a very large k (strong shrinkage to prior)", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural(),
      k = 1e6
    )
    crawled <- out[out$origin %in% c("both", "structural_only"), ]
    expect_true(all(crawled$transition_probability > 0))
  })
})

describe("smooth_transitions: probabilities sum to 1 per source", {
  it("each source's smoothed probabilities sum to 1", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural())
    sums <- tapply(out$transition_probability, out$from, sum)
    expect_true(all(abs(sums - 1) < 1e-9))
  })
})

describe("smooth_transitions: lambda rule", {
  it("lambda_i = n_i / (n_i + k)", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural(), k = 5)
    # Source A: n = 8 -> 8/13; Source B: n = 4 -> 4/9.
    lam_a <- unique(out$lambda[out$from == "A"])
    lam_b <- unique(out$lambda[out$from == "B"])
    expect_equal(lam_a, 8 / 13)
    expect_equal(lam_b, 4 / 9)
  })

  it("lambda increases monotonically with sample size", {
    base_struct <- data.frame(
      from = c("X", "X"), to = c("Y", "Z")
    )
    lambdas <- vapply(c(1, 5, 20, 100), function(nn) {
      emp <- data.frame(from = "X", to = "Y", n = nn)
      out <- smooth_transitions(emp, base_struct, k = 5)
      unique(out$lambda)
    }, numeric(1))
    expect_true(all(diff(lambdas) > 0))
  })

  it("blends empirical and prior in the documented proportion", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural(), k = 5)
    # A->B: lambda=8/13, emp_share=1 (8/8), prior=1/3 (uniform over B,C,D).
    ab <- out[out$from == "A" & out$to == "B", ]
    expected <- (8 / 13) * 1 + (5 / 13) * (1 / 3)
    expect_equal(ab$transition_probability, expected)
  })
})

describe("smooth_transitions: per-source special cases", {
  it("uses the pure structural prior when a source has no empirical data", {
    # C is crawled (C->A, C->B) but never observed.
    emp <- data.frame(from = "A", to = "B", n = 5)
    struct <- data.frame(
      from = c("A", "C", "C"), to = c("B", "A", "B")
    )
    out <- smooth_transitions(emp, struct, k = 5)
    c_rows <- out[out$from == "C", ]
    expect_equal(unique(c_rows$lambda), 0)
    # Uniform prior over C's two crawled links.
    expect_equal(sort(c_rows$transition_probability), c(0.5, 0.5))
    expect_true(all(c_rows$origin == "structural_only"))
  })

  it("uses the pure empirical dist when a source has no structural prior", {
    # A has empirical out-edges but no crawled out-links.
    emp <- data.frame(
      from = c("A", "A"), to = c("B", "C"), n = c(3, 1)
    )
    struct <- data.frame(from = "Z", to = "Y")
    out <- smooth_transitions(emp, struct, k = 5)
    a_rows <- out[out$from == "A", ]
    expect_equal(unique(a_rows$lambda), 1)
    expect_equal(
      a_rows$transition_probability[a_rows$to == "B"], 0.75
    )
    expect_true(all(a_rows$origin == "empirical_only"))
  })

  it("falls back to the prior below min_support", {
    emp <- data.frame(from = "A", to = "B", n = 2)
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C")
    )
    out <- smooth_transitions(emp, struct, k = 5, min_support = 3)
    a_rows <- out[out$from == "A", ]
    expect_equal(unique(a_rows$lambda), 0)
    # Pure uniform prior over B and C.
    expect_equal(sort(a_rows$transition_probability), c(0.5, 0.5))
  })

  it("drops empirical_only edges that fall to zero below min_support", {
    # A->D observed but not crawled; below support so lambda=0 -> P(A->D)=0.
    emp <- data.frame(
      from = c("A", "A"), to = c("B", "D"), n = c(1, 1)
    )
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C")
    )
    out <- smooth_transitions(emp, struct, k = 5, min_support = 5)
    expect_equal(nrow(out[out$from == "A" & out$to == "D", ]), 0L)
    # The crawled links survive.
    expect_setequal(out$to[out$from == "A"], c("B", "C"))
  })
})

describe("smooth_transitions: origin classification", {
  it("labels edges both / empirical_only / structural_only", {
    emp <- data.frame(
      from = c("A", "A"), to = c("B", "E"), n = c(4, 2)
    )
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C")
    )
    out <- smooth_transitions(emp, struct, k = 5)
    o <- setNames(out$origin, out$to)
    expect_equal(unname(o["B"]), "both")
    expect_equal(unname(o["E"]), "empirical_only")
    expect_equal(unname(o["C"]), "structural_only")
  })
})

describe("smooth_transitions: structural weights", {
  it("uses a weighted (non-uniform) prior when given a weight column", {
    emp <- data.frame(
      from = character(0), to = character(0), n = numeric(0)
    )
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C"), w = c(3, 1)
    )
    out <- smooth_transitions(emp, struct, structural_weight_col = "w")
    # No empirical data -> pure (weighted) prior: 3/4 and 1/4.
    expect_equal(out$transition_probability[out$to == "B"], 0.75)
    expect_equal(out$transition_probability[out$to == "C"], 0.25)
  })

  it("sums duplicate structural rows into the prior weight", {
    emp <- data.frame(from = "A", to = "B", n = 0)
    # A->B appears twice (multiplicity 2), A->C once.
    struct <- data.frame(
      from = c("A", "A", "A"), to = c("B", "B", "C")
    )
    out <- smooth_transitions(emp, struct, k = 5)
    expect_equal(out$transition_probability[out$to == "B"], 2 / 3)
    expect_equal(out$transition_probability[out$to == "C"], 1 / 3)
  })

  it("drops non-positive structural weights", {
    emp <- data.frame(
      from = character(0), to = character(0), n = numeric(0)
    )
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C"), w = c(2, 0)
    )
    out <- smooth_transitions(emp, struct, structural_weight_col = "w")
    expect_equal(nrow(out), 1L)
    expect_equal(out$to, "B")
  })
})

describe("smooth_transitions: lambda_fn override", {
  it("applies a custom lambda function of sample size", {
    emp <- data.frame(from = "A", to = "B", n = 10)
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C")
    )
    out <- smooth_transitions(emp, struct, lambda_fn = function(n) 0.5)
    expect_equal(unique(out$lambda), 0.5)
    # A->B: 0.5*1 + 0.5*0.5 = 0.75; A->C: 0.5*0 + 0.5*0.5 = 0.25.
    expect_equal(out$transition_probability[out$to == "B"], 0.75)
    expect_equal(out$transition_probability[out$to == "C"], 0.25)
  })

  it("rejects a lambda_fn returning out-of-range values", {
    emp <- data.frame(from = "A", to = "B", n = 5)
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C")
    )
    expect_error(
      smooth_transitions(emp, struct, lambda_fn = function(n) 2),
      "in \\[0, 1\\]"
    )
  })
})

describe("smooth_transitions: time-decay-friendly fractional counts", {
  it("accepts non-integer (decayed) counts", {
    emp <- data.frame(
      from = c("A", "A"), to = c("B", "C"), n = c(2.5, 0.5)
    )
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C")
    )
    out <- smooth_transitions(emp, struct, k = 5)
    expect_equal(unique(out$support[out$from == "A"]), 3)
    expect_equal(unique(out$lambda[out$from == "A"]), 3 / 8)
  })
})

describe("smooth_transitions: additional validation", {
  it("errors on a non-data-frame structural_df", {
    expect_error(
      smooth_transitions(sparse_empirical(), list()),
      "must be a data frame"
    )
  })

  it("errors on a non-character from_col", {
    expect_error(
      smooth_transitions(sparse_empirical(), sparse_structural(), from_col = 1),
      "single non-NA character string"
    )
  })

  it("errors on a negative min_support", {
    expect_error(
      smooth_transitions(sparse_empirical(), sparse_structural(), min_support = -1),
      "non-negative"
    )
  })

  it("errors when lambda_fn is not a function", {
    expect_error(
      smooth_transitions(sparse_empirical(), sparse_structural(), lambda_fn = "bad"),
      "function"
    )
  })

  it("errors on a non-character structural_weight_col", {
    expect_error(
      smooth_transitions(sparse_empirical(), sparse_structural(),
        structural_weight_col = 1),
      "character string"
    )
  })

  it("errors when count_col is not numeric", {
    emp_char_count <- data.frame(from = "A", to = "B", n = "text")
    expect_error(
      smooth_transitions(emp_char_count, sparse_structural()),
      "must be numeric"
    )
  })

  it("errors when structural_df is missing required columns", {
    struct_no_cols <- data.frame(bad = "A")
    expect_error(
      smooth_transitions(sparse_empirical(), struct_no_cols),
      "missing required column"
    )
  })

  it("errors when structural_weight_col is not numeric", {
    struct_char_wt <- data.frame(
      from = c("A", "A"), to = c("B", "C"), w = c("heavy", "light")
    )
    expect_error(
      smooth_transitions(sparse_empirical(), struct_char_wt,
        structural_weight_col = "w"),
      "must be numeric"
    )
  })

  it("handles empirical input where all rows have NA endpoints", {
    emp_all_na <- data.frame(from = NA_character_, to = NA_character_, n = 1)
    out <- smooth_transitions(emp_all_na, sparse_structural())
    expect_true(is.data.frame(out))
    expect_true(nrow(out) > 0)
  })
})

describe("smooth_transitions: edge cases and validation", {
  it("returns an empty typed frame when both inputs are empty", {
    emp <- data.frame(
      from = character(0), to = character(0), n = numeric(0)
    )
    struct <- data.frame(
      from = character(0), to = character(0)
    )
    out <- smooth_transitions(emp, struct)
    expect_equal(nrow(out), 0L)
    expect_true("transition_probability" %in% names(out))
  })

  it("drops rows with NA endpoints before smoothing", {
    emp <- data.frame(
      from = c("A", NA), to = c("B", "C"), n = c(2, 9)
    )
    struct <- data.frame(
      from = c("A", "A"), to = c("B", "C")
    )
    out <- smooth_transitions(emp, struct, k = 5)
    expect_false(anyNA(out$from))
    # The NA-from empirical row is ignored; A's support is just 2.
    expect_equal(unique(out$support[out$from == "A"]), 2)
  })

  it("rejects a non-positive k", {
    expect_error(
      smooth_transitions(sparse_empirical(), sparse_structural(), k = 0),
      "positive"
    )
  })

  it("errors on a missing count column", {
    emp <- data.frame(from = "A", to = "B")
    expect_error(
      smooth_transitions(emp, sparse_structural()),
      "missing required column"
    )
  })

  it("errors on a non-data-frame input", {
    expect_error(
      smooth_transitions(list(), sparse_structural()),
      "must be a data frame"
    )
  })

  it("produces a pagerank-ready probability column", {
    out <- smooth_transitions(sparse_empirical(), sparse_structural())
    pr <- pagerank(
      out,
      weight_col = "transition_probability", clean_edge_urls = FALSE
    )
    expect_true(is.data.frame(pr))
    expect_true(all(c("node_name", "pagerank") %in% names(pr)))
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
  })
})
