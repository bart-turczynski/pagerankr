describe("validate_edge_weights", {
  it("reports valid per-source totals", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      p = c(0.25, 0.75, 1)
    )

    report <- validate_edge_weights(
      edges,
      weight_col = "p",
      expected_total = 1
    )

    expect_equal(report$source, c("A", "B"))
    expect_equal(report$total, c(1, 1))
    expect_true(all(report$total_ok))
    expect_true(all(report$valid))
  })

  it("reports negative, NA, NaN, and infinite weights separately", {
    edges <- data.frame(
      from = c("negative", "missing", "nan", "infinite"),
      to = rep("target", 4),
      weight = c(-1, NA_real_, NaN, Inf)
    )

    expect_error(
      validate_edge_weights(edges),
      "1 negative weight.*3 non-finite weight.*1 NA, 1 NaN, 1 Inf"
    )

    report <- validate_edge_weights(edges, action = "none")
    expect_equal(sum(report$n_negative), 1)
    expect_equal(sum(report$n_na), 1)
    expect_equal(sum(report$n_nan), 1)
    expect_equal(sum(report$n_infinite), 1)
    expect_false(any(report$valid))
  })

  it("identifies all-zero outgoing choice sets", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      weight = c(0, 0, 2)
    )

    expect_warning(
      report <- validate_edge_weights(edges, action = "warning"),
      "all-zero outgoing weights"
    )
    expect_true(report$all_zero[report$source == "A"])
    expect_false(report$valid[report$source == "A"])
    expect_true(report$valid[report$source == "B"])
  })

  it("makes probability-total checking opt-in and tolerance-aware", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      weight = c(0.2, 0.7, 1 + 1e-10)
    )

    raw_report <- validate_edge_weights(edges)
    expect_true(all(is.na(raw_report$total_ok)))
    expect_true(all(raw_report$valid))

    expect_error(
      validate_edge_weights(
        edges,
        expected_total = 1,
        tolerance = 1e-8
      ),
      "1 source total"
    )

    report <- validate_edge_weights(
      edges,
      expected_total = 1,
      tolerance = 0.11,
      action = "none"
    )
    expect_true(all(report$total_ok))
  })

  it("returns a stable empty report", {
    edges <- data.frame(
      from = character(0),
      weight = numeric(0)
    )
    report <- validate_edge_weights(edges)

    expect_equal(nrow(report), 0)
    expect_named(
      report,
      c(
        "source", "n_edges", "total", "n_negative", "n_na", "n_nan",
        "n_infinite", "all_zero", "total_ok", "valid"
      )
    )
  })

  it("validates its contract", {
    edges <- data.frame(from = "A", weight = 1)

    expect_error(validate_edge_weights(list()), "data frame")
    expect_error(
      validate_edge_weights(edges, weight_col = "missing"),
      "not found"
    )
    expect_error(
      validate_edge_weights(transform(edges, weight = "one")),
      "must be numeric"
    )
    expect_error(
      validate_edge_weights(edges, expected_total = -1),
      "non-negative"
    )
    expect_error(
      validate_edge_weights(edges, tolerance = Inf),
      "finite non-negative"
    )
  })

  it("validates weight_col as a single non-empty column name", {
    edges <- data.frame(from = "A", weight = 1)

    expect_error(
      validate_edge_weights(edges, weight_col = 123),
      "single non-empty column name"
    )
    expect_error(
      validate_edge_weights(edges, weight_col = c("weight", "w2")),
      "single non-empty column name"
    )
    expect_error(
      validate_edge_weights(edges, weight_col = NA_character_),
      "single non-empty column name"
    )
    expect_error(
      validate_edge_weights(edges, weight_col = ""),
      "single non-empty column name"
    )
  })

  it("validates expected_total as NULL or one finite non-negative number", {
    edges <- data.frame(from = "A", weight = 1)

    expect_error(
      validate_edge_weights(edges, expected_total = "bad"),
      "finite non-negative"
    )
    expect_error(
      validate_edge_weights(edges, expected_total = c(1, 2)),
      "finite non-negative"
    )
    expect_error(
      validate_edge_weights(edges, expected_total = NA_real_),
      "finite non-negative"
    )
    expect_error(
      validate_edge_weights(edges, expected_total = Inf),
      "finite non-negative"
    )
  })

  it("validates tolerance as one finite non-negative number", {
    edges <- data.frame(from = "A", weight = 1)

    expect_error(
      validate_edge_weights(edges, tolerance = "bad"),
      "finite non-negative"
    )
    expect_error(
      validate_edge_weights(edges, tolerance = c(0.1, 0.2)),
      "finite non-negative"
    )
    expect_error(
      validate_edge_weights(edges, tolerance = NA_real_),
      "finite non-negative"
    )
    expect_error(
      validate_edge_weights(edges, tolerance = -1),
      "finite non-negative"
    )
  })

  it("summarizes a source group when the source itself is NA", {
    edges <- data.frame(
      from = c(NA_character_, NA_character_, "B"),
      to = c("X", "Y", "Z"),
      weight = c(1, 1, 1)
    )
    report <- validate_edge_weights(edges)

    expect_true(anyNA(report$source))
    na_row <- report[is.na(report$source), ]
    expect_equal(na_row$n_edges, 2)
  })
})

describe("compute_pagerank edge-weight validation", {
  it("guards the solver from invalid weights by default", {
    negative <- data.frame(from = "A", to = "B", w = -1)
    non_finite <- data.frame(from = "A", to = "B", w = NA_real_)
    all_zero <- data.frame(from = c("A", "A"), to = c("B", "C"), w = 0)

    expect_error(
      compute_pagerank(negative, weight_col = "w"),
      "negative weight"
    )
    expect_error(
      compute_pagerank(non_finite, weight_col = "w"),
      "non-finite weight"
    )
    expect_error(
      compute_pagerank(all_zero, weight_col = "w"),
      "all-zero outgoing weights"
    )
  })

  it("can enforce per-source probability totals", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "A"),
      p = c(0.4, 0.4, 1)
    )

    expect_error(
      compute_pagerank(
        edges,
        weight_col = "p",
        weight_expected_total = 1
      ),
      "source total"
    )
  })
})
