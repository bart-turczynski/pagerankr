# Tests for the transition_audit / provenance object attached by pagerank()

describe("transition_audit structure", {
  it("is attached to the pagerank() result and is the documented S3 class", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      stringsAsFactors = FALSE
    )
    res <- pagerank(edges, clean_edge_urls = FALSE)

    # Backward-compatible: result is still the same data frame.
    expect_true(is.data.frame(res))
    expect_true(all(c("node_name", "pagerank") %in% names(res)))

    audit <- attr(res, "transition_audit")
    expect_s3_class(audit, "transition_audit")
    expect_type(audit, "list")
  })

  it("exposes the documented top-level fields", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")

    expect_setequal(
      names(audit),
      c("counts", "coverage", "normalization", "dropped", "config", "mass")
    )
    expect_setequal(
      names(audit$counts),
      c("n_input_rows", "n_edges", "n_vertices")
    )
    expect_setequal(
      names(audit$coverage),
      c("weighted", "weight_col", "n_edges_weighted", "coverage")
    )
    expect_setequal(names(audit$normalization), "pagerank_total")
    expect_setequal(
      names(audit$dropped),
      c(
        "n_rows_na", "n_rows_duplicate", "n_self_loops",
        "n_rows_collapsed", "n_prior_unmatched", "n_robots_blocked"
      )
    )
    expect_setequal(
      names(audit$mass),
      c("reported", "sink", "hidden", "total")
    )
  })

  it("stubs the mass-accounting fields as NULL (B2 territory)", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_null(audit$mass$reported)
    expect_null(audit$mass$sink)
    expect_null(audit$mass$hidden)
    expect_null(audit$mass$total)
  })

  it("has a print method that returns its input invisibly", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_output(print(audit), "Transition Construction Audit")
    expect_invisible(print(audit))
  })
})

describe("transition_audit counts and dropped accounting", {
  it("counts input rows, scored edges and result vertices", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      stringsAsFactors = FALSE
    )
    audit <- attr(
      pagerank(edges, clean_edge_urls = FALSE, drop_isolates_flag = TRUE),
      "transition_audit"
    )
    expect_equal(audit$counts$n_input_rows, 3L)
    expect_equal(audit$counts$n_edges, 3L)
    expect_equal(audit$counts$n_vertices, 3L)
  })

  it("accounts for duplicate rows collapsed by dedup", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "B", "C"),
      stringsAsFactors = FALSE
    )
    audit <- attr(
      pagerank(edges, clean_edge_urls = FALSE),
      "transition_audit"
    )
    expect_equal(audit$counts$n_input_rows, 3L)
    expect_equal(audit$dropped$n_rows_duplicate, 1L)
    expect_equal(audit$counts$n_edges, 2L)
    expect_equal(audit$dropped$n_rows_collapsed, 1L)
  })

  it("accounts for self-loops dropped under self_loops = 'drop'", {
    edges <- data.frame(
      from = c("A", "B", "B"),
      to = c("B", "B", "C"),
      stringsAsFactors = FALSE
    )
    audit <- attr(
      pagerank(edges, clean_edge_urls = FALSE, self_loops = "drop"),
      "transition_audit"
    )
    expect_equal(audit$dropped$n_self_loops, 1L)
  })

  it("accounts for rows dropped because an endpoint is NA", {
    edges <- data.frame(
      from = c("A", "B", NA),
      to = c("B", "C", "D"),
      stringsAsFactors = FALSE
    )
    audit <- attr(
      pagerank(edges, clean_edge_urls = FALSE),
      "transition_audit"
    )
    expect_equal(audit$dropped$n_rows_na, 1L)
  })
})

describe("transition_audit coverage and normalization", {
  it("reports unweighted by default", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_false(audit$coverage$weighted)
    expect_null(audit$coverage$weight_col)
    expect_true(is.na(audit$coverage$coverage))
  })

  it("reports behavioral-weight coverage when a weight_col is supplied", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      w = c(2, 0, 5),
      stringsAsFactors = FALSE
    )
    audit <- attr(
      pagerank(edges, clean_edge_urls = FALSE, weight_col = "w"),
      "transition_audit"
    )
    expect_true(audit$coverage$weighted)
    expect_equal(audit$coverage$weight_col, "w")
    # Two of three edges carry a finite positive weight.
    expect_equal(audit$coverage$n_edges_weighted, 2L)
    expect_equal(audit$coverage$coverage, 2 / 3)
  })

  it("records the PageRank normalization total", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      stringsAsFactors = FALSE
    )
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_equal(audit$normalization$pagerank_total, 1, tolerance = 1e-9)
  })
})

describe("transition_audit configuration capture", {
  it("records the relevant pagerank() arguments", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      stringsAsFactors = FALSE
    )
    audit <- attr(
      pagerank(
        edges,
        clean_edge_urls = FALSE,
        self_loops = "keep",
        drop_isolates_flag = FALSE,
        reverse = TRUE
      ),
      "transition_audit"
    )
    expect_equal(audit$config$self_loops, "keep")
    expect_false(audit$config$drop_isolates_flag)
    expect_true(audit$config$reverse)
    expect_false(audit$config$has_redirects)
    expect_false(audit$config$has_prior)
  })

  it("flags redirect usage in config", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(from = "B", to = "C", stringsAsFactors = FALSE)
    audit <- attr(
      pagerank(edges,
        redirects_df = redirects, clean_edge_urls = FALSE,
        clean_redirect_urls = FALSE
      ),
      "transition_audit"
    )
    expect_true(audit$config$has_redirects)
  })
})

describe("transition_audit prior accounting", {
  it("reports NA unmatched-prior count when no prior is supplied", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_true(is.na(audit$dropped$n_prior_unmatched))
    expect_false(audit$config$has_prior)
  })

  it("counts prior URLs that do not fold onto any vertex", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      stringsAsFactors = FALSE
    )
    prior <- data.frame(
      url = c("A", "Z"),
      weight = c(10, 5),
      stringsAsFactors = FALSE
    )
    audit <- attr(
      pagerank(
        edges,
        clean_edge_urls = FALSE,
        prior_df = prior,
        prior_verbose = FALSE
      ),
      "transition_audit"
    )
    expect_true(audit$config$has_prior)
    # "Z" is not a vertex; "A" is. One unmatched.
    expect_equal(audit$dropped$n_prior_unmatched, 1L)
  })
})
