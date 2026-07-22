# Tests for the transition_audit / provenance object attached by pagerank()

describe("transition_audit structure", {
  it("is attached to the pagerank() result and is the documented S3 class", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    res <- pagerank(edges, clean_edge_urls = FALSE)

    # Backward-compatible: result is still the same data frame.
    expect_s3_class(res, "data.frame")
    expect_true(all(c("node_name", "pagerank") %in% names(res)))

    audit <- attr(res, "transition_audit")
    expect_s3_class(audit, "transition_audit")
    expect_type(audit, "list")
  })

  it("exposes the documented top-level fields", {
    edges <- data.frame(from = "A", to = "B")
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")

    expect_setequal(
      names(audit),
      c(
        "counts", "coverage", "normalization", "dropped", "duplicates",
        "config", "mass", "fold"
      )
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
        "n_rows_collapsed", "n_prior_unmatched", "n_robots_blocked",
        "n_status_dead"
      )
    )
    expect_setequal(
      names(audit$duplicates),
      c(
        "policy", "n_duplicate_rows", "instance_count_col",
        "n_duplicate_instances", "duplicate_edges"
      )
    )
    expect_setequal(
      names(audit$mass),
      c("reported", "sink", "leaked", "hidden", "total")
    )
    expect_setequal(
      names(audit$fold),
      c("policy", "n_out_of_scope", "applied", "out_of_scope", "collisions")
    )
  })

  it("populates the mass-accounting fields (reported/sink/hidden/total)", {
    # B2 fills the reserved mass$ keys: with no evaporation or vanish, all the
    # stationary mass is reported and the total reconciles to 1.
    edges <- data.frame(from = "A", to = "B")
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_equal(audit$mass$reported, 1, tolerance = 1e-8)
    expect_equal(audit$mass$sink, 0)
    expect_equal(audit$mass$leaked, 0)
    expect_equal(audit$mass$hidden, 0)
    expect_equal(audit$mass$total, 1, tolerance = 1e-8)
  })

  it("has a print method that returns its input invisibly", {
    edges <- data.frame(from = "A", to = "B")
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_output(print(audit), "Transition Construction Audit")
    expect_invisible(print(audit))
  })
})

describe("transition_audit counts and dropped accounting", {
  it("counts input rows, scored edges and result vertices", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
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
      to = c("B", "B", "C")
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
      to = c("B", "B", "C")
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
      to = c("B", "C", "D")
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
    edges <- data.frame(from = "A", to = "B")
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_false(audit$coverage$weighted)
    expect_null(audit$coverage$weight_col)
    expect_true(is.na(audit$coverage$coverage))
  })

  it("reports behavioral-weight coverage when a weight_col is supplied", {
    edges <- data.frame(
      from = c("A", "B", "B", "C"),
      to = c("B", "C", "A", "A"),
      w = c(2, 0, 1, 5)
    )
    audit <- attr(
      pagerank(edges, clean_edge_urls = FALSE, weight_col = "w"),
      "transition_audit"
    )
    expect_true(audit$coverage$weighted)
    expect_equal(audit$coverage$weight_col, "w")
    # Three of four edges carry a finite positive weight. Source B's choice set
    # still has positive total weight despite containing one zero-weight edge.
    expect_equal(audit$coverage$n_edges_weighted, 3L)
    expect_equal(audit$coverage$coverage, 3 / 4)
  })

  it("records the PageRank normalization total", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A")
    )
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_equal(audit$normalization$pagerank_total, 1, tolerance = 1e-9)
  })
})

describe("transition_audit configuration capture", {
  it("records the relevant pagerank() arguments", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A")
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
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(from = "B", to = "C")
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
    edges <- data.frame(from = "A", to = "B")
    audit <- attr(pagerank(edges, clean_edge_urls = FALSE), "transition_audit")
    expect_true(is.na(audit$dropped$n_prior_unmatched))
    expect_false(audit$config$has_prior)
  })

  it("counts prior URLs that do not fold onto any vertex", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A")
    )
    prior <- data.frame(
      url = c("A", "Z"),
      weight = c(10, 5)
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

describe("transition_audit print method branches", {
  it("prints prior unmatched count when prior was supplied", {
    edges <- data.frame(from = "A", to = "B")
    prior <- data.frame(url = c("A", "Z"), weight = c(10, 5))
    audit <- attr(
      pagerank(
        edges,
        clean_edge_urls = FALSE, prior_df = prior, prior_verbose = FALSE
      ),
      "transition_audit"
    )
    expect_output(print(audit), "Prior URLs unmatched")
  })

  it("prints robots-blocked URL count when n_robots_blocked > 0", {
    audit <- pagerankr::new_transition_audit(
      n_robots_blocked = 2L,
      mass_reported = 0.8, mass_hidden = 0.2,
      pagerank_total = 0.8
    )
    expect_output(print(audit), "Robots-blocked URLs")
  })

  it("prints weighted coverage details when weight_col is supplied", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "C"), w = c(1, 2)
    )
    audit <- attr(
      pagerank(edges, clean_edge_urls = FALSE, weight_col = "w"),
      "transition_audit"
    )
    expect_output(print(audit), "Weight column")
    expect_output(print(audit), "Coverage")
  })

  it("prints NA coverage when weighted but zero edges", {
    audit <- pagerankr::new_transition_audit(
      weighted = TRUE, weight_col = "w", n_edges = 0L
    )
    out <- capture.output(print(audit))
    expect_true(any(grepl("NA", out, fixed = TRUE)))
  })

  it("prints NA pagerank total for an empty audit", {
    audit <- pagerankr::new_transition_audit()
    expect_output(print(audit), "NA")
  })

  it("prints instance count col and counted dup edges under count_instances", {
    edges <- data.frame(from = c("A", "A"), to = c("B", "B"))
    audit <- attr(
      pagerank(
        edges,
        clean_edge_urls = FALSE,
        duplicate_edge_policy = "count_instances"
      ),
      "transition_audit"
    )
    expect_output(print(audit), "Instance count col")
    expect_output(print(audit), "Counted dup edges")
  })
})

describe("transition_audit print helpers for degenerate structures", {
  it(".print_ta_duplicates is silent when duplicates is NULL", {
    expect_silent(.print_ta_duplicates(list(duplicates = NULL)))
  })

  it(".print_ta_fold is silent when fold is NULL", {
    expect_silent(.print_ta_fold(list(fold = NULL)))
  })

  it("is silent on fold-target collisions with zero rows", {
    audit <- pagerankr::new_transition_audit(
      fold_collisions = data.frame(
        target = character(0), n_independent_refs = integer(0),
        source = character(0)
      )
    )
    out <- paste(capture.output(print(audit)), collapse = "\n")
    expect_false(grepl("Fold-target collisions", out, fixed = TRUE))
  })

  it("prints fold-target collision rows when present", {
    audit <- pagerankr::new_transition_audit(
      fold_collisions = data.frame(
        target = "https://example.com/z",
        n_independent_refs = 2L,
        source = "https://example.com/a"
      )
    )
    expect_output(print(audit), "Fold-target collisions")
    expect_output(print(audit), "independent ref")
  })

  it("prints the leak action when out-of-scope folds route to the leak sink", {
    audit <- pagerankr::new_transition_audit(
      out_of_scope_fold = "leak",
      n_out_of_scope_folds = 1L,
      out_of_scope_folds_applied = TRUE
    )
    expect_output(print(audit), "routed to leak sink")
  })

  it("prints the relabeled action when out-of-scope folds were applied", {
    audit <- pagerankr::new_transition_audit(
      out_of_scope_fold = "relabel",
      n_out_of_scope_folds = 1L,
      out_of_scope_folds_applied = TRUE
    )
    expect_output(print(audit), "relabeled (applied)", fixed = TRUE)
  })

  it("prints the skipped action when out-of-scope folds were kept", {
    audit <- pagerankr::new_transition_audit(
      out_of_scope_fold = "keep",
      n_out_of_scope_folds = 1L,
      out_of_scope_folds_applied = FALSE
    )
    expect_output(print(audit), "skipped (kept)", fixed = TRUE)
  })
})
