context("coverage gaps: error / validation / print / defensive branches")

# ---------------------------------------------------------------------------
# screaming_frog_contract.R
# ---------------------------------------------------------------------------

describe("sf_read_input field and input validation", {
  it("rejects `fields` containing NA", {
    df <- data.frame(
      Address = "https://x/", `Status Code` = "200", check.names = FALSE
    )
    expect_error(
      pagerankr::sf_read_input(df, "internal_all", fields = c("address", NA)),
      "normalized fields",
      fixed = TRUE
    )
  })

  it("rejects `fields` not in the contract", {
    df <- data.frame(
      Address = "https://x/", `Status Code` = "200", check.names = FALSE
    )
    expect_error(
      pagerankr::sf_read_input(df, "internal_all", fields = "not_a_field"),
      "normalized fields",
      fixed = TRUE
    )
  })

  it("errors when a file path does not exist", {
    expect_error(
      pagerankr::sf_read_input(
        "/no/such/screaming-frog-file.csv", "internal_all"
      ),
      "does not exist",
      fixed = TRUE
    )
  })

  it("errors when `x` is neither a data frame nor a single file path", {
    expect_error(
      pagerankr::sf_read_input(123, "internal_all"),
      "must be a data frame or a single file path",
      fixed = TRUE
    )
  })

  it("coerces factor columns to character in the output", {
    df <- data.frame(
      Address = factor(c("https://x/", "https://y/")),
      `Status Code` = factor(c("200", "404")),
      check.names = FALSE
    )
    out <- pagerankr::sf_read_input(
      df, "internal_all", fields = c("address", "status_code")
    )
    expect_type(out$address, "character")
    expect_equal(out$address, c("https://x/", "https://y/"))
  })
})

# ---------------------------------------------------------------------------
# pagerank_convergence.R : print method
# ---------------------------------------------------------------------------

describe("print.pagerank_convergence branches", {
  it("prints 'NA' for a missing residual", {
    conv <- new_pagerank_convergence(residual = NA_real_)
    expect_output(print(conv), "Residual")
    expect_output(print(conv), "NA")
  })

  it("prints an ARPACK non-zero info line", {
    conv <- new_pagerank_convergence(
      algo = "arpack", info = 1L, tol_met = FALSE, residual = 0.5
    )
    expect_output(print(conv), "ARPACK info")
  })
})

# ---------------------------------------------------------------------------
# damping_sensitivity.R
# ---------------------------------------------------------------------------

describe("damping_sensitivity input validation", {
  it("errors when `alphas` is not numeric", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      damping_sensitivity(edges, alphas = "0.85"),
      "non-empty numeric vector",
      fixed = TRUE
    )
  })
})

# ---------------------------------------------------------------------------
# compute_pagerank.R
# ---------------------------------------------------------------------------

describe("compute_pagerank convergence-control validation", {
  edges <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))

  it("rejects a non-numeric `eps`", {
    expect_error(
      compute_pagerank(edges, eps = "x"),
      "single positive number",
      fixed = TRUE
    )
  })

  it("rejects a missing `eps`", {
    expect_error(
      compute_pagerank(edges, eps = NA_real_),
      "single positive number",
      fixed = TRUE
    )
  })

  it("rejects a non-numeric `niter`", {
    expect_error(
      compute_pagerank(edges, niter = "x"),
      "single positive integer",
      fixed = TRUE
    )
  })

  it("rejects a length > 1 `niter`", {
    expect_error(
      compute_pagerank(edges, niter = c(10, 20)),
      "single positive integer",
      fixed = TRUE
    )
  })

  it("rejects a missing `niter`", {
    expect_error(
      compute_pagerank(edges, niter = NA_integer_),
      "single positive integer",
      fixed = TRUE
    )
  })

  it("returns the empty result when the solver errors", {
    # A duplicate `options` argument forwarded through `...` makes
    # igraph::page_rank() error; .run_page_rank warns and returns NULL, and
    # compute_pagerank falls back to the empty result.
    expect_warning(
      res <- compute_pagerank(edges, options = igraph::arpack_defaults()),
      "page_rank computation failed",
      fixed = TRUE
    )
    expect_equal(nrow(res), 0)
  })
})

describe("compute_pagerank internal defensive helpers", {
  it(".pagerank_l1_residual returns NA on an empty graph", {
    expect_true(is.na(
      .pagerank_l1_residual(igraph::make_empty_graph(0), numeric(0), 0.85)
    ))
  })

  it(".build_pagerank_graph returns NULL with no edges and no nodes", {
    g <- .build_pagerank_graph(
      valid_edges_df = data.frame(from = character(0), to = character(0)),
      defined_nodes = NULL,
      weight_vector = NULL,
      reverse = FALSE,
      from_col = "from",
      to_col = "to"
    )
    expect_null(g)
  })
})

# ---------------------------------------------------------------------------
# resolve_redirects.R
# ---------------------------------------------------------------------------

describe("resolve_redirects internal graph helpers", {
  it(".resolve_via_graph errors on a bare self-loop when erroring", {
    expect_error(
      .resolve_via_graph("A", "A", loop_handling = "error"),
      "Redirect cycle detected",
      fixed = TRUE
    )
  })

  it(".resolve_via_graph strips a self-loop under prune_loop", {
    m <- .resolve_via_graph("A", "A", loop_handling = "prune_loop")
    expect_length(m, 1)
  })

  it(".format_cycle_path stops when a node has no in-cycle successor", {
    g <- igraph::graph_from_data_frame(
      data.frame(from = "A", to = "B"), directed = TRUE
    )
    expect_equal(.format_cycle_path(g, c("A", "B")), "A -> B")
  })

  it(".build_canonical_map returns an empty map for an empty graph", {
    expect_length(.build_canonical_map(igraph::make_empty_graph(0)), 0)
  })

  it("most_frequent tie-break keeps the earliest-occurring target", {
    edges <- data.frame(from = "B", to = "C")
    reds <- data.frame(from = c("B", "B"), to = c("Z", "A"))
    res <- resolve_redirects(
      edges, reds,
      duplicate_from_policy = "most_frequent"
    )
    # Z and A tie (once each); Z occurs first, so B folds to Z.
    expect_equal(res$from, "Z")
  })
})

# ---------------------------------------------------------------------------
# hits.R
# ---------------------------------------------------------------------------

describe("HITS validation and defensive branches", {
  edges <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))

  it(".resolve_defined_nodes drops an all-NA vertex column", {
    res <- compute_hits(
      edges,
      vertices_df = data.frame(node_name = NA_character_)
    )
    expect_true(all(c("A", "B", "C") %in% res$node_name))
  })

  it("builds an isolates graph from vertices when there are no edges", {
    res <- compute_hits(
      data.frame(from = character(0), to = character(0)),
      vertices_df = data.frame(node_name = c("A", "B"))
    )
    expect_true(all(c("A", "B") %in% res$node_name))
  })

  it("returns the empty result when hits_scores errors", {
    # A duplicate `weights` argument forwarded through `...` makes
    # igraph::hits_scores() error; .run_hits_scores warns and returns NULL.
    expect_warning(
      res <- compute_hits(edges, weights = 1),
      "hits_scores computation failed",
      fixed = TRUE
    )
    expect_equal(nrow(res), 0)
  })

  it("hits() accepts a valid weight column", {
    w_edges <- data.frame(
      from = c("A", "B"), to = c("B", "A"), w = c(2, 3)
    )
    res <- hits(w_edges, weight_col = "w", clean_edge_urls = FALSE)
    expect_true(all(c("A", "B") %in% res$node_name))
  })

  it(".clean_cols_if is a no-op when no from/to columns are present", {
    df <- data.frame(a = 1, b = 2)
    expect_identical(
      .clean_cols_if(df, intersect(c("from", "to"), names(df)), TRUE, list()),
      df
    )
  })
})

# ---------------------------------------------------------------------------
# trustrank.R
# ---------------------------------------------------------------------------

describe("trustrank / seed_prior validation", {
  it("seed_prior rejects a non-numeric `seed_weight`", {
    expect_error(
      seed_prior(c("A", "B"), seed_weight = "x"),
      "must be numeric or NULL",
      fixed = TRUE
    )
  })

  it("trustrank rejects a non-data-frame edge list", {
    expect_error(
      trustrank("not a data frame", "A"),
      "must be a data frame",
      fixed = TRUE
    )
  })
})

# ---------------------------------------------------------------------------
# pagerank.R
# ---------------------------------------------------------------------------

describe("pagerank argument validation", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "A"))

  it("rejects a non-numeric `damping` (unit-interval assert)", {
    expect_error(
      pagerank(edges, damping = "x", clean_edge_urls = FALSE),
      "between 0 and 1",
      fixed = TRUE
    )
  })

  it("rejects canonicals_df missing its from/to columns", {
    expect_error(
      pagerank(
        edges,
        canonicals_df = data.frame(x = 1, y = 2),
        clean_edge_urls = FALSE
      ),
      "must have",
      fixed = TRUE
    )
  })

  it("rejects a non-data-frame prior_df", {
    expect_error(
      pagerank(edges, prior_df = "x", clean_edge_urls = FALSE),
      "must be a data frame or NULL",
      fixed = TRUE
    )
  })

  it("rejects prior_df missing the url column", {
    expect_error(
      pagerank(
        edges, prior_df = data.frame(weight = 1), clean_edge_urls = FALSE
      ),
      "not found in `prior_df`",
      fixed = TRUE
    )
  })

  it("rejects prior_df missing the weight column", {
    expect_error(
      pagerank(
        edges, prior_df = data.frame(url = "A"), clean_edge_urls = FALSE
      ),
      "not found in `prior_df`",
      fixed = TRUE
    )
  })

  it("rejects a non-numeric prior weight column", {
    expect_error(
      pagerank(
        edges,
        prior_df = data.frame(url = "A", weight = "x"),
        clean_edge_urls = FALSE
      ),
      "must be a numeric column",
      fixed = TRUE
    )
  })
})

describe("pagerank internal defensive helpers", {
  it(".sf_faf_present is FALSE for an empty node set", {
    expect_false(.sf_faf_present(character(0), "example.com", "domain", list()))
  })

  it(".duplicate_edge_audit_rows returns NULL without an instance-count col", {
    expect_null(
      .duplicate_edge_audit_rows(
        data.frame(from = "a", to = "b", n = 1L), "from", "to", NULL, NULL
      )
    )
  })

  it(".duplicate_edge_audit_rows returns an empty frame when nothing repeats", {
    out <- .duplicate_edge_audit_rows(
      data.frame(from = "a", to = "b", n = 1L), "from", "to", "n", NULL
    )
    expect_equal(nrow(out), 0)
    expect_named(out, c("from", "to", "instance_count"))
  })

  it(".classify_indexability splits noindex vs robots-blocked (robots wins)", {
    res <- .classify_indexability(
      indexability_df = data.frame(
        url = c("A", "B", "C"),
        indexability_status = c(
          "noindex", "Blocked by robots.txt", "Blocked by robots.txt,noindex"
        )
      ),
      indexability_url_col = "url",
      indexability_status_col = "indexability_status"
    )
    expect_equal(res$noindex_urls, "A")
    # C is both, but robots.txt takes priority, so it is never noindex.
    expect_setequal(res$robots_blocked_urls, c("B", "C"))
  })

  it(".detect_fold_collisions skips targets folded only by uncrawled sources", {
    res <- .detect_fold_collisions(
      fold_map = c(X = "Y"),
      edge_list_df = data.frame(from = "P", to = "Q"),
      prefold_nodes = c("P", "Q"),
      indexability_df = data.frame(url = "Z"),
      indexability_url_col = "url",
      clean_edge_urls = FALSE,
      effective_rurl_params = list(),
      from_col = "from",
      to_col = "to"
    )
    expect_null(res)
  })

  it(".prepare_prior relabels leaked prior URLs onto the leak sink", {
    out <- .prepare_prior(
      prior_df = data.frame(url = "A", weight = 1),
      prior_url_col = "url",
      prior_weight_col = "weight",
      clean_edge_urls = FALSE,
      effective_rurl_params = list(),
      fold_map = character(0),
      used_leak_sink = TRUE,
      leak_sources = "A",
      leak_sink_name = "__sink__"
    )
    expect_equal(out$url, "__sink__")
  })
})

# ---------------------------------------------------------------------------
# aggregate_edges.R / audit_redirects.R / drop_isolates.R / export_graph.R /
# screaming_frog_bundle.R
# ---------------------------------------------------------------------------

describe("aggregate_edges input validation", {
  it("errors when `agg` is not a list", {
    edges <- data.frame(from = "A", to = "B", value = 1)
    expect_error(
      aggregate_edges(edges, agg = "not a list"),
      "must be a named list",
      fixed = TRUE
    )
  })
})

describe("audit_redirects defensive branches", {
  it(".audit_chain_lengths terminates on a residual cycle", {
    # Defensive guard: the public path prunes loops before chain analysis, so
    # a cycle should never reach .audit_chain_lengths. Feed an unbroken 2-cycle
    # directly to confirm the traversal breaks instead of spinning forever.
    g <- igraph::graph_from_data_frame(
      data.frame(from = c("A", "B"), to = c("B", "A")), directed = TRUE
    )
    lengths <- .audit_chain_lengths(g)
    expect_length(lengths, 2)
    expect_true(all(is.finite(lengths)))
  })

  it(".audit_orphans returns NULL for an empty edge list", {
    audit <- audit_redirects(
      data.frame(from = "A", to = "B"),
      edge_list_df = data.frame(from = character(0), to = character(0))
    )
    expect_null(audit$orphaned_redirects)
  })
})

describe("drop_isolates argument validation", {
  it("rejects a multi-element `node_col_name`", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      drop_isolates(edges, node_col_name = c("a", "b")),
      "non-empty single character string",
      fixed = TRUE
    )
  })
})

describe("export_graph validation and node-attr handling", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "A"))
  pr <- data.frame(node_name = c("A", "B"), pagerank = c(0.5, 0.5))

  it("rejects a multi-element `file`", {
    expect_error(
      export_graph(pr, edges, file = c("a.graphml", "b.graphml")),
      "single file path string",
      fixed = TRUE
    )
  })

  it("ignores non-list node_attrs", {
    tmp <- tempfile(fileext = ".graphml")
    on.exit(unlink(tmp), add = TRUE)
    out <- export_graph(pr, edges, file = tmp, node_attrs = "x")
    expect_true(file.exists(out))
  })
})

describe("screaming_frog_bundle count helper", {
  it(".sf_count_df returns a typed empty frame for empty input", {
    out <- .sf_count_df(data.frame(type = character(0)), "type")
    expect_equal(nrow(out), 0)
    expect_true(all(c("type", "n") %in% names(out)))
  })
})
