context("compute_pagerank")

describe("compute_pagerank basic functionality", {
  it("computes PageRank for a simple graph", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    pr <- compute_pagerank(edges)
    expect_equal(nrow(pr), 3)
    expect_true(all(c("node_name", "pagerank") %in% names(pr)))
    expect_true(all(pr$node_name %in% c("A", "B", "C")))
    expect_true(all(pr$pagerank > 0))
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
  })

  it("handles custom column names for input and output", {
    edges_c <- data.frame(
      source = c("N1", "N2"),
      target = c("N2", "N1")
    )
    # N3 is isolate
    verts_c <- data.frame(
      name = c("N1", "N2", "N3")
    )
    pr_c <- compute_pagerank(edges_c,
      vertices_df = verts_c,
      from_col = "source", to_col = "target",
      vertex_col_name = "name",
      pr_node_col = "ID", pr_value_col = "PR_Score"
    )
    expect_equal(nrow(pr_c), 3)
    expect_true(all(c("ID", "PR_Score") %in% names(pr_c)))
    expect_equal(sum(pr_c$PR_Score), 1, tolerance = 1e-9)
    expect_true("N3" %in% pr_c$ID)
    # Isolates get some PR via teleportation
    expect_gt(pr_c$PR_Score[pr_c$ID == "N3"], 0)
  })

  it("applies damping factor correctly", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A")
    )
    pr_d85 <- compute_pagerank(edges, damping = 0.85)
    pr_d50 <- compute_pagerank(edges, damping = 0.50)
    # For a symmetric 2-node graph, PR should be 0.5 for both,
    # regardless of damping.
    # Order may vary
    expect_equal(
      pr_d85$pagerank,
      c(0.5, 0.5),
      tolerance = 1e-9,
      ignore_attr = TRUE
    )
    expect_equal(
      pr_d50$pagerank,
      c(0.5, 0.5),
      tolerance = 1e-9,
      ignore_attr = TRUE
    )

    # More complex graph to see damping effect (qualitative)
    edges2 <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C")
    )
    # A links to B and C. B links to C. C is a sink (or links to A if we
    # make it a cycle for stability)
    # Let's make C link to A to avoid C taking all PR if it were a pure sink.
    edges3 <- rbind(
      edges2,
      data.frame(from = "C", to = "A")
    )
    pr3_d85 <- compute_pagerank(edges3, damping = 0.85)
    pr3_d50 <- compute_pagerank(edges3, damping = 0.50)
    # Hard to predict exact values, but they should sum to 1 and be different.
    expect_equal(sum(pr3_d85$pagerank), 1, tolerance = 1e-9)
    expect_equal(sum(pr3_d50$pagerank), 1, tolerance = 1e-9)
    # We expect PR values to differ if damping is different for a
    # non-trivial graph. This is not strictly guaranteed for all nodes
    # but generally true for the vector.
    # For this graph, they should be different.
    expect_false(all(abs(pr3_d85$pagerank - pr3_d50$pagerank) < 1e-9))
  })
})

describe("compute_pagerank graph structure handling", {
  it("handles single-node graph (with self-loop from edges)", {
    edges_single_loop <- data.frame(
      from = "A",
      to = "A"
    )
    pr <- compute_pagerank(edges_single_loop)
    expect_equal(nrow(pr), 1)
    expect_equal(pr$node_name, "A")
    expect_equal(pr$pagerank, 1)
  })

  it("handles single-node graph (no edges, defined by vertices_df)", {
    verts_single <- data.frame(node_name = "A")
    # igraph requires edges to not be NULL, so pass empty character
    # vectors for edge columns
    edges_empty_struct <- data.frame(
      from = character(0),
      to = character(0)
    )
    pr <- compute_pagerank(edges_empty_struct, vertices_df = verts_single)
    expect_equal(nrow(pr), 1)
    expect_equal(pr$node_name, "A")
    expect_equal(pr$pagerank, 1)
  })

  it("handles graph with isolates (defined by vertices_df)", {
    edges <- data.frame(from = "A", to = "B")
    # C, D are isolates
    verts <- data.frame(
      node_name = c("A", "B", "C", "D")
    )
    pr <- compute_pagerank(edges, vertices_df = verts)
    expect_equal(nrow(pr), 4)
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
    # Isolates get base PR
    expect_true(all(pr$pagerank[pr$node_name %in% c("C", "D")] > 0))
  })

  it("handles empty graph (no edges, no vertices_df)", {
    edges_empty <- data.frame(
      from = character(),
      to = character()
    )
    pr <- compute_pagerank(edges_empty)
    expect_equal(nrow(pr), 0)
    expect_named(pr, c("node_name", "pagerank"))
    expect_true(is.character(pr$node_name))
    expect_true(is.numeric(pr$pagerank))
  })

  it("handles graph defined only by empty vertices_df (no edges)", {
    verts_empty <- data.frame(
      node_name = character(0)
    )
    edges_empty <- data.frame(
      from = character(),
      to = character()
    )
    pr <- compute_pagerank(edges_empty, vertices_df = verts_empty)
    expect_equal(nrow(pr), 0)
  })

  it("handles graph with no edges but non-empty vertices_df", {
    verts_no_edges <- data.frame(v = c("X", "Y", "Z"))
    edges_empty <- data.frame(
      f = character(),
      t = character()
    )
    pr <- compute_pagerank(
      edges_empty,
      vertices_df = verts_no_edges,
      vertex_col_name = "v",
      from_col = "f",
      to_col = "t"
    )
    expect_equal(nrow(pr), 3)
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
    # For a graph with no edges, PR is distributed equally
    expect_true(all(abs(pr$pagerank - (1 / 3)) < 1e-9))
  })

  it("handles NA values in edge_list_df (edges with NA are dropped)", {
    edges_na <- data.frame(
      from = c("A", NA, "C", "D", "A"),
      to = c("B", "E", NA, "E", "C")
    )
    # Valid edges: A->B, D->E, A->C. Nodes: A,B,C,D,E
    pr <- compute_pagerank(edges_na)
    expect_equal(nrow(pr), 5) # A, B, C, D, E should be present
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)

    # With vertices_df that includes nodes not in valid edges (e.g. F from NA)
    verts_na <- data.frame(
      name = c("A", "B", "C", "D", "E", "F")
    )
    pr_v <- compute_pagerank(
      edges_na,
      vertices_df = verts_na,
      vertex_col_name = "name"
    )
    expect_equal(nrow(pr_v), 6) # F is included as an isolate
    expect_true("F" %in% pr_v$node_name)
    expect_gt(pr_v$pagerank[pr_v$node_name == "F"], 0)
  })

  it("handles case where all edges become NA or are empty after processing", {
    edges_all_na <- data.frame(from = NA_character_, to = NA_character_)
    pr <- compute_pagerank(edges_all_na)
    expect_equal(nrow(pr), 0)

    edges_all_na_verts <- data.frame(from = NA_character_, to = NA_character_)
    verts <- data.frame(node_name = c("A", "B"))
    pr_v <- compute_pagerank(edges_all_na_verts, vertices_df = verts)
    expect_equal(nrow(pr_v), 2) # A, B are isolates
    expect_equal(
      pr_v$pagerank,
      c(0.5, 0.5),
      tolerance = 1e-9,
      ignore_attr = TRUE
    )
  })
})

describe("compute_pagerank error handling and edge cases", {
  it("errors on invalid input types", {
    expect_error(compute_pagerank(list()))
    expect_error(
      compute_pagerank(data.frame(f = "a", t = "b"), vertices_df = list())
    )
    expect_error(
      compute_pagerank(data.frame(f = "a", t = "b"), damping = "high")
    )
    expect_error(
      compute_pagerank(data.frame(f = "a", t = "b"), pr_node_col = 123)
    )
    # Same names
    expect_error(
      compute_pagerank(
        data.frame(f = "a", t = "b"),
        pr_node_col = "node",
        pr_value_col = "node"
      )
    )
  })

  it("errors if columns are missing and df is not empty", {
    df <- data.frame(fcol = "a", tcol = "b")
    vdf <- data.frame(vert_name = "a")
    expect_error(compute_pagerank(df)) # Default from/to missing in df
    expect_error(compute_pagerank(edges_list_df = df, from_col = "x"))
    expect_error(
      compute_pagerank(edges_list_df = df, from_col = "fcol", to_col = "y")
    )
    # Default v_col_name missing
    expect_error(
      compute_pagerank(
        edges_list_df = df,
        from_col = "fcol",
        to_col = "tcol",
        vertices_df = vdf
      )
    )
    expect_error(
      compute_pagerank(
        edges_list_df = df,
        from_col = "fcol",
        to_col = "tcol",
        vertices_df = vdf,
        vertex_col_name = "z"
      )
    )
  })

  it("computes weighted PageRank when weight_col is provided", {
    # A -> B (weight 1), A -> C (weight 10)
    # With higher weight on A->C, C should get more PR than B
    edges <- data.frame(
      from = c("A", "A", "B", "C"),
      to = c("B", "C", "A", "A"),
      w = c(1, 10, 1, 1)
    )
    pr_w <- compute_pagerank(edges, weight_col = "w")
    expect_equal(nrow(pr_w), 3)
    expect_equal(sum(pr_w$pagerank), 1, tolerance = 1e-9)
    pr_c <- pr_w$pagerank[pr_w$node_name == "C"]
    pr_b <- pr_w$pagerank[pr_w$node_name == "B"]
    expect_gt(pr_c, pr_b)
  })

  it("weight_col = NULL produces unweighted PageRank", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    pr_null <- compute_pagerank(edges, weight_col = NULL)
    pr_default <- compute_pagerank(edges)
    expect_equal(pr_null$pagerank, pr_default$pagerank, tolerance = 1e-9)
  })

  it("errors when weight_col does not exist", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_pagerank(edges, weight_col = "nonexistent"),
      "not found"
    )
  })

  it("errors when weight_col is not numeric", {
    edges <- data.frame(
      from = "A",
      to = "B",
      w = "high"
    )
    expect_error(
      compute_pagerank(edges, weight_col = "w"),
      "numeric"
    )
  })

  it("passes additional arguments to igraph::page_rank via ...", {
    skip_if_not_installed("igraph")
    local_edition(3)
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A")
    )
    # `pagerankr_sentinel` is not an argument compute_pagerank() handles
    # itself, so its only route to page_rank() is the `...` passthrough.
    # Observing it inside the mock proves the forwarding works.
    sentinel_seen <- FALSE
    local_mocked_bindings(
      page_rank = function(graph, ...) {
        dots <- list(...)
        sentinel_seen <<- "pagerankr_sentinel" %in% names(dots)
        expect_true(sentinel_seen)
        expect_identical(dots[["pagerankr_sentinel"]], "flows-through")
        n <- igraph::gorder(graph)
        list(
          vector = stats::setNames(
            rep(1 / n, n), igraph::V(graph)$name
          ),
          options = NULL
        )
      },
      .package = "igraph"
    )
    result <- compute_pagerank(edges, pagerankr_sentinel = "flows-through")
    expect_true(sentinel_seen)
    expect_s3_class(result, "data.frame")
    expect_setequal(result$node_name, c("A", "B"))
    expect_equal(sum(result$pagerank), 1, tolerance = 1e-9)
  })
})

describe("compute_pagerank validation coverage", {
  it("errors when vertices_df is not a data frame", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_pagerank(edges, vertices_df = "not_a_df"),
      "data frame or NULL"
    )
  })

  it("errors when vertex_col_name is missing from vertices_df", {
    edges <- data.frame(from = "A", to = "B")
    verts <- data.frame(wrong_col = "A")
    expect_error(
      compute_pagerank(edges, vertices_df = verts),
      "column named"
    )
  })

  it("errors when damping is out of range", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(compute_pagerank(edges, damping = 1.5), "between 0 and 1")
    expect_error(compute_pagerank(edges, damping = -0.1), "between 0 and 1")
    expect_error(compute_pagerank(edges, damping = "high"), "between 0 and 1")
  })

  it("errors when pr_node_col is invalid", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(compute_pagerank(edges, pr_node_col = ""), "non-empty")
    expect_error(compute_pagerank(edges, pr_node_col = 123), "non-empty")
  })

  it("errors when pr_value_col is invalid", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(compute_pagerank(edges, pr_value_col = ""), "non-empty")
  })

  it("errors when pr_node_col equals pr_value_col", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_pagerank(edges, pr_node_col = "x", pr_value_col = "x"),
      "must be different"
    )
  })

  it("errors when weight_col is not a character string", {
    edges <- data.frame(from = "A", to = "B", w = 1)
    expect_error(
      compute_pagerank(edges, weight_col = 123),
      "single character string"
    )
  })

  it("handles graph where vcount is 0 after construction", {
    # Empty edges with NULL vertices -> empty result
    edges_empty <- data.frame(
      from = character(0), to = character(0)
    )
    pr <- compute_pagerank(edges_empty)
    expect_equal(nrow(pr), 0)
  })

  it("handles vertices_df where all nodes are NA", {
    edges <- data.frame(
      from = character(0), to = character(0)
    )
    verts <- data.frame(
      node_name = c(NA_character_, NA_character_)
    )
    pr <- compute_pagerank(edges, vertices_df = verts)
    expect_equal(nrow(pr), 0)
  })

  it("handles edges with no valid rows and no defined_nodes", {
    edges <- data.frame(
      from = NA_character_, to = NA_character_
    )
    pr <- compute_pagerank(edges)
    expect_equal(nrow(pr), 0)
  })

  it("errors when edge_list_df is not a data frame", {
    expect_error(
      compute_pagerank("not_a_df"),
      "must be a data frame"
    )
  })

  it("errors when non-empty edge_list_df is missing from/to columns", {
    edges <- data.frame(x = "A", y = "B")
    expect_error(
      compute_pagerank(edges),
      "must have.*from.*to"
    )
  })

  it("errors when pr_score_col is not a non-empty string", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_pagerank(edges, pr_value_col = 123),
      "non-empty character string"
    )
  })
})
