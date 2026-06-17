context("reverse / inverse PageRank (CheiRank)")

describe("compute_pagerank reverse = TRUE", {
  it("matches igraph::page_rank on a manually reversed edge list", {
    # Hand-built graph: A funnels outward (A->B, A->C), B->C.
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      stringsAsFactors = FALSE
    )

    pr_rev <- compute_pagerank(edges, reverse = TRUE)

    # Ground truth: igraph on the manually swapped edge list.
    edges_swapped <- data.frame(
      from = edges$to, to = edges$from,
      stringsAsFactors = FALSE
    )
    g_manual <- igraph::graph_from_data_frame(edges_swapped, directed = TRUE)
    pr_manual <- igraph::page_rank(g_manual)$vector

    got <- stats::setNames(pr_rev$pagerank, pr_rev$node_name)
    expect_equal(got[names(pr_manual)], pr_manual, tolerance = 1e-9)
  })

  it("equals running compute_pagerank on swapped columns", {
    edges <- data.frame(
      from = c("A", "A", "B", "C"),
      to = c("B", "C", "C", "A"),
      stringsAsFactors = FALSE
    )
    pr_rev <- compute_pagerank(edges, reverse = TRUE)

    edges_swapped <- data.frame(
      from = edges$to, to = edges$from,
      stringsAsFactors = FALSE
    )
    pr_swapped <- compute_pagerank(edges_swapped, reverse = FALSE)

    a <- pr_rev[order(pr_rev$node_name), ]
    b <- pr_swapped[order(pr_swapped$node_name), ]
    expect_equal(a$node_name, b$node_name)
    expect_equal(a$pagerank, b$pagerank, tolerance = 1e-12)
  })

  it("surfaces the outflow funnel: A scores highest reversed", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      stringsAsFactors = FALSE
    )
    pr_fwd <- compute_pagerank(edges, reverse = FALSE)
    pr_rev <- compute_pagerank(edges, reverse = TRUE)

    # Forward: C (pure inflow sink) is top. Reversed: A (pure outflow) is top.
    expect_equal(pr_fwd$node_name[which.max(pr_fwd$pagerank)], "C")
    expect_equal(pr_rev$node_name[which.max(pr_rev$pagerank)], "A")
  })

  it("preserves edge weights under reversal", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      w = c(5, 1, 1),
      stringsAsFactors = FALSE
    )
    pr_rev <- compute_pagerank(edges, weight_col = "w", reverse = TRUE)

    edges_swapped <- data.frame(
      from = edges$to, to = edges$from, w = edges$w,
      stringsAsFactors = FALSE
    )
    pr_manual <- compute_pagerank(edges_swapped, weight_col = "w")

    a <- pr_rev[order(pr_rev$node_name), ]
    b <- pr_manual[order(pr_manual$node_name), ]
    expect_equal(a$pagerank, b$pagerank, tolerance = 1e-12)
  })

  it("reverse = FALSE is identical to the default", {
    edges <- data.frame(
      from = c("A", "B", "C"), to = c("B", "C", "A"),
      stringsAsFactors = FALSE
    )
    expect_equal(
      compute_pagerank(edges, reverse = FALSE),
      compute_pagerank(edges)
    )
  })

  it("errors on a non-logical reverse argument", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(
      compute_pagerank(edges, reverse = "yes"),
      "`reverse` must be a single logical value"
    )
    expect_error(
      compute_pagerank(edges, reverse = NA),
      "`reverse` must be a single logical value"
    )
  })
})

describe("pagerank wrapper reverse = TRUE", {
  it("equals running pagerank on swapped edge columns end-to-end", {
    edges <- data.frame(
      from = c("http://A.com/", "http://A.com/", "B.com"),
      to = c("B.com", "C.com", "C.com"),
      stringsAsFactors = FALSE
    )
    pr_rev <- pagerank(edges, reverse = TRUE)

    edges_swapped <- data.frame(
      from = edges$to, to = edges$from,
      stringsAsFactors = FALSE
    )
    pr_manual <- pagerank(edges_swapped, reverse = FALSE)

    a <- pr_rev[order(pr_rev$node_name), ]
    b <- pr_manual[order(pr_manual$node_name), ]
    expect_equal(a$node_name, b$node_name)
    expect_equal(a$pagerank, b$pagerank, tolerance = 1e-12)
  })

  it("allows nofollow_action = 'drop' under reverse", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      nofollow = c(FALSE, TRUE, FALSE),
      stringsAsFactors = FALSE
    )
    expect_silent(
      pr <- pagerank(
        edges,
        nofollow_col = "nofollow", nofollow_action = "drop",
        reverse = TRUE, clean_edge_urls = FALSE
      )
    )
    expect_true(is.data.frame(pr))
  })

  it("errors on nofollow_action = 'evaporate' under reverse", {
    edges <- data.frame(
      from = c("A", "A"), to = c("B", "C"),
      nofollow = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    )
    expect_error(
      pagerank(
        edges,
        nofollow_col = "nofollow", nofollow_action = "evaporate",
        reverse = TRUE, clean_edge_urls = FALSE
      ),
      "evaporate.*reverse|reverse.*evaporate"
    )
  })

  it("errors when indexability_df is supplied under reverse", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    idx <- data.frame(
      url = "A", indexability_status = "noindex",
      stringsAsFactors = FALSE
    )
    expect_error(
      pagerank(
        edges,
        indexability_df = idx, reverse = TRUE, clean_edge_urls = FALSE
      ),
      "indexability_df.*reverse|reverse.*indexability"
    )
  })

  it("is orthogonal to the TIPR prior (reverse + prior_df combine)", {
    edges <- data.frame(
      from = c("A", "A", "B"), to = c("B", "C", "C"),
      stringsAsFactors = FALSE
    )
    prior <- data.frame(url = "C", weight = 10, stringsAsFactors = FALSE)
    expect_silent(
      pr <- pagerank(
        edges,
        prior_df = prior, reverse = TRUE,
        clean_edge_urls = FALSE, prior_verbose = FALSE
      )
    )
    expect_true("prior_weight" %in% names(pr))
  })

  it("errors on a non-logical reverse argument", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(
      pagerank(edges, reverse = "yes"),
      "`reverse` must be a single logical value"
    )
  })
})
