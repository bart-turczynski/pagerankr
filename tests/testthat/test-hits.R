context("HITS hub / authority scores")

describe("compute_hits", {
  it("matches igraph::hits_scores on a hand-built graph", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C")
    )
    res <- compute_hits(edges)

    g <- igraph::graph_from_data_frame(edges, directed = TRUE)
    truth <- igraph::hits_scores(g)

    got_hub <- stats::setNames(res$hub, res$node_name)
    got_auth <- stats::setNames(res$authority, res$node_name)
    expect_equal(got_hub[names(truth$hub)], truth$hub, tolerance = 1e-9)
    expect_equal(
      got_auth[names(truth$authority)], truth$authority,
      tolerance = 1e-9
    )
  })

  it("returns node_name, hub and authority columns", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
    res <- compute_hits(edges)
    expect_named(res, c("node_name", "hub", "authority"))
    expect_equal(nrow(res), 3)
  })

  it("encodes Kleinberg direction semantics", {
    # A is a pure hub (only points out); C is a pure authority (only pointed
    # to). Good hubs point to good authorities and vice versa.
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C")
    )
    res <- compute_hits(edges)
    expect_equal(res$node_name[which.max(res$hub)], "A")
    expect_equal(res$node_name[which.max(res$authority)], "C")
  })

  it("scales scores so the maximum is 1 by default", {
    edges <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
    res <- compute_hits(edges)
    expect_equal(max(res$hub), 1, tolerance = 1e-9)
    expect_equal(max(res$authority), 1, tolerance = 1e-9)
  })

  it("scale = FALSE returns unit-norm eigenvectors", {
    edges <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "C"))
    res <- compute_hits(edges, scale = FALSE)
    expect_equal(sum(res$hub^2), 1, tolerance = 1e-9)
    expect_equal(sum(res$authority^2), 1, tolerance = 1e-9)
  })

  it("retains isolates supplied via vertices_df with zero scores", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
    verts <- data.frame(node_name = c("A", "B", "C", "D"))
    res <- compute_hits(edges, vertices_df = verts)
    expect_true("D" %in% res$node_name)
    d <- res[res$node_name == "D", ]
    expect_equal(d$hub, 0)
    expect_equal(d$authority, 0)
  })

  it("honors edge weights", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C"),
      w = c(5, 1, 1)
    )
    res <- compute_hits(edges, weight_col = "w")

    g <- igraph::graph_from_data_frame(
      edges[, c("from", "to")],
      directed = TRUE
    )
    truth <- igraph::hits_scores(g, weights = edges$w)
    got_auth <- stats::setNames(res$authority, res$node_name)
    expect_equal(
      got_auth[names(truth$authority)], truth$authority,
      tolerance = 1e-9
    )
  })

  it("returns an empty data frame for an empty edge list", {
    res <- compute_hits(data.frame(from = character(0), to = character(0)))
    expect_named(res, c("node_name", "hub", "authority"))
    expect_equal(nrow(res), 0)
  })

  it("drops NA edges before computation", {
    edges <- data.frame(
      from = c("A", NA, "B"),
      to = c("B", "C", NA)
    )
    res <- compute_hits(edges)
    # Only A -> B survives.
    expect_setequal(res$node_name, c("A", "B"))
  })

  it("errors on duplicate output column names", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_hits(edges, hub_col = "node_name"),
      "must be distinct"
    )
  })

  it("errors on a non-logical scale argument", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(compute_hits(edges, scale = "yes"), "`scale` must be")
  })
})

describe("hits wrapper", {
  it("aligns its vertex set with pagerank() on the same input", {
    edges <- data.frame(
      from = c("http://A.com/", "http://A.com/", "B.com"),
      to = c("B.com", "C.com", "C.com")
    )
    h <- hits(edges)
    pr <- pagerank(edges)
    expect_setequal(h$node_name, pr$node_name)
  })

  it("canonicalizes URLs through the shared rurl profile", {
    # Trailing-slash / fragment variants fold to the same cleaned node as in
    # pagerank(), so the node set is the cleaned form.
    edges <- data.frame(
      from = c("http://A.com/", "http://A.com"),
      to = c("B.com#frag", "B.com")
    )
    h <- hits(edges)
    pr <- pagerank(edges)
    expect_setequal(h$node_name, pr$node_name)
  })

  it("folds redirects into the same identities as pagerank()", {
    edges <- data.frame(
      from = c("A.com", "B.com"),
      to = c("B.com", "C.com")
    )
    redirects <- data.frame(
      from = "B.com", to = "C.com"
    )
    h <- hits(edges, redirects_df = redirects)
    pr <- pagerank(edges, redirects_df = redirects)
    expect_setequal(h$node_name, pr$node_name)
    expect_false("B.com" %in% h$node_name)
  })

  it("surfaces hub vs authority on a cleaned graph", {
    edges <- data.frame(
      from = c("http://A.com/", "http://A.com/", "B.com"),
      to = c("B.com", "C.com", "C.com")
    )
    h <- hits(edges)
    top_hub <- h$node_name[which.max(h$hub)]
    top_auth <- h$node_name[which.max(h$authority)]
    # Host is lowercased by the shared canonicalization profile.
    expect_true(grepl("a.com", top_hub, fixed = TRUE))
    expect_true(grepl("c.com", top_auth, fixed = TRUE))
  })

  it("keeps isolates when drop_isolates_flag = FALSE", {
    edges <- rbind(
      data.frame(from = "A", to = "B"),
      data.frame(from = "ISO", to = "LAND")
    )
    h <- hits(edges, drop_isolates_flag = FALSE, clean_edge_urls = FALSE)
    expect_true(all(c("A", "B", "ISO", "LAND") %in% h$node_name))
  })

  it("respects domain filtering (user-filtered graph HITS)", {
    edges <- data.frame(
      from = c("http://site.com/a", "http://site.com/a"),
      to = c("http://site.com/b", "http://other.com/x")
    )
    h <- hits(edges, keep_domains = "site.com")
    expect_false(any(grepl("other.com", h$node_name)))
  })

  it("counts duplicate link slots under count_instances", {
    edges <- data.frame(
      from = c("A", "A", "A"),
      to = c("B", "C", "C")
    )
    # A -> C twice should make C a stronger authority than B.
    h <- hits(edges,
      duplicate_edge_policy = "count_instances",
      clean_edge_urls = FALSE
    )
    auth <- stats::setNames(h$authority, h$node_name)
    expect_gt(auth[["C"]], auth[["B"]])
  })

  it("errors on a non-data-frame edge list", {
    expect_error(hits(list(a = 1)), "must be a data frame")
  })
})

describe("compute_hits input validation", {
  it("errors on a non-data-frame edge list", {
    expect_error(compute_hits(list(from = "A")), "must be a data frame")
  })

  it("errors on nonempty edge list missing from/to columns", {
    edges <- data.frame(src = "A", dst = "B")
    expect_error(compute_hits(edges, from_col = "from"), "'from'")
  })

  it("errors on a non-data-frame vertices_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_hits(edges, vertices_df = list(node_name = "A")),
      "must be a data frame"
    )
  })

  it("errors on nonempty vertices_df missing the vertex column", {
    edges <- data.frame(from = "A", to = "B")
    verts <- data.frame(bad_col = "A")
    expect_error(compute_hits(edges, vertices_df = verts), "must have a column named")
  })

  it("errors on an empty pr_node_col string", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(compute_hits(edges, pr_node_col = ""), "non-empty character strings")
  })

  it("errors on a non-character weight_col", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(compute_hits(edges, weight_col = 1L), "single character string")
  })

  it("errors when weight_col is absent from edge_list_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(compute_hits(edges, weight_col = "w"), "not found")
  })

  it("errors when weight_col column is not numeric", {
    edges <- data.frame(from = "A", to = "B", w = "heavy")
    expect_error(compute_hits(edges, weight_col = "w"), "must be a numeric column")
  })
})

describe("hits wrapper input validation", {
  it("errors on a non-data-frame redirects_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(hits(edges, redirects_df = list(from = "A")), "must be a data frame")
  })

  it("errors on a non-data-frame canonicals_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(hits(edges, canonicals_df = list(from = "A")), "must be a data frame")
  })

  it("errors on a non-logical clean_canonical_urls", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(hits(edges, clean_canonical_urls = "yes"), "single logical value")
  })

  it("errors when nonempty canonicals_df is missing required columns", {
    edges <- data.frame(from = "A", to = "B")
    canonicals <- data.frame(bad_col = "A")
    expect_error(hits(edges, canonicals_df = canonicals), "must have '")
  })

  it("errors on a non-logical clean_edge_urls", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(hits(edges, clean_edge_urls = "yes"), "single logical value")
  })

  it("errors on a non-logical clean_redirect_urls", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(hits(edges, clean_redirect_urls = "yes"), "single logical value")
  })

  it("errors on a non-list rurl_params", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(hits(edges, rurl_params = "bad"), "must be a list")
  })

  it("errors on a non-logical drop_isolates_flag", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(hits(edges, drop_isolates_flag = 1), "single logical value")
  })

  it("errors on a non-character weight_col", {
    edges <- data.frame(from = "A", to = "B", w = 1)
    expect_error(hits(edges, weight_col = 1L), "single character string")
  })

  it("errors when weight_col is absent from edge_list_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(hits(edges, weight_col = "missing"), "not found")
  })
})

describe("hits wrapper canonicals_df path", {
  it("folds canonicals into the same node identities as pagerank()", {
    edges <- data.frame(from = c("A.com", "B.com"), to = c("B.com", "C.com"))
    canonicals <- data.frame(from = "B.com", to = "C.com")
    h <- hits(edges, canonicals_df = canonicals)
    pr <- pagerank(edges, canonicals_df = canonicals)
    expect_setequal(h$node_name, pr$node_name)
    expect_false("B.com" %in% h$node_name)
  })
})
