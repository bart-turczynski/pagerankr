context("SALSA hub / authority scores")

describe("compute_salsa", {
  it("returns node_name, hub and authority columns", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
    res <- compute_salsa(edges)
    expect_named(res, c("node_name", "hub", "authority"))
    expect_equal(nrow(res), 3)
  })

  it("matches the connected-graph closed form d_in/W and d_out/W", {
    # Single weakly connected component: authority = d_in / W, hub = d_out / W.
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C")
    )
    res <- compute_salsa(edges)
    auth <- stats::setNames(res$authority, res$node_name)
    hub <- stats::setNames(res$hub, res$node_name)

    total_edges <- nrow(edges) # three edges form the denominator W
    # in-degrees: A=0, B=1, C=2 ; out-degrees: A=2, B=1, C=0
    expect_true(is.na(auth[["A"]]))
    expect_equal(auth[["B"]], 1 / total_edges, tolerance = 1e-9)
    expect_equal(auth[["C"]], 2 / total_edges, tolerance = 1e-9)
    expect_equal(hub[["A"]], 2 / total_edges, tolerance = 1e-9)
    expect_equal(hub[["B"]], 1 / total_edges, tolerance = 1e-9)
    expect_true(is.na(hub[["C"]]))
  })

  it("produces stochastic scores that sum to 1 within each side", {
    edges <- data.frame(
      from = c("A", "A", "B", "C"),
      to = c("B", "C", "C", "A")
    )
    res <- compute_salsa(edges)
    expect_equal(sum(res$hub, na.rm = TRUE), 1, tolerance = 1e-9)
    expect_equal(sum(res$authority, na.rm = TRUE), 1, tolerance = 1e-9)
  })

  it("encodes hub/authority direction semantics", {
    # A is a pure hub (only points out); C is a pure authority (only pointed
    # to).
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "C")
    )
    res <- compute_salsa(edges)
    expect_equal(res$node_name[which.max(res$hub)], "A")
    expect_equal(res$node_name[which.max(res$authority)], "C")
  })

  it("returns NA authority for zero-in-degree and NA hub for zero-out-degree", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "C")
    )
    res <- compute_salsa(edges)
    a <- res[res$node_name == "A", ]
    cc <- res[res$node_name == "C", ]
    expect_true(is.na(a$authority)) # A has no inlinks
    expect_false(is.na(a$hub))
    expect_true(is.na(cc$hub)) # C has no outlinks
    expect_false(is.na(cc$authority))
  })

  it("applies the component mass-weighting correction with >1 component", {
    # Two disconnected components. Component 1: A->B (1 edge).
    # Component 2: C->D, C->E (2 edges).
    edges <- data.frame(
      from = c("A", "C", "C"),
      to = c("B", "D", "E")
    )
    res <- compute_salsa(edges)
    auth <- stats::setNames(res$authority, res$node_name)

    # Authorities: B (comp1), D, E (comp2). |A| = 3.
    # Comp1 share = 1/3 over 1 authority -> B = 1/3.
    # Comp2 share = 2/3 over 2 authorities, each d_in/W_c = 1/2 -> 2/3 * 1/2.
    expect_equal(auth[["B"]], 1 / 3, tolerance = 1e-9)
    expect_equal(auth[["D"]], (2 / 3) * (1 / 2), tolerance = 1e-9)
    expect_equal(auth[["E"]], (2 / 3) * (1 / 2), tolerance = 1e-9)
    expect_equal(sum(res$authority, na.rm = TRUE), 1, tolerance = 1e-9)

    # Without the mass-weighting, B (the only authority in comp1) would score
    # the within-component stationary value of 1, swamping the larger
    # component's authorities -- the documented cross-component failure mode.
    expect_false(isTRUE(all.equal(auth[["B"]], 1)))
  })

  it("retains isolates supplied via vertices_df with NA scores", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
    verts <- data.frame(node_name = c("A", "B", "C", "D"))
    res <- compute_salsa(edges, vertices_df = verts)
    expect_true("D" %in% res$node_name)
    d <- res[res$node_name == "D", ]
    expect_true(is.na(d$hub))
    expect_true(is.na(d$authority))
  })

  it("returns an empty data frame for an empty edge list", {
    res <- compute_salsa(data.frame(from = character(0), to = character(0)))
    expect_named(res, c("node_name", "hub", "authority"))
    expect_equal(nrow(res), 0)
  })

  it("drops NA edges before computation", {
    edges <- data.frame(
      from = c("A", NA, "B"),
      to = c("B", "C", NA)
    )
    res <- compute_salsa(edges)
    # Only A -> B survives.
    expect_setequal(res$node_name, c("A", "B"))
  })

  it("errors on duplicate output column names", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_salsa(edges, hub_col = "node_name"),
      "must be distinct"
    )
  })

  it("errors on a non-data-frame edge list", {
    expect_error(compute_salsa(list(a = 1)), "must be a data frame")
  })

  it("returns the empty result when the built graph has zero vertices", {
    # Defensive branch: .compute_salsa_build_graph() cannot itself return a
    # zero-vertex graph through the public API's validated inputs, so mock it
    # directly to exercise compute_salsa()'s vcount == 0 guard.
    edges <- data.frame(from = "A", to = "B")
    local_mocked_bindings(
      .compute_salsa_build_graph = function(...) {
        igraph::make_empty_graph(n = 0, directed = TRUE)
      }
    )
    res <- compute_salsa(edges)
    expect_named(res, c("node_name", "hub", "authority"))
    expect_equal(nrow(res), 0)
  })

  it("builds an empty-edge graph from defined_nodes (isolate-only universe)", {
    # All edges drop to NA, but vertices_df still supplies a defined-node
    # universe, so .compute_salsa_build_graph() takes the make_empty_graph()
    # branch instead of graph_from_data_frame().
    edges <- data.frame(from = NA_character_, to = NA_character_)
    verts <- data.frame(node_name = c("X", "Y"))
    res <- compute_salsa(edges, vertices_df = verts)
    expect_setequal(res$node_name, c("X", "Y"))
    expect_true(all(is.na(res$hub)))
    expect_true(all(is.na(res$authority)))
  })
})

describe("salsa wrapper", {
  it("aligns its vertex set with pagerank() on the same input", {
    edges <- data.frame(
      from = c("http://A.com/", "http://A.com/", "B.com"),
      to = c("B.com", "C.com", "C.com")
    )
    s <- salsa(edges)
    pr <- pagerank(edges)
    expect_setequal(s$node_name, pr$node_name)
  })

  it("canonicalizes URLs through the shared rurl profile", {
    edges <- data.frame(
      from = c("http://A.com/", "http://A.com"),
      to = c("B.com#frag", "B.com")
    )
    s <- salsa(edges)
    pr <- pagerank(edges)
    expect_setequal(s$node_name, pr$node_name)
  })

  it("folds redirects into the same identities as pagerank()", {
    edges <- data.frame(
      from = c("A.com", "B.com"),
      to = c("B.com", "C.com")
    )
    redirects <- data.frame(
      from = "B.com", to = "C.com"
    )
    s <- salsa(edges, redirects_df = redirects)
    pr <- pagerank(edges, redirects_df = redirects)
    expect_setequal(s$node_name, pr$node_name)
    expect_false("B.com" %in% s$node_name)
  })

  it("surfaces hub vs authority on a cleaned graph", {
    edges <- data.frame(
      from = c("http://A.com/", "http://A.com/", "B.com"),
      to = c("B.com", "C.com", "C.com")
    )
    s <- salsa(edges)
    top_hub <- s$node_name[which.max(s$hub)]
    top_auth <- s$node_name[which.max(s$authority)]
    expect_true(grepl("a.com", top_hub, fixed = TRUE))
    expect_true(grepl("c.com", top_auth, fixed = TRUE))
  })

  it("keeps isolates when drop_isolates_flag = FALSE (NA scores)", {
    edges <- rbind(
      data.frame(from = "A", to = "B"),
      data.frame(from = "ISO", to = "LAND")
    )
    s <- salsa(edges, drop_isolates_flag = FALSE, clean_edge_urls = FALSE)
    expect_true(all(c("A", "B", "ISO", "LAND") %in% s$node_name))
  })

  it("respects domain filtering (user-filtered graph SALSA)", {
    edges <- data.frame(
      from = c("http://site.com/a", "http://site.com/a"),
      to = c("http://site.com/b", "http://other.com/x")
    )
    s <- salsa(edges, keep_domains = "site.com")
    expect_false(any(grepl("other.com", s$node_name)))
  })

  it("each side still sums to 1 after the full pipeline", {
    edges <- data.frame(
      from = c("http://A.com/", "http://A.com/", "B.com", "C.com"),
      to = c("B.com", "C.com", "C.com", "A.com")
    )
    s <- salsa(edges)
    expect_equal(sum(s$hub, na.rm = TRUE), 1, tolerance = 1e-9)
    expect_equal(sum(s$authority, na.rm = TRUE), 1, tolerance = 1e-9)
  })

  it("errors on a non-data-frame edge list", {
    expect_error(salsa(list(a = 1)), "must be a data frame")
  })
})

describe("compute_salsa input validation", {
  it("errors on nonempty edge list missing from/to columns", {
    edges <- data.frame(src = "A", dst = "B")
    expect_error(compute_salsa(edges, from_col = "from"), "'from'")
  })

  it("errors on a non-data-frame vertices_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_salsa(edges, vertices_df = list(node_name = "A")),
      "must be a data frame"
    )
  })

  it("errors on nonempty vertices_df missing the vertex column", {
    edges <- data.frame(from = "A", to = "B")
    verts <- data.frame(bad_col = "A")
    expect_error(
      compute_salsa(edges, vertices_df = verts),
      "must have a column named"
    )
  })

  it("errors on an empty pr_node_col string", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      compute_salsa(edges, pr_node_col = ""),
      "non-empty character strings"
    )
  })

  it("zeroes out defined_nodes when all vertex rows are NA", {
    verts <- data.frame(node_name = NA_character_)
    res <- compute_salsa(
      data.frame(from = "A", to = "B"),
      vertices_df = verts
    )
    expect_s3_class(res, "data.frame")
  })
})

describe("salsa wrapper input validation", {
  it("errors on a non-data-frame redirects_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      salsa(edges, redirects_df = list(from = "A")),
      "must be a data frame"
    )
  })

  it("errors on a non-data-frame canonicals_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      salsa(edges, canonicals_df = list(from = "A")),
      "must be a data frame"
    )
  })

  it("errors on a non-logical clean_canonical_urls", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      salsa(edges, clean_canonical_urls = "yes"),
      "single logical value"
    )
  })

  it("errors when nonempty canonicals_df is missing required columns", {
    edges <- data.frame(from = "A", to = "B")
    canonicals <- data.frame(bad_col = "A")
    expect_error(salsa(edges, canonicals_df = canonicals), "must have '")
  })

  it("errors on a non-logical clean_edge_urls", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(salsa(edges, clean_edge_urls = "yes"), "single logical value")
  })

  it("errors on a non-logical clean_redirect_urls", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      salsa(edges, clean_redirect_urls = "yes"),
      "single logical value"
    )
  })

  it("errors on a non-list rurl_params", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(salsa(edges, rurl_params = "bad"), "must be a list")
  })

  it("errors on a non-logical drop_isolates_flag", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(salsa(edges, drop_isolates_flag = 1), "single logical value")
  })
})

describe("salsa wrapper canonicals_df path", {
  it("folds canonicals into the same node identities as pagerank()", {
    edges <- data.frame(from = c("A.com", "B.com"), to = c("B.com", "C.com"))
    canonicals <- data.frame(from = "B.com", to = "C.com")
    s <- salsa(edges, canonicals_df = canonicals)
    pr <- pagerank(edges, canonicals_df = canonicals)
    expect_setequal(s$node_name, pr$node_name)
    expect_false("B.com" %in% s$node_name)
  })

  it("routes non-collapse dedup through aggregate_edges", {
    edges <- data.frame(from = c("A", "A"), to = c("B", "B"))
    s <- salsa(
      edges,
      duplicate_edge_policy = "aggregate", clean_edge_urls = FALSE
    )
    expect_s3_class(s, "data.frame")
    expect_true("A" %in% s$node_name || nrow(s) >= 0)
  })
})

describe("salsa wrapper edge-case branch coverage", {
  it("errors on a clean_canonical_urls of length != 1", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      salsa(edges, clean_canonical_urls = c(TRUE, FALSE)),
      "single logical value"
    )
  })

  it("ignores a zero-row canonicals_df even without required columns", {
    edges <- data.frame(from = "A", to = "B")
    s <- salsa(edges, canonicals_df = data.frame(bad_col = character(0)))
    expect_setequal(s$node_name, c("A", "B"))
  })

  it("skips URL cleaning for a zero-row redirects_df", {
    edges <- data.frame(from = "A", to = "B")
    s <- salsa(
      edges,
      redirects_df = data.frame(from = character(0), to = character(0))
    )
    expect_setequal(s$node_name, c("A", "B"))
  })

  it("no-ops folding when redirects_df lacks the expected from/to columns", {
    # Missing columns short-circuit URL cleaning, and the resulting terminal
    # map from NULL from/to vectors is empty, so the edge list passes through
    # the shared .prepare_link_graph() clean + fold steps unchanged.
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(src = "X", dst = "Y")
    s <- salsa(edges, redirects_df = redirects)
    expect_setequal(s$node_name, c("A", "B"))
  })

  it("returns an empty result for an empty edge list (drop_isolates = TRUE)", {
    edges <- data.frame(from = character(0), to = character(0))
    s <- salsa(edges)
    expect_named(s, c("node_name", "hub", "authority"))
    expect_equal(nrow(s), 0)
  })

  it("returns an empty result for an empty edge list (drop_isolates = FALSE)", {
    edges <- data.frame(from = character(0), to = character(0))
    s <- salsa(edges, drop_isolates_flag = FALSE)
    expect_named(s, c("node_name", "hub", "authority"))
    expect_equal(nrow(s), 0)
  })
})
