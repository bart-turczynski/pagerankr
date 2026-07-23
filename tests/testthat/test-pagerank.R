context("pagerank wrapper function")

describe("pagerank main wrapper basic functionality", {
  it("runs end-to-end with sensible defaults", {
    edges <- data.frame(
      from = c("http://A.com/", "B.com", "C.com?q=1"),
      to = c("B.com", "http://A.com", "D.com#frag")
    )
    redirects <- data.frame(
      from = c("C.com?q=1", "B.com"),
      to = c("C-resolved.com", "A.com/")
    )

    # Expected flow: clean URLs, resolve redirects,
    # deduplicate, drop self-loops, drop isolates,
    # then compute PageRank. D should have higher PR
    # than C-resolved since C-resolved links to D.

    pr_full <- pagerank(edges, redirects_df = redirects)

    expect_s3_class(pr_full, "data.frame")
    expect_true(all(
      c("node_name", "pagerank") %in% names(pr_full)
    ))
    if (nrow(pr_full) > 0) {
      expect_equal(
        sum(pr_full$pagerank), 1,
        tolerance = 1e-9
      )
    }

    # After cleaning, resolving, and deduplication:
    # Only C-resolved -> D edge remains.
    # A becomes isolate and is dropped.
    expect_true(nrow(pr_full) %in% c(0, 2))
    if (nrow(pr_full) == 2) {
      expect_true(
        "http://c-resolved.com/" %in% pr_full$node_name
      )
      expect_true(
        "http://d.com/" %in% pr_full$node_name
      )
      pr_d <- pr_full$pagerank[
        pr_full$node_name == "http://d.com/"
      ]
      pr_c_res <- pr_full$pagerank[
        pr_full$node_name == "http://c-resolved.com/"
      ]
      expect_gt(pr_d, pr_c_res)
    }
  })

  it("runs correctly with NULL redirects_df", {
    edges <- data.frame(
      from = "A", to = "B"
    )
    pr <- pagerank(edges, redirects_df = NULL)
    expect_equal(nrow(pr), 2)
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
    expect_true(all(pr$pagerank > 0))
  })

  it("controls URL cleaning flags", {
    edges_dirty <- data.frame(
      from = "HTTP://Example.COM/path?q=1#frag",
      to = "Sub.example.NET"
    )

    pr_clean <- pagerank(edges_dirty, clean_edge_urls = TRUE)
    # rurl normalizes scheme, lowercases the host, strips query/fragment
    # (case_handling = "lower_host"); the path keeps its case.
    expect_true(
      "http://example.com/path" %in% pr_clean$node_name
    )
    expect_true(
      "http://sub.example.net/" %in% pr_clean$node_name
    )

    # Warning if query params present and cleaning off
    expect_warning(
      pr_no_clean <- pagerank(
        edges_dirty,
        clean_edge_urls = FALSE
      ),
      regexp = "URLs in `edge_list_df` may contain"
    )
    expect_true(
      "HTTP://Example.COM/path?q=1#frag" %in%
        pr_no_clean$node_name
    )
    expect_true(
      "Sub.example.NET" %in% pr_no_clean$node_name
    )
  })

  it("passes rurl_params to URL cleaning", {
    edges <- data.frame(
      from = "http://www.example.com/page#section",
      to = "test.net"
    )
    # Empty list to test the rurl_params pathway
    my_rurl_params <- list()
    pr <- pagerank(edges, rurl_params = my_rurl_params)
    # Fragment dropped, scheme added, trailing slash
    expect_true(
      "http://www.example.com/page" %in% pr$node_name
    )
    expect_true(
      "http://test.net/" %in% pr$node_name
    )
  })

  it("controls self_loops argument", {
    edges_sl <- data.frame(
      from = c("http://page.com/a", "http://page.com/b"),
      to = c("http://page.com/a", "http://page.com/a")
    )
    # A->A is dropped, B->A remains
    pr_drop_sl <- pagerank(
      edges_sl,
      self_loops = "drop", drop_isolates_flag = FALSE
    )
    # Node "b" points to "a". "a" has a self-loop.
    # If self-loop "a->a" is dropped, graph is "b" -> "a".
    # "b" becomes a source, "a" a sink. PR("a") > PR("b").
    expect_equal(nrow(pr_drop_sl), 2)
    pr_a <- pr_drop_sl$pagerank[
      pr_drop_sl$node_name == "http://page.com/a"
    ]
    pr_b <- pr_drop_sl$pagerank[
      pr_drop_sl$node_name == "http://page.com/b"
    ]
    expect_gt(pr_a, pr_b)

    # A->A kept, B->A kept.
    pr_keep_sl <- pagerank(
      edges_sl,
      self_loops = "keep", drop_isolates_flag = FALSE
    )
    expect_equal(nrow(pr_keep_sl), 2)
    # Expect different PR values
    expect_false(all(
      pr_drop_sl$pagerank == pr_keep_sl$pagerank
    ))
  })

  it("controls drop_isolates_flag argument", {
    # Scenario: A->B, B->A. C->C (self-loop).
    edges_complex <- rbind(
      data.frame(
        from = "http://page.com/a",
        to = "http://page.com/b"
      ),
      data.frame(
        from = "http://page.com/b",
        to = "http://page.com/a"
      ),
      data.frame(
        from = "http://page.com/c",
        to = "http://page.com/c"
      )
    )

    # Case 1: self_loops="keep", drop_isolates_flag=FALSE
    pr_keep_iso_keep_sl <- pagerank(
      edges_complex,
      self_loops = "keep", drop_isolates_flag = FALSE
    )
    expect_equal(nrow(pr_keep_iso_keep_sl), 3)
    expect_true(all(
      c(
        "http://page.com/a",
        "http://page.com/b",
        "http://page.com/c"
      ) %in%
        pr_keep_iso_keep_sl$node_name
    ))

    # Case 2: self_loops="drop", drop_isolates_flag=FALSE
    # C->C is dropped. C becomes an isolate but is KEPT.
    pr_keep_iso_drop_sl <- pagerank(
      edges_complex,
      self_loops = "drop", drop_isolates_flag = FALSE
    )
    expect_equal(nrow(pr_keep_iso_drop_sl), 3)
    pr_c <- pr_keep_iso_drop_sl$pagerank[
      pr_keep_iso_drop_sl$node_name == "http://page.com/c"
    ]
    pr_a <- pr_keep_iso_drop_sl$pagerank[
      pr_keep_iso_drop_sl$node_name == "http://page.com/a"
    ]
    expect_lt(pr_c, pr_a)

    # Case 3: self_loops="drop", drop_isolates_flag=TRUE
    # C becomes isolate and is DROPPED.
    pr_drop_iso_drop_sl <- pagerank(
      edges_complex,
      self_loops = "drop", drop_isolates_flag = TRUE
    )
    expect_equal(nrow(pr_drop_iso_drop_sl), 2)
    expect_false(
      "http://page.com/c" %in% pr_drop_iso_drop_sl$node_name
    )
  })

  it("handles custom column names for edges and redirects", {
    edges_cust <- data.frame(
      source = "X.com", target = "Y.net"
    )
    redirects_cust <- data.frame(
      orig = "Y.net", final = "Z.org"
    )
    pr <- pagerank(
      edges_cust,
      redirects_df = redirects_cust,
      edge_from_col = "source", edge_to_col = "target",
      redirect_from_col = "orig", redirect_to_col = "final"
    )
    expect_equal(nrow(pr), 2)
    # rurl adds http:// and trailing slash for bare domains, lowercases host
    expect_true("http://x.com/" %in% pr$node_name)
    expect_true("http://z.org/" %in% pr$node_name)
  })

  it("passes ... (e.g. damping) to compute_pagerank", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    pr_d70 <- pagerank(edges, damping = 0.70)
    expect_equal(
      pr_d70$pagerank, c(0.5, 0.5),
      tolerance = 1e-9, ignore_attr = TRUE
    )
  })
})

describe("pagerank wrapper warnings and errors", {
  it("warns if clean_edge_urls=FALSE and query params exist", {
    edges_query <- data.frame(
      from = "example.com/page?q=1", to = "other.com"
    )
    expect_warning(
      pagerank(edges_query, clean_edge_urls = FALSE),
      "URLs in `edge_list_df` may contain query parameters"
    )
    # No warning if no query params
    edges_no_query <- data.frame(
      from = "example.com/page", to = "other.com"
    )
    # NA means no warning expected
    expect_warning(
      pagerank(edges_no_query, clean_edge_urls = FALSE),
      regexp = NA
    )
  })

  it("errors on invalid main arguments", {
    bad_df <- data.frame(f = "a", t = "b")
    expect_error(pagerank(list()))
    expect_error(pagerank(bad_df, redirects_df = list()))
    expect_error(pagerank(bad_df, clean_edge_urls = "true"))
    expect_error(pagerank(bad_df, rurl_params = "param=val"))
    expect_error(pagerank(bad_df, self_loops = "maybe"))
    expect_error(pagerank(bad_df, drop_isolates_flag = 0))
  })
})


describe("pagerank shared memoization for cleaning (conceptual)", {
  it("uses shared memoizer when both cleaning flags are TRUE", {
    # Shared memoization ensures the same raw URL is cleaned once
    # and cached across edges and redirects tables.

    # Scenario 1: Same raw URL in edges (from) and redirects (from).
    edges_df <- data.frame(
      from = "HTTP://SiteA.com/path",
      to = "SiteB.com"
    )
    redirects_df <- data.frame(
      from = "HTTP://SiteA.com/path",
      to = "SiteA_RESOLVED.com"
    )
    pr_shared <- pagerank(
      edges_df, redirects_df,
      clean_edge_urls = TRUE,
      clean_redirect_urls = TRUE
    )
    expect_true(
      "http://sitea_resolved.com/" %in% pr_shared$node_name
    )
    expect_true(
      "http://siteb.com/" %in% pr_shared$node_name
    )

    # Scenario 2: URL from edge `to` matches redirect `to`.
    edges_df2 <- data.frame(
      from = "Source.com",
      to = "HTTP://CommonTarget.com/Page"
    )
    redirects_df2 <- data.frame(
      from = "RedirectFrom.com",
      to = "HTTP://CommonTarget.com/Page"
    )
    pr_shared2 <- pagerank(
      edges_df2, redirects_df2,
      clean_edge_urls = TRUE,
      clean_redirect_urls = TRUE
    )
    expect_true(
      "http://source.com/" %in% pr_shared2$node_name
    )
    expect_true(
      "http://commontarget.com/Page" %in% pr_shared2$node_name
    )
  })
})

describe("pagerank nofollow handling", {
  it("nofollow_action='keep' treats nofollow edges as follow", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "A"),
      nf = c(FALSE, TRUE, FALSE)
    )
    pr_keep <- pagerank(edges,
      nofollow_col = "nf", nofollow_action = "keep",
      clean_edge_urls = FALSE
    )
    # All 3 nodes, PR sums to 1
    expect_equal(nrow(pr_keep), 3)
    expect_equal(sum(pr_keep$pagerank), 1, tolerance = 1e-9)
  })

  it("nofollow_action='drop' removes nofollow edges", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "A"),
      nf = c(FALSE, TRUE, FALSE)
    )
    pr_drop <- pagerank(edges,
      nofollow_col = "nf", nofollow_action = "drop",
      clean_edge_urls = FALSE
    )
    # C should get no direct link, but still present if drop_isolates_flag=TRUE
    # After dropping A->C: edges are A->B, B->A. C has no edges.
    # drop_isolates_flag=TRUE (default): C is dropped
    expect_equal(nrow(pr_drop), 2)
    expect_true(all(c("A", "B") %in% pr_drop$node_name))
    expect_false("C" %in% pr_drop$node_name)
    expect_equal(sum(pr_drop$pagerank), 1, tolerance = 1e-9)
  })

  it("nofollow_action='evaporate' redirects nofollow to sink (PR < 1)", {
    # A -> B (follow), A -> C (nofollow), B -> A (follow)
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "A"),
      nf = c(FALSE, TRUE, FALSE)
    )
    pr_evap <- pagerank(
      edges,
      nofollow_col = "nf",
      nofollow_action = "evaporate",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    # Sink node should NOT appear in results
    expect_false("__pr_waste_sink__" %in% pr_evap$node_name)
    # C gets no PR from A (redirected to sink),
    # but C is still in results as isolate
    # PR sum should be less than 1 (evaporated portion went to sink)
    expect_lt(sum(pr_evap$pagerank), 1)
    # C should have very low PR (only teleportation)
    pr_c <- pr_evap$pagerank[pr_evap$node_name == "C"]
    pr_a <- pr_evap$pagerank[pr_evap$node_name == "A"]
    expect_gt(pr_a, pr_c)
  })

  it("evaporate mode: all edges nofollow means all PR evaporates", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      nf = c(TRUE, TRUE)
    )
    pr <- pagerank(edges,
      nofollow_col = "nf", nofollow_action = "evaporate",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    expect_true(all(c("A", "B") %in% pr$node_name))
    # Sum should be well below 1 since all link value goes to sink
    expect_lt(sum(pr$pagerank), 1)
  })

  it("nofollow with no nofollow edges behaves normally", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      nf = c(FALSE, FALSE)
    )
    pr <- pagerank(edges,
      nofollow_col = "nf", nofollow_action = "evaporate",
      clean_edge_urls = FALSE
    )
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
  })

  it("errors on invalid nofollow_col", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      pagerank(edges, nofollow_col = "missing_col", clean_edge_urls = FALSE),
      "not found"
    )
  })
})

describe("pagerank indexability handling", {
  it("noindex pages route their outgoing budget to the sink", {
    # A -> B, A -> C. A is noindex.
    # All of A's outgoing edges are replaced by one edge to the waste sink.
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "A")
    )
    idx_df <- data.frame(
      url = "A",
      indexability_status = "noindex"
    )
    pr <- pagerank(edges,
      indexability_df = idx_df,
      nofollow_action = "evaporate",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    expect_false("__pr_waste_sink__" %in% pr$node_name)
    expect_equal(pr$page_state[pr$node_name == "A"], "noindex")
    # A passes nothing to B or C; B still links to A, so A gets some PR.
    # PR sums to < 1 because A's throughput evaporates to the sink.
    expect_lt(sum(pr$pagerank), 1)
  })

  it("noindex is decoupled from nofollow_action (still routes under drop)", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "A")
    )
    idx_df <- data.frame(
      url = "A",
      indexability_status = "noindex"
    )
    pr <- pagerank(edges,
      indexability_df = idx_df,
      nofollow_action = "drop",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    # nofollow_action = "drop" governs only real rel=nofollow edges; noindex
    # still routes to the sink rather than dropping A's edges, so mass
    # evaporates (sum < 1) instead of dangling back via teleport (sum == 1).
    expect_true(all(c("A", "B", "C") %in% pr$node_name))
    expect_lt(sum(pr$pagerank), 1)
    # A is noindex, so under the default prior_exclude_waste it gets no teleport
    # share while the live pages keep theirs. A now collects only B's damped
    # throughput, so a noindex page no longer outranks a live page merely for
    # existing (PAGE-bcpacnfm). Under prior_exclude_waste = FALSE (uniform
    # teleport) the ordering flips back — A's teleport share puts it on top.
    pr_c <- pr$pagerank[pr$node_name == "C"]
    pr_a <- pr$pagerank[pr$node_name == "A"]
    expect_lt(pr_a, pr_c)
    pr_uniform <- pagerank(edges,
      indexability_df = idx_df,
      nofollow_action = "drop",
      prior_exclude_waste = FALSE,
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    expect_gt(
      pr_uniform$pagerank[pr_uniform$node_name == "A"],
      pr_uniform$pagerank[pr_uniform$node_name == "C"]
    )
  })

  it("robots-blocked pages are shown, routing throughput to the sink", {
    # A -> B, B -> Blocked, Blocked -> C
    # Blocked is robots-blocked: outgoing edges replaced by one edge to the
    # waste sink (no self-loop trap), and the page itself is shown.
    edges <- data.frame(
      from = c("A", "B", "Blocked"),
      to = c("B", "Blocked", "C")
    )
    idx_df <- data.frame(
      url = "Blocked",
      indexability_status = "Blocked by robots.txt"
    )
    pr_show <- pagerank(edges,
      indexability_df = idx_df,
      robots_blocked_action = "show",
      clean_edge_urls = FALSE, drop_isolates_flag = TRUE
    )
    # Blocked appears, showing the authority it collects.
    expect_true("Blocked" %in% pr_show$node_name)
    expect_equal(
      pr_show$page_state[pr_show$node_name == "Blocked"], "robots_blocked"
    )
    # C had only Blocked -> C, which was replaced; with drop_isolates it goes.
    expect_false("C" %in% pr_show$node_name)
    # Blocked passes its authority to the sink, so visible scores sum to < 1
    # (no self-loop trap that would keep the total at 1 by absorbing it).
    expect_lt(sum(pr_show$pagerank), 1)
    expect_equal(attr(pr_show, "transition_audit")$mass$total, 1,
      tolerance = 1e-8)
  })

  it("robots-blocked pages vanish from results", {
    edges <- data.frame(
      from = c("A", "B", "Blocked"),
      to = c("B", "Blocked", "C")
    )
    idx_df <- data.frame(
      url = "Blocked",
      indexability_status = "Blocked by robots.txt"
    )
    pr_vanish <- pagerank(edges,
      indexability_df = idx_df,
      robots_blocked_action = "vanish",
      clean_edge_urls = FALSE, drop_isolates_flag = TRUE
    )
    # Blocked should NOT appear in results
    expect_false("Blocked" %in% pr_vanish$node_name)
    # PR sum < 1 because Blocked's share vanished
    expect_lt(sum(pr_vanish$pagerank), 1)
  })

  it("robots.txt takes priority over noindex", {
    edges <- data.frame(
      from = c("A", "Both"),
      to = c("Both", "C")
    )
    idx_df <- data.frame(
      url = "Both",
      indexability_status = "Blocked by robots.txt,noindex"
    )
    # Should be treated as robots-blocked (outgoing edges routed to the sink),
    # not noindex — but since both now route to the same sink, the observable
    # difference is only the page_state label.
    pr <- pagerank(edges,
      indexability_df = idx_df,
      robots_blocked_action = "show",
      clean_edge_urls = FALSE, drop_isolates_flag = TRUE
    )
    # "Both" is shown as robots_blocked; C loses its only inbound edge.
    expect_true("Both" %in% pr$node_name)
    expect_equal(pr$page_state[pr$node_name == "Both"], "robots_blocked")
    expect_false("C" %in% pr$node_name)
  })

  it("comma-separated status parsing works", {
    idx_df <- data.frame(
      url = c("PageA", "PageB", "PageC"),
      indexability_status = c(
        "Canonicalised,noindex",
        "Blocked by robots.txt",
        "indexable"
      )
    )
    edges <- data.frame(
      from = c("PageA", "PageB", "PageC"),
      to = c("X", "Y", "Z")
    )
    pr <- pagerank(edges,
      indexability_df = idx_df,
      nofollow_action = "drop",
      robots_blocked_action = "show",
      clean_edge_urls = FALSE, drop_isolates_flag = TRUE
    )
    # PageA is noindex: its outgoing edge is routed to the sink (noindex is
    #   decoupled from nofollow_action, so "drop" does not apply to it).
    # PageB is robots-blocked: outgoing edge routed to the sink, page shown.
    # PageC is indexable: normal.
    expect_true("PageB" %in% pr$node_name)
    expect_equal(pr$page_state[pr$node_name == "PageB"], "robots_blocked")
    expect_true("PageC" %in% pr$node_name) # normal
    expect_true("Z" %in% pr$node_name) # PageC -> Z works
    # X was PageA's only inbound; PageA -> X is replaced by PageA -> sink, so X
    # has no inbound edge and is dropped as an isolate.
    expect_false("X" %in% pr$node_name)
  })
})

describe("pagerank weight_col passthrough", {
  it("passes weight_col through to compute_pagerank", {
    edges <- data.frame(
      from = c("A", "A", "B", "C"),
      to = c("B", "C", "A", "A"),
      w = c(1, 10, 1, 1)
    )
    pr <- pagerank(edges, weight_col = "w", clean_edge_urls = FALSE)
    expect_equal(nrow(pr), 3)
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
    # C should get more PR than B due to higher weight on A->C
    pr_c <- pr$pagerank[pr$node_name == "C"]
    pr_b <- pr$pagerank[pr$node_name == "B"]
    expect_gt(pr_c, pr_b)
  })

  it("weight_col = NULL behaves like unweighted", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "A")
    )
    pr <- pagerank(edges, weight_col = NULL, clean_edge_urls = FALSE)
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
  })
})

describe("pagerank isolate handling with partial rows", {
  it("drop_isolates_flag=FALSE includes nodes from partial rows as isolates", {
    # Edge list with partial rows: "Orphan" and "Dead" are known URLs
    # but don't participate in complete edges.
    edges <- data.frame(
      from = c("A", "B", NA, "Dead"),
      to = c("B", "A", "Orphan", NA)
    )
    # Disable URL cleaning to keep URLs as-is for predictable results
    pr <- pagerank(edges, clean_edge_urls = FALSE, drop_isolates_flag = FALSE)

    expect_s3_class(pr, "data.frame")
    expect_true(all(c("node_name", "pagerank") %in% names(pr)))
    # All 4 unique nodes should appear: A, B, Dead, Orphan
    expect_equal(nrow(pr), 4)
    expect_true(all(c("A", "B", "Dead", "Orphan") %in% pr$node_name))
    # PageRank should still sum to 1
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
    # Isolates get lower PageRank than connected nodes
    pr_a <- pr$pagerank[pr$node_name == "A"]
    pr_orphan <- pr$pagerank[pr$node_name == "Orphan"]
    expect_gt(pr_a, pr_orphan)
  })

  it("drop_isolates_flag=TRUE excludes nodes from partial rows", {
    edges <- data.frame(
      from = c("A", "B", NA, "Dead"),
      to = c("B", "A", "Orphan", NA)
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE, drop_isolates_flag = TRUE)

    expect_s3_class(pr, "data.frame")
    # Only A, B should appear (connected via complete edges)
    expect_equal(nrow(pr), 2)
    expect_true(all(c("A", "B") %in% pr$node_name))
    expect_false("Dead" %in% pr$node_name)
    expect_false("Orphan" %in% pr$node_name)
    expect_equal(sum(pr$pagerank), 1, tolerance = 1e-9)
  })

  it("drop_isolates_flag produces different results with partial rows", {
    edges <- data.frame(
      from = c("X", "Y", "IsolatedNode"),
      to = c("Y", "X", NA)
    )
    pr_keep <- pagerank(
      edges,
      clean_edge_urls = FALSE,
      drop_isolates_flag = FALSE
    )
    pr_drop <- pagerank(
      edges,
      clean_edge_urls = FALSE,
      drop_isolates_flag = TRUE
    )

    expect_equal(nrow(pr_keep), 3) # X, Y, IsolatedNode
    expect_equal(nrow(pr_drop), 2) # X, Y only
    expect_true("IsolatedNode" %in% pr_keep$node_name)
    expect_false("IsolatedNode" %in% pr_drop$node_name)
  })
})

describe("pagerank validation coverage", {
  it("errors on invalid clean_redirect_urls", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      pagerank(edges, clean_redirect_urls = "yes"),
      "single logical"
    )
  })

  it("errors on invalid rurl_params", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(pagerank(edges, rurl_params = "bad"), "must be a list")
  })

  it("errors on invalid drop_isolates_flag", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      pagerank(edges, drop_isolates_flag = "yes"),
      "single logical"
    )
  })

  it("errors on invalid weight_col type", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      pagerank(edges, weight_col = 123, clean_edge_urls = FALSE),
      "single character string"
    )
  })

  it("errors when weight_col not found", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      pagerank(edges,
        weight_col = "missing",
        clean_edge_urls = FALSE
      ),
      "not found"
    )
  })

  it("errors on invalid nofollow_col type", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      pagerank(edges, nofollow_col = TRUE, clean_edge_urls = FALSE),
      "single character string"
    )
  })

  it("errors when nofollow_col not found", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      pagerank(edges,
        nofollow_col = "missing",
        clean_edge_urls = FALSE
      ),
      "not found"
    )
  })

  it("errors on invalid indexability_df type", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      pagerank(edges,
        indexability_df = "bad",
        clean_edge_urls = FALSE
      ),
      "data frame or NULL"
    )
  })

  it("errors when indexability_url_col not found", {
    edges <- data.frame(from = "A", to = "B")
    idx <- data.frame(
      wrong = "A", indexability_status = "noindex"
    )
    expect_error(
      pagerank(edges,
        indexability_df = idx,
        clean_edge_urls = FALSE
      ),
      "not found"
    )
  })

  it("errors when indexability_status_col not found", {
    edges <- data.frame(
      from = "A", to = "B"
    )
    idx <- data.frame(
      url = "A", wrong = "noindex"
    )
    expect_error(
      pagerank(edges,
        indexability_df = idx,
        clean_edge_urls = FALSE
      ),
      "not found"
    )
  })
})

describe("pagerank robots-blocked with extra columns", {
  it("fills extra columns correctly on the member -> sink edge", {
    edges <- data.frame(
      from = c("A", "Blocked"),
      to = c("Blocked", "C"),
      weight = c(5, 10),
      nofollow = c(FALSE, FALSE),
      label = c("x", "y")
    )
    idx_df <- data.frame(
      url = "Blocked",
      indexability_status = "Blocked by robots.txt"
    )
    pr <- pagerank(edges,
      indexability_df = idx_df,
      robots_blocked_action = "show",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    # Blocked is shown; the synthetic Blocked -> sink edge rbinds cleanly even
    # though the edge list carries extra numeric / logical / character columns.
    expect_true("Blocked" %in% pr$node_name)
  })
})

describe("pagerank nofollow evaporate with extra columns", {
  it("fills extra columns correctly on sink row", {
    edges <- data.frame(
      from = c("A", "A"),
      to = c("B", "C"),
      weight = c(1, 2),
      nf = c(FALSE, TRUE),
      tag = c("keep", "drop")
    )
    pr <- pagerank(edges,
      nofollow_col = "nf", nofollow_action = "evaporate",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    expect_false("__pr_waste_sink__" %in% pr$node_name)
    expect_lt(sum(pr$pagerank), 1)
  })
})

describe("pagerank noindex routes to the sink without a nofollow column", {
  it("handles noindex when no nofollow_col is supplied", {
    edges <- data.frame(
      from = c("NI", "B"),
      to = c("B", "NI")
    )
    idx_df <- data.frame(
      url = "NI",
      indexability_status = "noindex"
    )
    # noindex no longer needs (or creates) a nofollow column: it routes NI's
    # outgoing edge straight to the waste sink.
    pr <- pagerank(edges,
      indexability_df = idx_df,
      nofollow_action = "evaporate",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    expect_false("__pr_waste_sink__" %in% pr$node_name)
    expect_equal(pr$page_state[pr$node_name == "NI"], "noindex")
    # NI's outgoing edge routes to the sink, so PR < 1
    expect_lt(sum(pr$pagerank), 1)
  })
})

describe("pagerank numeric nofollow column coercion", {
  it("handles numeric nofollow column (0/1)", {
    edges <- data.frame(
      from = c("A", "A"),
      to = c("B", "C"),
      nf = c(0, 1)
    )
    pr <- pagerank(edges,
      nofollow_col = "nf", nofollow_action = "drop",
      clean_edge_urls = FALSE, drop_isolates_flag = TRUE
    )
    # The nofollow edge (A->C) is dropped, only A->B remains
    expect_true("A" %in% pr$node_name)
    expect_true("B" %in% pr$node_name)
    expect_false("C" %in% pr$node_name)
  })
})

# =============================================================================
# Domain filtering passthrough tests
# =============================================================================

describe("pagerank() keep_domains parameter", {
  it("filters to internal links only", {
    edges <- data.frame(
      from = c(
        "http://example.com/a", "http://example.com/b",
        "http://other.com/c"
      ),
      to = c(
        "http://example.com/b", "http://other.com/d",
        "http://example.com/a"
      )
    )
    pr <- pagerank(edges, keep_domains = "example.com")
    # Only example.com nodes should appear
    expect_true(all(grepl("example.com", pr$node_name)))
  })

  it("returns fewer nodes than without filter", {
    edges <- data.frame(
      from = c("http://a.com/1", "http://a.com/2", "http://b.com/1"),
      to = c("http://a.com/2", "http://b.com/1", "http://a.com/1")
    )
    pr_all <- pagerank(edges)
    pr_filtered <- pagerank(edges, keep_domains = "a.com")
    expect_lte(nrow(pr_filtered), nrow(pr_all))
  })
})

describe("pagerank() exclude_domains parameter", {
  it("removes edges involving excluded domains", {
    edges <- data.frame(
      from = c(
        "http://example.com/a", "http://example.com/b",
        "http://spam.com/x"
      ),
      to = c(
        "http://example.com/b", "http://spam.com/y",
        "http://example.com/a"
      )
    )
    pr <- pagerank(edges, exclude_domains = "spam.com")
    # spam.com nodes should not appear
    expect_false(any(grepl("spam.com", pr$node_name)))
  })
})

describe("pagerank() domain filtering with NULL (default)", {
  it("no filtering when both are NULL", {
    edges <- data.frame(
      from = c("http://a.com/1", "http://b.com/1"),
      to = c("http://b.com/1", "http://a.com/1")
    )
    pr <- pagerank(edges)
    expect_equal(nrow(pr), 2)
  })
})

describe("pagerank() host-level filtering", {
  edges <- data.frame(
    from = c("http://www.ex.com/a", "http://www.ex.com/a"),
    to = c("http://www.ex.com/b", "http://cdn.ex.com/c")
  )

  it("keep_hosts restricts to an exact host", {
    pr <- pagerank(edges, keep_hosts = "www.ex.com")
    expect_setequal(
      pr$node_name,
      c("http://www.ex.com/a", "http://www.ex.com/b")
    )
  })

  it("exclude_hosts drops edges touching a host", {
    pr <- pagerank(edges, exclude_hosts = "cdn.ex.com")
    expect_false(any(grepl("cdn.ex.com", pr$node_name)))
    expect_equal(nrow(pr), 2)
  })

  it("host filtering folds IDN under a matching host_encoding", {
    idn <- data.frame(
      from = c("http://münchen.de/a", "http://münchen.de/a"),
      to = c("http://xn--mnchen-3ya.de/b", "http://other.de/c")
    )
    # With idna folding, the Punycode `to` endpoint matches the Unicode
    # keep_hosts value, so that edge survives (2 nodes).
    folded <- pagerank(
      idn,
      keep_hosts = "münchen.de", rurl_params = list(host_encoding = "idna")
    )
    expect_equal(nrow(folded), 2)
    # Without folding, the Punycode endpoint does not match -> dropped.
    unfolded <- pagerank(idn, keep_hosts = "münchen.de")
    expect_equal(nrow(unfolded), 0)
  })
})

describe("pagerank() IDN host_encoding", {
  edges <- data.frame(
    from = c("http://münchen.de/a", "http://xn--mnchen-3ya.de/a"),
    to = c("http://example.com/x", "http://example.com/y")
  )

  it("keeps IDN spellings distinct by default (host_encoding = keep)", {
    pr <- pagerank(edges)
    expect_true(all(c(
      "http://münchen.de/a", "http://xn--mnchen-3ya.de/a"
    ) %in% pr$node_name))
  })

  it("folds IDN spellings to one node under host_encoding = idna", {
    pr <- pagerank(edges, rurl_params = list(host_encoding = "idna"))
    expect_false("http://münchen.de/a" %in% pr$node_name)
    expect_true("http://xn--mnchen-3ya.de/a" %in% pr$node_name)
    # München folded into one node; example.com/x and /y remain distinct
    expect_equal(nrow(pr), 3)
  })
})

test_that("damping is a first-class argument: validated and effective", {
  edges <- data.frame(
    from = c("A", "B", "C", "A", "D"),
    to = c("B", "C", "A", "C", "A")
  )

  # Validation at the wrapper (not just compute_pagerank).
  expect_error(pagerank(edges, damping = 1.5), "between 0 and 1")
  expect_error(pagerank(edges, damping = -0.1), "between 0 and 1")
  expect_error(pagerank(edges, damping = c(0.8, 0.9)), "single numeric")
  expect_error(pagerank(edges, damping = NA_real_), "single numeric")

  # The factor actually changes the scores.
  low <- pagerank(edges, damping = 0.50, clean_edge_urls = FALSE)
  high <- pagerank(edges, damping = 0.95, clean_edge_urls = FALSE)
  m <- merge(low, high, by = "node_name", suffixes = c("_lo", "_hi"))
  expect_false(isTRUE(all.equal(m$pagerank_lo, m$pagerank_hi)))
  # Default matches an explicit 0.85.
  expect_equal(
    pagerank(edges, clean_edge_urls = FALSE)$pagerank,
    pagerank(edges, damping = 0.85, clean_edge_urls = FALSE)$pagerank
  )
})
