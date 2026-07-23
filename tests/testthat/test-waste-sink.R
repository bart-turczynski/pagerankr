context("unified waste-sink mechanism (PAGE-pvfdijrw)")

describe("one sink, applied uniformly to the whole class", {
  it("scores a robots-blocked page identically to a noindex page", {
    # Same graph, same member: with the self-loop trap removed, robots-blocked
    # and noindex both strip outedges and add one edge to the shared sink, so
    # the member's own score must match exactly.
    edges <- data.frame(
      from = c("A", "B", "M", "M"),
      to = c("M", "M", "A", "B")
    )
    ni <- pagerank(
      edges,
      indexability_df = data.frame(url = "M", indexability_status = "noindex"),
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    rb <- pagerank(
      edges,
      indexability_df = data.frame(
        url = "M", indexability_status = "Blocked by robots.txt"
      ),
      robots_blocked_action = "show",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    m_ni <- ni$pagerank[ni$node_name == "M"]
    m_rb <- rb$pagerank[rb$node_name == "M"]
    expect_equal(m_ni, m_rb, tolerance = 1e-9)
  })

  it("does not let a robots-blocked page self-amplify (no rank sink)", {
    # The old self-loop trap parked ~89% of the graph on the blocked page. With
    # the sink, the member cannot hold a majority of the visible mass.
    edges <- data.frame(
      from = c("A", "B", "C", "D", "Blocked"),
      to = c("Blocked", "Blocked", "Blocked", "Blocked", "A")
    )
    pr <- pagerank(
      edges,
      indexability_df = data.frame(
        url = "Blocked", indexability_status = "Blocked by robots.txt"
      ),
      robots_blocked_action = "show",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    blocked <- pr$pagerank[pr$node_name == "Blocked"]
    expect_lt(blocked, 0.5)
    expect_equal(attr(pr, "transition_audit")$mass$total, 1, tolerance = 1e-8)
  })
})

describe("response-dead pages route to the sink (adds one edge)", {
  it("keeps a no-outlink 404 from dangling into teleport", {
    # X is a 404 with NO outlinks. Without the added sink edge it would dangle
    # and recycle its inbound authority to every page via teleport, keeping the
    # visible total at 1. With the sink edge, its throughput evaporates.
    edges <- data.frame(from = c("A", "B"), to = c("X", "A"))
    st <- data.frame(url = c("A", "B", "X"), status_code = c(200L, 200L, 404L))
    pr <- pagerank(edges, status_df = st, clean_edge_urls = FALSE,
      drop_isolates_flag = FALSE)
    au <- attr(pr, "transition_audit")
    expect_gt(au$mass$sink, 0)
    expect_lt(sum(pr$pagerank), 1)
    expect_equal(au$mass$total, 1, tolerance = 1e-8)
    expect_equal(pr$page_state[pr$node_name == "X"], "response_dead")
  })

  it("routes 4xx and 5xx through the same sink (no split)", {
    edges <- data.frame(
      from = c("A", "B", "P", "Q"), to = c("P", "Q", "A", "B")
    )
    st <- data.frame(
      url = c("A", "B", "P", "Q"),
      status_code = c(200L, 200L, 404L, 503L)
    )
    pr <- pagerank(edges, status_df = st, clean_edge_urls = FALSE)
    states <- pr$page_state[match(c("P", "Q"), pr$node_name)]
    expect_equal(states, c("response_dead", "response_dead"))
  })
})

describe("page_state per-URL attribution", {
  it("is absent when neither indexability_df nor status_df is supplied", {
    pr <- pagerank(
      data.frame(from = c("A", "B"), to = c("B", "A")),
      clean_edge_urls = FALSE
    )
    expect_false("page_state" %in% names(pr))
  })

  it("tags live / noindex / robots_blocked / response_dead", {
    edges <- data.frame(
      from = c("Live", "NI", "RB", "Dead"),
      to = c("NI", "RB", "Dead", "Live")
    )
    idx <- data.frame(
      url = c("NI", "RB"),
      indexability_status = c("noindex", "Blocked by robots.txt")
    )
    st <- data.frame(url = "Dead", status_code = 404L)
    pr <- pagerank(
      edges, indexability_df = idx, status_df = st,
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    lookup <- stats::setNames(pr$page_state, pr$node_name)
    expect_equal(unname(lookup["Live"]), "live")
    expect_equal(unname(lookup["NI"]), "noindex")
    expect_equal(unname(lookup["RB"]), "robots_blocked")
    expect_equal(unname(lookup["Dead"]), "response_dead")
  })

  it("applies precedence robots_blocked > response_dead > noindex", {
    # A page carrying conflicting signals takes the highest-precedence label.
    edges <- data.frame(from = c("A", "P"), to = c("P", "A"))
    idx <- data.frame(url = "P", indexability_status = "noindex")
    st <- data.frame(url = "P", status_code = 404L)
    pr <- pagerank(
      edges, indexability_df = idx, status_df = st, clean_edge_urls = FALSE
    )
    expect_equal(pr$page_state[pr$node_name == "P"], "response_dead")

    idx2 <- data.frame(url = "P", indexability_status = "Blocked by robots.txt")
    pr2 <- pagerank(
      edges, indexability_df = idx2, status_df = st, clean_edge_urls = FALSE
    )
    expect_equal(pr2$page_state[pr2$node_name == "P"], "robots_blocked")
  })

  it("does not tag vanished robots-blocked pages (they are removed)", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "RB"))
    idx <- data.frame(url = "RB", indexability_status = "Blocked by robots.txt")
    pr <- pagerank(
      edges, indexability_df = idx, robots_blocked_action = "vanish",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    expect_false("RB" %in% pr$node_name)
    expect_true(all(pr$page_state == "live"))
  })
})

describe("wasted_mass per-URL attribution", {
  it("is absent when neither indexability_df nor status_df is supplied", {
    pr <- pagerank(
      data.frame(from = c("A", "B"), to = c("B", "A")),
      clean_edge_urls = FALSE
    )
    expect_false("wasted_mass" %in% names(pr))
  })

  it("is 0 for live pages and d/(1-d)*score for the waste class", {
    # Live hub feeds each waste page directly so the class carries real inflow.
    edges <- data.frame(
      from = c("H", "A", "B", "A", "B", "H"),
      to = c("A", "H", "H", "NI", "RB", "Dead")
    )
    idx <- data.frame(
      url = c("NI", "RB"),
      indexability_status = c("noindex", "Blocked by robots.txt")
    )
    st <- data.frame(url = "Dead", status_code = 404L)
    d <- 0.85
    pr <- pagerank(
      edges, indexability_df = idx, status_df = st,
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE, damping = d
    )
    expect_true("wasted_mass" %in% names(pr))
    expect_type(pr$wasted_mass, "double")
    live_mask <- pr$page_state == "live"
    expect_true(all(pr$wasted_mass[live_mask] == 0))
    expect_true(all(pr$wasted_mass[!live_mask] > 0))
    expected <- ifelse(live_mask, 0, d / (1 - d) * pr$pagerank)
    expect_equal(pr$wasted_mass, expected, tolerance = 1e-8)
  })

  it("equals damping / (1 - damping) times the page's own score", {
    edges <- data.frame(from = c("A", "P"), to = c("P", "A"))
    st <- data.frame(url = "P", status_code = 404L)
    d <- 0.85
    pr <- pagerank(
      edges, status_df = st, clean_edge_urls = FALSE, damping = d
    )
    row <- pr[pr$node_name == "P", ]
    expect_equal(row$wasted_mass, d / (1 - d) * row$pagerank, tolerance = 1e-8)
  })

  it("sums across the waste class to the evaporated (sink) mass", {
    # With no nofollow evaporation, the sink is fed only by the waste class,
    # so the per-URL shares reconcile exactly with the aggregate mass$sink.
    edges <- data.frame(
      from = c("hub", "hub", "p1", "p2", "p1", "p2", "hub"),
      to = c("p1", "p2", "hub", "hub", "N", "N", "D")
    )
    idx <- data.frame(url = "N", indexability_status = "noindex")
    st <- data.frame(url = "D", status_code = 404L)
    pr <- pagerank(
      edges, indexability_df = idx, status_df = st,
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    sink_mass <- attr(pr, "transition_audit")$mass$sink
    expect_equal(sum(pr$wasted_mass), sink_mass, tolerance = 1e-8)
  })
})

describe("robots_blocked_action argument", {
  it("rejects the removed 'trap' value", {
    expect_error(
      pagerank(
        data.frame(from = "A", to = "B"),
        indexability_df = data.frame(
          url = "A", indexability_status = "Blocked by robots.txt"
        ),
        robots_blocked_action = "trap", clean_edge_urls = FALSE
      ),
      "should be one of"
    )
  })
})
