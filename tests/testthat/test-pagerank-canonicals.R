context("pagerank() first-class canonical support")

describe("pagerank canonical folding integration", {
  edges <- data.frame(
    from = c("http://x/", "http://y/", "http://a/"),
    to   = c("http://a/", "http://a/", "http://z/"),
    stringsAsFactors = FALSE
  )

  it("folds a cross-canonical source into its canonical target", {
    can <- data.frame(from = "http://a/", to = "http://c/")
    pr <- pagerank(edges, canonicals_df = can, drop_isolates_flag = FALSE)
    nodes <- sort(pr[[1]])
    expect_true("http://c/" %in% nodes)
    expect_false("http://a/" %in% nodes)
  })

  it("preserves prior behaviour when canonicals_df is NULL", {
    pr_no_canon <- pagerank(edges, drop_isolates_flag = FALSE)
    pr_null <- pagerank(edges, canonicals_df = NULL, drop_isolates_flag = FALSE)
    expect_equal(pr_no_canon, pr_null)
  })

  it("redirect-only result is identical whether or not the arg is named", {
    r <- data.frame(from = "http://a/", to = "http://z/")
    pr1 <- pagerank(edges, redirects_df = r, drop_isolates_flag = FALSE)
    pr2 <- pagerank(edges,
      redirects_df = r, canonicals_df = NULL,
      drop_isolates_flag = FALSE
    )
    expect_equal(pr1, pr2)
  })

  it("self-canonicals are dropped (no effect on the graph)", {
    can <- data.frame(from = "http://a/", to = "http://a/")
    pr_self <- pagerank(edges, canonicals_df = can, drop_isolates_flag = FALSE)
    pr_none <- pagerank(edges, drop_isolates_flag = FALSE)
    expect_equal(pr_self, pr_none)
  })

  it("redirect wins over a conflicting canonical by default", {
    r <- data.frame(from = "http://a/", to = "http://b/")
    can <- data.frame(from = "http://a/", to = "http://d/")
    pr <- pagerank(edges,
      redirects_df = r, canonicals_df = can,
      drop_isolates_flag = FALSE
    )
    nodes <- pr[[1]]
    expect_true("http://b/" %in% nodes)
    expect_false("http://d/" %in% nodes)
    expect_false("http://a/" %in% nodes)
  })

  it("canonical_wins routes the conflicting source to the canonical", {
    r <- data.frame(from = "http://a/", to = "http://b/")
    can <- data.frame(from = "http://a/", to = "http://d/")
    pr <- pagerank(edges,
      redirects_df = r, canonicals_df = can,
      canonical_conflict_policy = "canonical_wins",
      drop_isolates_flag = FALSE
    )
    nodes <- pr[[1]]
    expect_true("http://d/" %in% nodes)
    expect_false("http://b/" %in% nodes)
  })

  it("canonical_conflict_policy='error' surfaces the disagreement", {
    r <- data.frame(from = "http://a/", to = "http://b/")
    can <- data.frame(from = "http://a/", to = "http://d/")
    expect_error(
      pagerank(edges,
        redirects_df = r, canonicals_df = can,
        canonical_conflict_policy = "error", drop_isolates_flag = FALSE
      ),
      "conflict"
    )
  })

  it("cleans canonical URLs through the same rurl profile as edges", {
    # The query string on the canonical source/target must be stripped so it
    # lands on the same node identity as the cleaned edges.
    can <- data.frame(from = "http://a/?utm=1", to = "http://c/?ref=x")
    pr <- pagerank(edges, canonicals_df = can, drop_isolates_flag = FALSE)
    nodes <- pr[[1]]
    expect_true("http://c/" %in% nodes)
    expect_false(any(grepl("\\?", nodes)))
  })
})

describe("acceptance contract: composed-map parity", {
  edges <- data.frame(
    from = c("http://x/", "http://y/", "http://b/"),
    to   = c("http://a/", "http://b/", "http://q/"),
    stringsAsFactors = FALSE
  )
  redirects <- data.frame(from = "http://b/", to = "http://c/")
  canonicals <- data.frame(from = "http://a/", to = "http://b/")

  it("exported map applied to edges matches pagerank's internal vertex set", {
    fm <- build_fold_map(redirects_df = redirects, canonicals_df = canonicals)
    map <- stats::setNames(fm$to, fm$from)

    fold <- function(u) ifelse(u %in% names(map), unname(map[u]), u)
    expected_nodes <- sort(unique(c(fold(edges$from), fold(edges$to))))

    pr <- pagerank(edges,
      redirects_df = redirects, canonicals_df = canonicals,
      self_loops = "keep", drop_isolates_flag = FALSE
    )
    expect_equal(sort(pr[[1]]), expected_nodes)
  })

  it("TIPR prior folds through the same composed map as the edges", {
    fm <- build_fold_map(redirects_df = redirects, canonicals_df = canonicals)
    map <- stats::setNames(fm$to, fm$from)

    # Prior on http://a/ should land on a's composed representative.
    prior <- data.frame(url = "http://a/", weight = 100)
    pr <- pagerank(edges,
      redirects_df = redirects, canonicals_df = canonicals,
      prior_df = prior, prior_alpha = 0, prior_verbose = FALSE,
      self_loops = "keep", drop_isolates_flag = FALSE
    )
    rep_a <- unname(map[["http://a/"]])
    pw <- pr$prior_weight[pr[[1]] == rep_a]
    expect_true(length(pw) == 1 && pw > 0)
    # No other vertex carries the prior weight.
    expect_equal(sum(pr$prior_weight > 0), 1)
  })
})
