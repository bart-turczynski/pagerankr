# Tests for scope-aware folding (out_of_scope_fold, PAGE-ttlaxjkw).
# An "out-of-scope fold" is a composed fold-map entry whose TARGET is not a
# crawled node; folding it relabels a crawled page onto a phantom vertex.

describe("out_of_scope_fold policy", {
  # Crawled node set = {x, y, a, z}. The canonical a -> c points at the
  # uncrawled c, so a -> c is an out-of-scope fold.
  edges <- data.frame(
    from = c("http://x/", "http://y/", "http://a/"),
    to   = c("http://a/", "http://a/", "http://z/")
  )
  can <- data.frame(from = "http://a/", to = "http://c/")

  it("defaults to relabel: preserves today's cross-scope folding", {
    pr_default <- pagerank(edges,
      canonicals_df = can, drop_isolates_flag = FALSE
    )
    pr_relabel <- pagerank(edges,
      canonicals_df = can, out_of_scope_fold = "relabel",
      drop_isolates_flag = FALSE
    )
    # Default equals explicit "relabel".
    expect_equal(pr_default, pr_relabel)
    nodes <- pr_default[[1]]
    # a was relabeled onto the uncrawled target c.
    expect_true("http://c/" %in% nodes)
    expect_false("http://a/" %in% nodes)
  })

  it("keep: crawled source retains its as-crawled identity", {
    pr_keep <- pagerank(edges,
      canonicals_df = can, out_of_scope_fold = "keep",
      drop_isolates_flag = FALSE
    )
    nodes <- pr_keep[[1]]
    # The out-of-scope target does NOT appear as a vertex.
    expect_false("http://c/" %in% nodes)
    # The crawled source node survives under its own identity.
    expect_true("http://a/" %in% nodes)
  })

  it("keep folds the TIPR prior through the same filtered map", {
    # Prior on the crawled source a; under keep, a stays a, so the prior lands
    # on a (not on the dropped phantom c).
    prior <- data.frame(url = "http://a/", weight = 100)
    pr_keep <- pagerank(edges,
      canonicals_df = can, out_of_scope_fold = "keep",
      prior_df = prior, prior_alpha = 0, prior_verbose = FALSE,
      drop_isolates_flag = FALSE
    )
    pw <- pr_keep$prior_weight[pr_keep[[1]] == "http://a/"]
    expect_length(pw, 1)
    expect_gt(pw, 0)
    expect_false("http://c/" %in% pr_keep[[1]])
  })

  it("keep still applies in-scope folds", {
    # a -> z is in scope (z is crawled); keep must still fold it, only skipping
    # the out-of-scope a -> c. Use two canonicals: one in-scope, one out.
    edges2 <- data.frame(
      from = c("http://p/", "http://q/"),
      to   = c("http://z/", "http://a/")
    )
    can2 <- data.frame(
      from = c("http://p/", "http://q/"),
      to   = c("http://z/", "http://out/")
    )
    pr_keep <- pagerank(edges2,
      canonicals_df = can2, out_of_scope_fold = "keep",
      drop_isolates_flag = FALSE
    )
    nodes <- pr_keep[[1]]
    # In-scope fold applied: p folded onto crawled z.
    expect_false("http://p/" %in% nodes)
    expect_true("http://z/" %in% nodes)
    # Out-of-scope fold skipped: q kept, phantom out absent.
    expect_true("http://q/" %in% nodes)
    expect_false("http://out/" %in% nodes)
  })

  it("leak: routes the out-of-scope source's inbound equity out of the graph", {
    pr_leak <- pagerank(edges,
      canonicals_df = can, out_of_scope_fold = "leak",
      drop_isolates_flag = FALSE
    )
    nodes <- pr_leak[[1]]
    # The out-of-scope target is NOT invented as a vertex.
    expect_false("http://c/" %in% nodes)
    # The leaking source itself does not rank (routed onto the leak sink).
    expect_false("http://a/" %in% nodes)
    # The synthetic leak sink is internal and never reported.
    expect_false("__pr_leak_sink__" %in% nodes)
    # z survives but is NOT credited the leaked inbound equity (a -> z dropped),
    # so its only mass is teleport -- strictly less than under relabel/keep,
    # where a passes equity to z.
    pr_keep <- pagerank(edges,
      canonicals_df = can, out_of_scope_fold = "keep",
      drop_isolates_flag = FALSE
    )
    z_leak <- pr_leak[[2]][pr_leak[[1]] == "http://z/"]
    z_keep <- pr_keep[[2]][pr_keep[[1]] == "http://z/"]
    expect_lt(z_leak, z_keep)
  })

  it("leak: records the leaked mass and reconciles the mass accounting", {
    audit <- attr(
      pagerank(edges,
        canonicals_df = can, out_of_scope_fold = "leak",
        drop_isolates_flag = FALSE
      ),
      "transition_audit"
    )
    # The inbound equity left the measured graph as leaked mass.
    expect_gt(audit$mass$leaked, 0)
    # reported + sink + leaked + hidden == total, and total reconciles to 1.
    expect_equal(
      audit$mass$reported + audit$mass$sink +
        audit$mass$leaked + audit$mass$hidden,
      audit$mass$total,
      tolerance = 1e-8
    )
    expect_equal(audit$mass$total, 1, tolerance = 1e-8)
  })

  it("leak: leaked mass is zero when no fold is out of scope", {
    # Canonical y -> a is in scope (a crawled), so nothing leaks.
    can_in <- data.frame(from = "http://y/", to = "http://a/")
    audit <- attr(
      pagerank(edges,
        canonicals_df = can_in, out_of_scope_fold = "leak",
        drop_isolates_flag = FALSE
      ),
      "transition_audit"
    )
    expect_equal(audit$mass$leaked, 0)
    expect_equal(audit$mass$total, 1, tolerance = 1e-8)
  })
})

describe("transition_audit fold section", {
  edges <- data.frame(
    from = c("http://x/", "http://y/", "http://a/"),
    to   = c("http://a/", "http://a/", "http://z/")
  )
  can <- data.frame(from = "http://a/", to = "http://c/")

  it("reports the out-of-scope count and list under relabel", {
    audit <- attr(
      pagerank(edges,
        canonicals_df = can, out_of_scope_fold = "relabel",
        drop_isolates_flag = FALSE
      ),
      "transition_audit"
    )
    expect_equal(audit$fold$policy, "relabel")
    expect_equal(audit$fold$n_out_of_scope, 1L)
    expect_true(audit$fold$applied)
    expect_true(is.data.frame(audit$fold$out_of_scope))
    expect_equal(audit$fold$out_of_scope$source, "http://a/")
    expect_equal(audit$fold$out_of_scope$target, "http://c/")
    expect_equal(audit$fold$out_of_scope$signal, "canonical")
  })

  it("reports the same diagnostics but skipped=TRUE under keep", {
    audit <- attr(
      pagerank(edges,
        canonicals_df = can, out_of_scope_fold = "keep",
        drop_isolates_flag = FALSE
      ),
      "transition_audit"
    )
    expect_equal(audit$fold$policy, "keep")
    expect_equal(audit$fold$n_out_of_scope, 1L)
    expect_false(audit$fold$applied)
    expect_equal(audit$fold$out_of_scope$target, "http://c/")
  })

  it("reports policy == 'leak' and applied == TRUE under leak", {
    audit <- attr(
      pagerank(edges,
        canonicals_df = can, out_of_scope_fold = "leak",
        drop_isolates_flag = FALSE
      ),
      "transition_audit"
    )
    expect_equal(audit$fold$policy, "leak")
    expect_equal(audit$fold$n_out_of_scope, 1L)
    # Under leak the out-of-scope folds are acted upon (routed to the sink).
    expect_true(audit$fold$applied)
    expect_equal(audit$fold$out_of_scope$source, "http://a/")
    expect_equal(audit$fold$out_of_scope$target, "http://c/")
  })

  it("reports zero out-of-scope folds when all targets are crawled", {
    # Canonical y -> a: a is crawled, so the fold is in scope.
    can_in <- data.frame(from = "http://y/", to = "http://a/")
    audit <- attr(
      pagerank(edges, canonicals_df = can_in, drop_isolates_flag = FALSE),
      "transition_audit"
    )
    expect_equal(audit$fold$n_out_of_scope, 0L)
    expect_null(audit$fold$out_of_scope)
  })
})

describe("folded-away domain filter warning (PAGE-owdnylqo)", {
  # Repro: a crawl on tidioreviews.pages.dev whose canonicals fold every node
  # onto the never-crawled tidioreviews.com. Filtering on the crawled domain
  # (which runs AFTER folding) then matches nothing.
  crawled <- "tidioreviews.pages.dev"
  folded <- "tidioreviews.com"
  edges <- data.frame(
    from = c(
      "https://tidioreviews.pages.dev/",
      "https://tidioreviews.pages.dev/a",
      "https://tidioreviews.pages.dev/b"
    ),
    to = c(
      "https://tidioreviews.pages.dev/a",
      "https://tidioreviews.pages.dev/b",
      "https://tidioreviews.pages.dev/"
    )
  )
  # Every crawled node declares a canonical onto tidioreviews.com.
  can <- data.frame(
    from = c(
      "https://tidioreviews.pages.dev/",
      "https://tidioreviews.pages.dev/a",
      "https://tidioreviews.pages.dev/b"
    ),
    to = c(
      "https://tidioreviews.com/",
      "https://tidioreviews.com/a",
      "https://tidioreviews.com/b"
    )
  )

  it("warns, naming the crawled value and pointing at the fold", {
    expect_warning(
      pagerank(
        edges,
        canonicals_df = can,
        keep_domains = crawled,
        drop_isolates_flag = FALSE
      ),
      regexp = crawled
    )
    # The warning also blames the fold, not the filter.
    expect_warning(
      pagerank(
        edges,
        canonicals_df = can,
        keep_domains = crawled,
        drop_isolates_flag = FALSE
      ),
      regexp = "fold"
    )
  })

  it("does not warn for a normal same-domain crawl (post-fold match)", {
    # Canonicals fold within the crawled domain, so keep_domains still matches.
    can_in <- data.frame(
      from = "https://tidioreviews.pages.dev/a",
      to = "https://tidioreviews.pages.dev/"
    )
    expect_no_warning(
      pagerank(
        edges,
        canonicals_df = can_in,
        keep_domains = crawled,
        drop_isolates_flag = FALSE
      )
    )
  })

  it("does not warn for a genuinely absent domain (no pre-fold match)", {
    # example.org appears nowhere pre- or post-fold: this is an ordinary empty
    # filter result, not a folded-away crawled domain.
    expect_no_warning(
      pagerank(
        edges,
        canonicals_df = can,
        keep_domains = "example.org",
        drop_isolates_flag = FALSE
      )
    )
  })
})
