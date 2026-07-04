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
