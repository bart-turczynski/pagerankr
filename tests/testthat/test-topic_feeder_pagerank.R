context("topic_feeder_pagerank (seeded reverse-graph PageRank)")

describe("feeder_seed_prior", {
  it("builds an equal-weight prior from a character vector", {
    p <- feeder_seed_prior(c("/ai", "/ai-demo"))
    expect_equal(p$url, c("/ai", "/ai-demo"))
    expect_equal(p$weight, c(1, 1))
  })

  it("recycles a scalar seed_weight and accepts per-seed weights", {
    expect_equal(
      feeder_seed_prior(c("a", "b"), seed_weight = 3)$weight, c(3, 3)
    )
    expect_equal(
      feeder_seed_prior(c("a", "b"), seed_weight = c(2, 5))$weight, c(2, 5)
    )
  })

  it("reads URL/weight columns from a data frame", {
    df <- data.frame(
      url = c("a", "b"), weight = c(4, 1)
    )
    expect_equal(feeder_seed_prior(df)$weight, c(4, 1))
  })

  it("rejects seed_weight alongside a data frame", {
    df <- data.frame(url = "a", weight = 1)
    expect_error(
      feeder_seed_prior(df, seed_weight = 2),
      "applies only when `seeds` is a character vector"
    )
  })

  it("errors on negative weights and on empty seed sets", {
    expect_error(
      feeder_seed_prior(c("a"), seed_weight = -1),
      "must be non-negative"
    )
    expect_error(feeder_seed_prior(c(NA, "")), "no usable cluster URLs")
  })

  it("errors on a mismatched seed_weight length", {
    expect_error(
      feeder_seed_prior(c("a", "b", "c"), seed_weight = c(1, 2)),
      "length 1 or match the number of seeds"
    )
  })

  it("errors when seeds data frame lacks required columns", {
    expect_error(
      feeder_seed_prior(data.frame(x = 1)),
      "must have"
    )
  })

  it("errors when seed_weight is non-numeric", {
    expect_error(
      feeder_seed_prior(c("a", "b"), seed_weight = "bad"),
      "must be numeric or NULL"
    )
  })

  it("errors when seeds is neither a character vector nor a data frame", {
    expect_error(
      feeder_seed_prior(123),
      "must be a character vector of URLs or a data frame"
    )
  })
})

describe("topic_feeder_pagerank", {
  # F1 feeds the cluster {X1, X2} via two links; F2 feeds X1 only;
  # N1 -> N2 is noise disconnected from the cluster.
  edges <- data.frame(
    from = c("F1", "F1", "F2", "X1", "N1"),
    to = c("X1", "X2", "X1", "X2", "N2")
  )

  it("surfaces feeders: F1 (feeds both) outranks F2 (feeds one)", {
    fr <- topic_feeder_pagerank(
      edges, seeds = c("X1", "X2"),
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    feeders <- fr[fr$prior_weight == 0, ]
    # Top feeder by construction is F1; F2 is the weaker feeder.
    expect_equal(feeders$node_name[1], "F1")
    expect_gt(
      feeders$pagerank[feeders$node_name == "F1"],
      feeders$pagerank[feeders$node_name == "F2"]
    )
    # Noise disconnected from the cluster earns no feeder credit.
    noise <- feeders$pagerank[feeders$node_name %in% c("N1", "N2")]
    expect_equal(noise, c(0, 0))
  })

  it("places the cluster's teleport mass on the seeds only", {
    fr <- topic_feeder_pagerank(
      edges, seeds = c("X1", "X2"),
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    seeds <- fr$node_name[fr$prior_weight > 0]
    expect_setequal(seeds, c("X1", "X2"))
  })

  it("equals pagerank() with the same prior under reverse = TRUE", {
    fr <- topic_feeder_pagerank(
      edges, seeds = c("X1", "X2"),
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    prior <- feeder_seed_prior(c("X1", "X2"))
    manual <- pagerank(
      edges, prior_df = prior, reverse = TRUE,
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    a <- fr[order(fr$node_name), ]
    b <- manual[order(manual$node_name), ]
    expect_equal(a$node_name, b$node_name)
    expect_equal(a$pagerank, b$pagerank, tolerance = 1e-12)
  })

  it("differs from forward topic_sensitive_pagerank (flow direction matters)", {
    feeders <- topic_feeder_pagerank(
      edges, seeds = c("X1", "X2"),
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    auth <- topic_sensitive_pagerank(
      edges, topics = list(cluster = c("X1", "X2")),
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    # Forward (authority) credits the cluster itself; reverse credits feeders.
    top_feeder <- feeders$node_name[feeders$prior_weight == 0][1]
    top_auth <- auth$node_name[which.max(auth$cluster)]
    expect_equal(top_feeder, "F1")
    expect_true(top_auth %in% c("X1", "X2"))
    expect_false(top_feeder == top_auth)
  })

  it("records reverse = TRUE in the transition audit", {
    fr <- topic_feeder_pagerank(
      edges, seeds = c("X1", "X2"),
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    expect_true(attr(fr, "transition_audit")$config$reverse)
  })

  it("is sorted by pagerank descending", {
    fr <- topic_feeder_pagerank(
      edges, seeds = c("X1", "X2"),
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    expect_false(is.unsorted(rev(fr$pagerank)))
  })

  it("honors graded cluster weights", {
    expect_silent(
      fr <- topic_feeder_pagerank(
        edges,
        seeds = data.frame(
          url = c("X1", "X2"), weight = c(3, 1)
        ),
        clean_edge_urls = FALSE, prior_verbose = FALSE
      )
    )
    pw <- stats::setNames(fr$prior_weight, fr$node_name)
    expect_gt(pw[["X1"]], pw[["X2"]])
  })

  it("rejects caller-owned prior and reverse arguments", {
    for (arg in list(
      list(prior_df = data.frame(url = "X1", weight = 1)),
      list(prior_url_col = "u"),
      list(prior_weight_col = "w"),
      list(reverse = FALSE)
    )) {
      expect_error(
        do.call(
          topic_feeder_pagerank,
          c(list(edges, seeds = "X1", clean_edge_urls = FALSE), arg)
        ),
        "Do not pass"
      )
    }
  })

  it("errors on a non-data-frame edge list", {
    expect_error(
      topic_feeder_pagerank(list(), seeds = "X1"),
      "must be a data frame"
    )
  })

  it("inherits pagerank()'s reverse-mode guards (evaporate is rejected)", {
    nf <- data.frame(
      from = c("F1", "F1"), to = c("X1", "X2"),
      nofollow = c(FALSE, TRUE)
    )
    expect_error(
      topic_feeder_pagerank(
        nf, seeds = c("X1", "X2"),
        nofollow_col = "nofollow", nofollow_action = "evaporate",
        clean_edge_urls = FALSE, prior_verbose = FALSE
      ),
      "evaporate.*reverse|reverse.*evaporate"
    )
  })
})
