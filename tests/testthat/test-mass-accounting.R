# Mass accounting (B2 / PAGE-mqsxrcdz)
#
# The internal stationary vector always sums to 1. The returned visible scores
# can sum to < 1 because synthetic (nofollow sink) and hidden (robots-blocked
# vanish) nodes are removed. The transition audit's `mass` field decomposes
# that total into reported / evaporated (sink) / hidden / total (= 1).

describe("mass accounting", {
  it("decomposes total mass into reported + evaporated + hidden = 1", {
    # A -> B (follow), A -> C (nofollow => evaporates to the sink),
    # B -> Blocked (Blocked is robots-blocked and vanishes).
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "Blocked"),
      nofollow = c(FALSE, TRUE, FALSE)
    )
    idx_df <- data.frame(
      url = "Blocked",
      indexability_status = "Blocked by robots.txt"
    )

    pr <- pagerank(edges,
      nofollow_col = "nofollow",
      nofollow_action = "evaporate",
      indexability_df = idx_df,
      robots_blocked_action = "vanish",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )

    audit <- attr(pr, "transition_audit")
    expect_s3_class(audit, "transition_audit")

    mass <- audit$mass
    # All four components present and numeric.
    expect_true(is.numeric(mass$reported))
    expect_true(is.numeric(mass$sink))
    expect_true(is.numeric(mass$hidden))
    expect_true(is.numeric(mass$total))

    # Both deficit channels are exercised: real (positive) evaporated and
    # hidden mass, not just zeros.
    expect_gt(mass$sink, 0)
    expect_gt(mass$hidden, 0)

    # Reported equals the summed visible scores.
    expect_equal(mass$reported, sum(pr$pagerank), tolerance = 1e-9)

    # The components reconcile to 1 (the internal stationary vector).
    expect_equal(mass$total, 1, tolerance = 1e-8)
    expect_equal(
      mass$reported + mass$sink + mass$hidden, 1,
      tolerance = 1e-8
    )
  })

  it("reports zero evaporated/hidden mass when neither channel is active", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    mass <- attr(pr, "transition_audit")$mass

    expect_equal(mass$sink, 0)
    expect_equal(mass$hidden, 0)
    expect_equal(mass$reported, 1, tolerance = 1e-8)
    expect_equal(mass$total, 1, tolerance = 1e-8)
  })

  it("attributes evaporated mass only to the nofollow sink", {
    edges <- data.frame(
      from = c("A", "A"),
      to = c("B", "C"),
      nofollow = c(FALSE, TRUE)
    )
    pr <- pagerank(edges,
      nofollow_col = "nofollow",
      nofollow_action = "evaporate",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    mass <- attr(pr, "transition_audit")$mass

    expect_gt(mass$sink, 0)
    expect_equal(mass$hidden, 0)
    expect_equal(mass$total, 1, tolerance = 1e-8)
  })

  it("attributes hidden mass only to vanished robots-blocked nodes", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "Blocked")
    )
    idx_df <- data.frame(
      url = "Blocked",
      indexability_status = "Blocked by robots.txt"
    )
    pr <- pagerank(edges,
      indexability_df = idx_df,
      robots_blocked_action = "vanish",
      clean_edge_urls = FALSE, drop_isolates_flag = FALSE
    )
    mass <- attr(pr, "transition_audit")$mass

    expect_equal(mass$sink, 0)
    expect_gt(mass$hidden, 0)
    expect_equal(mass$total, 1, tolerance = 1e-8)
  })

  it("leaves mass fields NULL on an empty graph", {
    edges <- data.frame(
      from = character(0), to = character(0)
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    mass <- attr(pr, "transition_audit")$mass

    expect_null(mass$reported)
    expect_null(mass$sink)
    expect_null(mass$hidden)
    expect_null(mass$total)
  })
})
