context("position")

# Crawler-neutral throughout: position is a plain numeric per-source index, so
# these tests prove the axis works on any edge list rather than asserting it.

apply_pos <- function(edges,
                      position_col = "pos",
                      transform = "zipf",
                      alpha = 1,
                      floor = 0.01,
                      weight_col = NULL) {
  pagerankr:::.pr_apply_position(
    edge_list_df = edges,
    position_col = position_col,
    from_col = "from",
    position_transform = transform,
    position_alpha = alpha,
    position_floor = floor,
    weight_col = weight_col
  )
}

# Two source pages, each carrying its own reading-order index.
two_page_edges <- function() {
  data.frame(
    from = c("A", "A", "A", "E", "E"),
    to = c("B", "C", "D", "F", "G"),
    pos = c(1, 2, 3, 1, 2)
  )
}

weight_of <- function(res) res$edge_list_df[[res$weight_col]]

describe("the reading-order decay", {
  it("applies a zipf decay within each source page", {
    res <- apply_pos(two_page_edges())
    # 1 / rank^1, ranked per source: A -> 1, 1/2, 1/3; E -> 1, 1/2.
    expect_equal(weight_of(res), c(1, 1 / 2, 1 / 3, 1, 1 / 2))
  })

  it("ranks per source, so every page's first link keeps full weight", {
    res <- apply_pos(two_page_edges())
    w <- weight_of(res)
    expect_equal(w[[1L]], 1) # A's first
    expect_equal(w[[4L]], 1) # E's first
  })

  it("honors the rank_linear shape", {
    res <- apply_pos(two_page_edges(), transform = "rank_linear")
    # (n - rank + 1) / n: A (n=3) -> 1, 2/3, 1/3; E (n=2) -> 1, 1/2.
    expect_equal(weight_of(res), c(1, 2 / 3, 1 / 3, 1, 1 / 2))
  })

  it("makes the drop-off steeper as alpha rises", {
    res <- apply_pos(two_page_edges(), alpha = 2)
    # 1 / rank^2: A -> 1, 1/4, 1/9.
    expect_equal(weight_of(res)[1:3], c(1, 1 / 4, 1 / 9))
  })

  it("clamps decayed weights up to the floor", {
    edges <- data.frame(
      from = "A",
      to = sprintf("t%03d", seq_len(100)),
      pos = seq_len(100)
    )
    res <- apply_pos(edges, floor = 0.05)
    w <- weight_of(res)
    # zipf reaches 1/100 by the last link; the floor holds it at 0.05.
    expect_equal(min(w), 0.05)
    expect_true(all(w >= 0.05))
  })

  it("leaves unindexed (NA) edges at weight 1", {
    edges <- data.frame(
      from = c("A", "A", "A"),
      to = c("B", "C", "D"),
      pos = c(1, NA, 2)
    )
    res <- apply_pos(edges)
    w <- weight_of(res)
    expect_equal(w[[2L]], 1) # the chrome / unindexed link is untouched
    expect_equal(w[[1L]], 1) # rank 1 among the two indexed links
    expect_equal(w[[3L]], 1 / 2) # rank 2 among the two indexed links
  })

  it("records the factors it used in provenance", {
    res <- apply_pos(two_page_edges(), alpha = 1.5, floor = 0.02)
    prov <- res$provenance
    expect_equal(prov$position_transform, "zipf")
    expect_equal(prov$position_alpha, 1.5)
    expect_equal(prov$position_floor, 0.02)
    expect_equal(prov$n_edges_scored, 5L)
    expect_equal(prov$n_sources_scored, 2L)
    expect_equal(prov$min_position_weight, 1 / 3^1.5)
  })

  it("is a no-op when no position column is supplied", {
    edges <- two_page_edges()
    res <- pagerankr:::.pr_apply_position(
      edge_list_df = edges,
      position_col = NULL,
      from_col = "from",
      position_transform = "zipf",
      position_alpha = 1,
      position_floor = 0.01,
      weight_col = NULL
    )
    expect_null(res$weight_col)
    expect_null(res$provenance)
    expect_identical(res$edge_list_df, edges)
  })
})

describe("composition with the graded boilerplate axis", {
  it("multiplies into an existing synthetic weight column", {
    edges <- two_page_edges()
    wc <- pagerankr:::.pr_edge_weight_col()
    # Pretend placement/boilerplate already graded these edges.
    edges[[wc]] <- c(0.1, 1, 1, 0.5, 1)
    res <- apply_pos(edges, weight_col = wc)
    # graded * zipf: 0.1*1, 1*1/2, 1*1/3, 0.5*1, 1*1/2.
    expect_equal(weight_of(res), c(0.1, 1 / 2, 1 / 3, 0.5, 1 / 2))
  })

  it("lets a top boilerplate CTA outrank a trailing organic link", {
    # notes/edge-weighting-model.md section 2: 0.5 * 1.0 beats 1.0 * 0.2, and it
    # falls out of the arithmetic rather than a special case.
    wc <- pagerankr:::.pr_edge_weight_col()
    edges <- data.frame(
      from = c("P", "P", "P", "P", "P", "Q"),
      to = c("t1", "t2", "t3", "t4", "organic", "cta"),
      pos = c(1, 2, 3, 4, 5, 1)
    )
    # P's links are organic (graded 1.0); Q's lone link is a boilerplate CTA
    # (graded 0.5) sitting at the top of its page.
    edges[[wc]] <- c(1, 1, 1, 1, 1, 0.5)
    res <- apply_pos(edges, weight_col = wc)
    w <- weight_of(res)
    trailing_organic <- w[[5L]] # 1.0 * (1/5)
    top_cta <- w[[6L]] # 0.5 * 1.0
    expect_equal(trailing_organic, 0.2)
    expect_equal(top_cta, 0.5)
    expect_gt(top_cta, trailing_organic)
  })
})

describe("validation", {
  it("errors when position_col is not in the edge list", {
    expect_error(
      apply_pos(two_page_edges(), position_col = "nope"),
      "position_col"
    )
  })

  it("rejects an unknown transform", {
    expect_error(
      apply_pos(two_page_edges(), transform = "exponential"),
      "position_transform"
    )
  })

  it("rejects a non-positive alpha", {
    expect_error(
      apply_pos(two_page_edges(), alpha = 0),
      "position_alpha"
    )
  })

  it("rejects a floor outside (0, 1]", {
    expect_error(apply_pos(two_page_edges(), floor = 0), "position_floor")
    expect_error(apply_pos(two_page_edges(), floor = 1.5), "position_floor")
  })

  it("rejects a non-numeric position column", {
    edges <- two_page_edges()
    edges$pos <- as.character(edges$pos)
    expect_error(apply_pos(edges), "must be a numeric column")
  })

  it("cannot be combined with a caller-supplied weight_col", {
    edges <- two_page_edges()
    edges$w <- 1
    expect_error(
      apply_pos(edges, weight_col = "w"),
      "cannot be combined with `position_col`"
    )
  })

  it("composes on the synthetic weight column without error", {
    edges <- two_page_edges()
    wc <- pagerankr:::.pr_edge_weight_col()
    edges[[wc]] <- 1
    expect_silent(apply_pos(edges, weight_col = wc))
  })
})

describe("pagerank() end to end", {
  it("switches the axis on and records it in the audit", {
    edges <- data.frame(
      from = c("A", "A", "B", "C"),
      to = c("B", "C", "A", "A"),
      pos = c(1, 2, NA, NA)
    )
    pr <- pagerank(edges, position_col = "pos", clean_edge_urls = FALSE)
    audit <- attr(pr, "transition_audit")
    expect_false(is.null(audit$config$position))
    expect_equal(audit$config$position$position_transform, "zipf")
    expect_equal(audit$config$position$n_edges_scored, 2L)
    # A splits its vote 1 : 0.5 toward B, so B outscores C.
    score <- stats::setNames(pr[[2L]], pr[[1L]])
    expect_gt(score[["B"]], score[["C"]])
  })

  it("is off by default and leaves the audit position slot NULL", {
    edges <- data.frame(from = c("A", "A"), to = c("B", "C"))
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    audit <- attr(pr, "transition_audit")
    expect_null(audit$config$position)
  })

  it("rejects position_col combined with a caller weight_col up front", {
    edges <- data.frame(
      from = c("A", "A"), to = c("B", "C"),
      pos = c(1, 2), w = c(1, 1)
    )
    expect_error(
      pagerank(edges, position_col = "pos", weight_col = "w",
        clean_edge_urls = FALSE),
      "cannot be combined with `position_col`"
    )
  })
})
