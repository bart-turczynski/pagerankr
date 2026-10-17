# The command-line entry point scores an edge list with pagerank() and
# compares two score files with compare_pagerank(). These tests first pin what
# those two functions return on the shipped case-study crawls, then require the
# CLI to reproduce it.

# The graph-eligible edges of one shipped Screaming Frog crawl, reduced to the
# columns the CLI reads: from, to and placement. The fixture is a crawl export,
# not an edge list, so the edge list is derived through the package's own
# Screaming Frog importer.
cli_fixture_edges <- function(phase) {
  dir <- system.file(
    "extdata", paste0("reviews-microsite-", phase),
    package = "pagerankr"
  )
  bundle <- screaming_frog_bundle(
    internal = file.path(dir, "internal_all.csv"),
    links = file.path(dir, "all_inlinks.csv")
  )
  bundle$edges[, c("from", "to", "placement")]
}

cli_top_node <- function(scores) {
  scores[which.max(scores$pagerank), , drop = FALSE]
}

describe("pagerank() on the case-study fixture (pinned for the CLI)", {
  edges <- cli_fixture_edges("before")

  it("scores the before crawl with the documented defaults", {
    pr <- pagerank(edges, placement_col = "placement")

    expect_named(pr, c("node_name", "pagerank"))
    expect_identical(nrow(pr), 78L)
    expect_equal(sum(pr$pagerank), 1)
    top <- cli_top_node(pr)
    expect_identical(
      top$node_name, "https://external-12.example.org/s8/p02?q=1"
    )
    expect_equal(top$pagerank, 0.0213480653, tolerance = 1e-8)
    # The CLI forwards --damping; the default it inherits is 0.85.
    expect_identical(
      pagerank(edges, placement_col = "placement", damping = 0.85)$pagerank,
      pr$pagerank
    )
  })

  it("computes CheiRank as PageRank on the transposed graph", {
    fwd <- pagerank(edges, placement_col = "placement")
    chei <- pagerank(edges, placement_col = "placement", reverse = TRUE)
    swapped <- edges
    swapped$from <- edges$to
    swapped$to <- edges$from
    expected <- pagerank(swapped, placement_col = "placement")

    expect_setequal(chei$node_name, fwd$node_name)
    expect_equal(
      chei$pagerank,
      expected$pagerank[match(chei$node_name, expected$node_name)]
    )
    expect_false(isTRUE(all.equal(
      chei$pagerank, fwd$pagerank[match(chei$node_name, fwd$node_name)]
    )))
    top <- cli_top_node(chei)
    expect_identical(top$node_name, "https://reviews-microsite.example.net/s2/")
    expect_equal(top$pagerank, 0.1352078442, tolerance = 1e-8)
  })

  it("moves the scores with damping and placement weights", {
    weights <- c(
      content = 1, nav = 0.1, header = 0.1, footer = 0.1, aside = 0.1
    )
    pr <- pagerank(
      edges,
      placement_col = "placement", placement_weights = weights, damping = 0.7
    )
    top <- cli_top_node(pr)
    expect_equal(top$pagerank, 0.01823862222, tolerance = 1e-8)

    content_only <- pagerank(
      edges,
      placement_col = "placement", accepted_placements = "content"
    )
    expect_identical(nrow(content_only), 77L)
  })
})

describe("compare_pagerank() on the case-study fixture (pinned for the CLI)", {
  it("compares the before and after crawls", {
    before <- pagerank(cli_fixture_edges("before"), placement_col = "placement")
    after <- pagerank(cli_fixture_edges("after"), placement_col = "placement")
    cmp <- compare_pagerank(before, after)

    expect_identical(nrow(cmp), 96L)
    common <- cmp[!is.na(cmp$pagerank_a) & !is.na(cmp$pagerank_b), ]
    expect_equal(
      common$pagerank_a,
      before$pagerank[match(common$node_name, before$node_name)]
    )
    expect_equal(
      common$delta,
      after$pagerank[match(common$node_name, after$node_name)] -
        before$pagerank[match(common$node_name, before$node_name)]
    )
    summary <- attr(cmp, "summary")
    expect_identical(summary$nodes_gained, 18L)
    expect_identical(summary$nodes_lost, 6L)
    expect_equal(summary$spearman_rho, 0.8322140496, tolerance = 1e-8)
    expect_equal(summary$mean_abs_delta, 0.0006117597391, tolerance = 1e-8)
  })
})
