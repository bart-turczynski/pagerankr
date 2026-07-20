context("placement")

# A deliberately crawler-agnostic edge list: no Screaming Frog anywhere, just a
# categorical column in the shared vocabulary. This is the whole point of
# promoting placement out of the SF wrapper.
placement_edges <- function() {
  data.frame(
    from = c("A", "A", "A", "B", "B", "C"),
    to = c("B", "C", "D", "C", "D", "A"),
    region = c("content", "nav", "footer", "content", "aside", "header"),
    stringsAsFactors = FALSE
  )
}

describe("pagerank() placement arguments", {
  it("is a no-op when placement_col is not supplied", {
    edges <- placement_edges()
    expect_equal(
      pagerank(edges, prior_verbose = FALSE),
      pagerank(edges, placement_col = NULL, prior_verbose = FALSE)
    )
  })

  it("filters to accepted_placements", {
    result <- pagerank(
      placement_edges(),
      placement_col = "region",
      accepted_placements = c("content", "aside"),
      prior_verbose = FALSE
    )

    audit <- attr(result, "transition_audit")
    # 3 of the 6 rows survive: two content, one aside.
    expect_identical(audit$counts$n_input_rows, 3L)
    expect_identical(audit$config$placement$n_rows_dropped, 3L)
    expect_identical(
      audit$config$placement$accepted_placements,
      c("content", "aside")
    )
  })

  it("weights edges by region and records it as the weight column", {
    result <- pagerank(
      placement_edges(),
      placement_col = "region",
      placement_weights = c(
        content = 1, nav = 0.1, header = 0.1, footer = 0.1, aside = 0.1
      ),
      prior_verbose = FALSE
    )

    audit <- attr(result, "transition_audit")
    expect_true(audit$coverage$weighted)
    expect_identical(audit$coverage$weight_col, ".__pr_edge_weight__")
    expect_identical(audit$config$placement$placement_weights[["nav"]], 0.1)
  })

  it("matches a hand-built weight column", {
    edges <- placement_edges()
    weights <- c(content = 1, nav = 0.1, header = 0.1, footer = 0.1)
    by_placement <- pagerank(
      edges,
      placement_col = "region",
      placement_weights = weights,
      prior_verbose = FALSE
    )
    edges$w <- unname(ifelse(
      edges$region %in% names(weights), weights[edges$region], 1
    ))
    by_hand <- pagerank(edges, weight_col = "w", prior_verbose = FALSE)

    attr(by_placement, "transition_audit") <- NULL
    attr(by_hand, "transition_audit") <- NULL
    expect_equal(by_placement, by_hand)
  })

  it("leaves unnamed placements at weight 1", {
    # `aside` is deliberately absent from the recipe below. It keeps weight 1,
    # which is why a complete recipe has to name all five terms.
    edges <- placement_edges()
    result <- pagerank(
      edges,
      placement_col = "region",
      placement_weights = c(nav = 0.1),
      prior_verbose = FALSE
    )
    expect_s3_class(result, "data.frame")

    applied <- .pr_apply_placement(
      edge_list_df = edges,
      placement_col = "region",
      accepted_placements = NULL,
      placement_weights = c(nav = 0.1),
      weight_col = NULL
    )
    expect_identical(
      applied$edge_list_df[[".__pr_edge_weight__"]],
      c(1, 0.1, 1, 1, 1, 1)
    )
  })

  it("normalizes case and whitespace on both sides of the match", {
    edges <- placement_edges()
    edges$region <- toupper(paste0(" ", edges$region, " "))
    applied <- .pr_apply_placement(
      edge_list_df = edges,
      placement_col = "region",
      accepted_placements = c("Content", " NAV "),
      placement_weights = NULL,
      weight_col = NULL
    )
    expect_identical(nrow(applied$edge_list_df), 3L)
  })

  it("rejects the vocabulary being stretched", {
    edges <- placement_edges()
    expect_error(
      pagerank(edges, placement_col = "region", accepted_placements = "main"),
      "`accepted_placements` must contain only"
    )
    expect_error(
      pagerank(edges, placement_col = "region", accepted_placements = 123),
      "`accepted_placements` must be a character vector"
    )
    expect_error(
      pagerank(edges, placement_col = "region", placement_weights = c(1, 2)),
      "`placement_weights` must be a named positive numeric vector"
    )
    expect_error(
      pagerank(
        edges,
        placement_col = "region", placement_weights = c(sidebar = 0.1)
      ),
      "`placement_weights` names must contain only"
    )
    expect_error(
      pagerank(
        edges,
        placement_col = "region", placement_weights = c(nav = -1)
      ),
      "`placement_weights` must contain finite positive values"
    )
    expect_error(
      pagerank(
        edges,
        placement_col = "region", placement_weights = c(nav = 1, NAV = 2)
      ),
      "`placement_weights` names must be unique"
    )
  })

  it("requires placement_col before the placement dials mean anything", {
    edges <- placement_edges()
    expect_error(
      pagerank(edges, accepted_placements = "content"),
      "`accepted_placements` requires `placement_col`"
    )
    expect_error(
      pagerank(edges, placement_weights = c(nav = 0.1)),
      "`placement_weights` requires `placement_col`"
    )
    expect_error(
      pagerank(edges, placement_col = "not_a_column"),
      "`placement_col` 'not_a_column' not found"
    )
  })

  it("refuses placement_weights alongside a caller-supplied weight_col", {
    edges <- placement_edges()
    edges$w <- 1
    expect_error(
      pagerank(
        edges,
        placement_col = "region",
        placement_weights = c(nav = 0.1),
        weight_col = "w"
      ),
      "`weight_col` cannot be combined with `placement_weights`"
    )
  })

  it("errors rather than scoring an empty graph", {
    edges <- placement_edges()
    edges$region <- "nav"
    expect_error(
      pagerank(edges, placement_col = "region", accepted_placements = "footer"),
      "No edges remain after filtering to `accepted_placements`"
    )
  })

  it("threads through a preset, since it is a pagerank() formal", {
    result <- pagerank(
      placement_edges(),
      preset = list(
        placement_col = "region",
        placement_weights = c(content = 1, nav = 0.1)
      ),
      prior_verbose = FALSE
    )
    audit <- attr(result, "transition_audit")
    expect_identical(audit$config$placement$placement_col, "region")
    expect_identical(audit$config$preset, "custom")
  })
})
