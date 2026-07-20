context("boilerplate")

# A plain, non-Screaming-Frog edge list throughout: the detector is
# crawler-neutral, so the tests prove that rather than asserting it.

pages <- function(n = 20) sprintf("https://x.test/p%02d", seq_len(n))

# `cta` recurs on every page and always points at one target -> boilerplate.
# `related` recurs on every page but points somewhere different each time ->
# not boilerplate, despite identical component recurrence.
template_edges <- function(n = 20) {
  p <- pages(n)
  rbind(
    data.frame(from = p, to = "https://x.test/demo", container = "cta"),
    data.frame(from = p, to = p[c(n, seq_len(n - 1))], container = "related")
  )
}

apply_bp <- function(edges,
                     container_col = "container",
                     threshold = 0.5,
                     min_pages = 10,
                     weight = 0.5,
                     weight_col = NULL) {
  pagerankr:::.pr_apply_boilerplate(
    edge_list_df = edges,
    container_col = container_col,
    from_col = "from",
    to_col = "to",
    boilerplate_threshold = threshold,
    min_container_pages = min_pages,
    boilerplate_weight = weight,
    weight_col = weight_col
  )
}

weights_by <- function(res, edges, col = "container") {
  tapply(res$edge_list_df[[res$weight_col]], edges[[col]], unique)
}

describe("the container-conditioned metric", {
  it("discounts a recycled CTA but not a varying module", {
    edges <- template_edges()
    w <- weights_by(apply_bp(edges), edges)
    expect_equal(unname(w[["cta"]]), 0.5)
    expect_equal(unname(w[["related"]]), 1)
  })

  it("conditions on the container, not the site", {
    # The consent-banner case from notes section 5: a component that appears on
    # only a fraction of pages but always links the same place. A sitewide
    # denominator scores it 10/50 = 0.2 and misses it; the container-conditioned
    # denominator scores it 10/10 = 1.0 and catches it.
    p <- pages(50)
    edges <- rbind(
      data.frame(from = p, to = "https://x.test/home", container = "body"),
      data.frame(
        from = p[1:10],
        to = "https://x.test/privacy",
        container = "consent"
      )
    )
    w <- weights_by(apply_bp(edges), edges)
    expect_equal(unname(w[["consent"]]), 0.5)
  })

  it("counts distinct pages, not edge rows", {
    # The same container linking the same target twice on one page is one
    # page's worth of evidence. Here every page links the target twice from
    # `cta`, and once from `body` to somewhere else.
    p <- pages(12)
    edges <- rbind(
      data.frame(from = p, to = "https://x.test/demo", container = "cta"),
      data.frame(from = p, to = "https://x.test/demo", container = "cta"),
      data.frame(from = p, to = p, container = "body")
    )
    res <- apply_bp(edges)
    expect_equal(res$provenance$n_edges_scored, nrow(edges))
    # 24 cta rows over 12 pages -> ratio is still 1, not 2.
    expect_equal(res$provenance$n_edges_discounted, 24L)
  })

  it("treats the ratio as a boilerplate score, higher meaning more", {
    # `half` points at the target on exactly half its pages -> ratio 0.5.
    p <- pages(20)
    edges <- rbind(
      data.frame(from = p[1:10], to = "https://x.test/t", container = "half"),
      data.frame(from = p[11:20], to = p[11:20], container = "half")
    )
    # At the threshold the comparison is >=, so ratio 0.5 is classified.
    at_threshold <- apply_bp(edges, threshold = 0.5)
    expect_equal(at_threshold$provenance$n_edges_discounted, 10L)

    # The same edges keep full weight under a stricter threshold.
    stricter <- apply_bp(edges, threshold = 0.9)
    expect_equal(stricter$provenance$n_edges_discounted, 0L)
    expect_equal(unique(stricter$edge_list_df[[stricter$weight_col]]), 1)
  })
})

describe("the evidence floor", {
  it("never classifies a container seen on too few pages", {
    p <- pages(5)
    edges <- data.frame(
      from = p, to = "https://x.test/demo", container = "cta"
    )
    # Ratio is 1.0, but five pages is below the default floor of ten.
    res <- apply_bp(edges)
    expect_equal(res$provenance$n_edges_judged, 0L)
    expect_equal(res$provenance$n_edges_discounted, 0L)
    expect_equal(unique(res$edge_list_df[[res$weight_col]]), 1)
  })

  it("classifies the same container once the floor is lowered", {
    p <- pages(5)
    edges <- data.frame(
      from = p, to = "https://x.test/demo", container = "cta"
    )
    res <- apply_bp(edges, min_pages = 5)
    expect_equal(res$provenance$n_edges_discounted, 5L)
  })
})

describe("composition with placement", {
  it("multiplies into the placement factor rather than replacing it", {
    p <- pages(20)
    edges <- rbind(
      data.frame(
        from = p, to = "https://x.test/demo",
        container = "cta", region = "content"
      ),
      data.frame(
        from = p, to = "https://x.test/menu",
        container = "menu", region = "nav"
      )
    )
    placed <- pagerankr:::.pr_apply_placement(
      edge_list_df = edges,
      placement_col = "region",
      accepted_placements = NULL,
      placement_weights = c(
        content = 1, nav = 0.1, header = 0.1, footer = 0.1, aside = 0.1
      ),
      weight_col = NULL
    )
    res <- apply_bp(placed$edge_list_df, weight_col = placed$weight_col)
    w <- weights_by(res, edges)
    # nav 0.1 x boilerplate 0.5; content 1 x boilerplate 0.5.
    expect_equal(unname(w[["menu"]]), 0.05)
    expect_equal(unname(w[["cta"]]), 0.5)
  })

  it("records both factor sets separately in the transition audit", {
    # notes section 2: storing only the product makes the output unauditable.
    p <- pages(20)
    edges <- rbind(
      data.frame(
        from = p, to = "https://x.test/demo",
        container = "cta", region = "content"
      ),
      data.frame(
        from = p, to = "https://x.test/menu",
        container = "menu", region = "nav"
      )
    )
    config <- attr(
      pagerank(
        edges,
        placement_col = "region",
        placement_weights = c(
          content = 1, nav = 0.1, header = 0.1, footer = 0.1, aside = 0.1
        ),
        container_col = "container"
      ),
      "transition_audit"
    )$config
    expect_equal(config$placement$placement_col, "region")
    expect_equal(config$boilerplate$container_col, "container")
    expect_equal(config$boilerplate$boilerplate_weight, 0.5)
    expect_equal(config$boilerplate$boilerplate_threshold, 0.5)
    expect_equal(config$boilerplate$min_container_pages, 10)
  })
})

describe("pagerank() integration", {
  it("is off by default and changes nothing", {
    edges <- template_edges()
    expect_equal(
      pagerank(edges)$pagerank,
      pagerank(edges, boilerplate_weight = 0.2)$pagerank
    )
    expect_null(
      attr(pagerank(edges), "transition_audit")$config$boilerplate
    )
  })

  it("lowers the rank of a template target", {
    edges <- template_edges()
    flat <- pagerank(edges)
    detected <- pagerank(edges, container_col = "container")
    target <- "https://x.test/demo"
    expect_lt(
      detected$pagerank[detected$node_name == target],
      flat$pagerank[flat$node_name == target]
    )
  })

  it("downweights rather than drops: no node disappears", {
    edges <- template_edges()
    expect_setequal(
      pagerank(edges, container_col = "container")$node_name,
      pagerank(edges)$node_name
    )
  })
})

describe("unscorable rows", {
  it("leaves rows with a missing or empty container at full weight", {
    p <- pages(12)
    edges <- rbind(
      data.frame(from = p, to = "https://x.test/demo", container = "cta"),
      data.frame(from = p, to = "https://x.test/x", container = NA_character_),
      data.frame(from = p, to = "https://x.test/y", container = "")
    )
    res <- apply_bp(edges)
    w <- res$edge_list_df[[res$weight_col]]
    expect_equal(res$provenance$n_edges_scored, 12L)
    unscorable <- is.na(edges$container) | !nzchar(edges$container)
    expect_equal(unique(w[unscorable]), 1)
  })
})

describe("validation", {
  it("rejects a caller-supplied weight_col alongside container_col", {
    edges <- template_edges()
    edges$w <- 1
    expect_error(
      pagerank(edges, weight_col = "w", container_col = "container"),
      "cannot be combined with `container_col`"
    )
  })

  it("rejects a container column that is not in the edge list", {
    expect_error(
      pagerank(template_edges(), container_col = "nope"),
      "nope"
    )
  })

  it("rejects out-of-range constants", {
    edges <- template_edges()
    expect_error(
      pagerank(edges, container_col = "container", boilerplate_threshold = 0),
      "`boilerplate_threshold` must be a single number in \\(0, 1\\]"
    )
    expect_error(
      pagerank(edges, container_col = "container", boilerplate_threshold = 1.5),
      "`boilerplate_threshold` must be a single number in \\(0, 1\\]"
    )
    # A weight of 0 is a silent deletion, which the model rejects outright.
    expect_error(
      pagerank(edges, container_col = "container", boilerplate_weight = 0),
      "`boilerplate_weight` must be a single number in \\(0, 1\\]"
    )
    expect_error(
      pagerank(edges, container_col = "container", min_container_pages = 0),
      "`min_container_pages` must be a single finite number >= 1"
    )
  })
})
