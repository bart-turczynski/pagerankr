context("prior_exclude_waste: excluding the waste class from teleport")

# A hub linking to a ring of real pages, plus K "dead" pages (HTTP 404) each
# discovered by exactly one hub link and carrying no outlinks. This is the
# field-notes section 11 experiment (notes/experiments/teleport-dead-pages.R)
# scaled down: uniform teleport pays every dead page for existing, so real
# pages collapse as K grows; excluding the class from teleport removes that
# manufactured collapse while preserving the genuine decline (the hub really
# does spend its outgoing link budget on broken links).
.hub <- "https://ex.com/"
.real <- sprintf("https://ex.com/p%02d", 1:10)
.ring_to <- c(.real[-1], .real[1])
.base_edges <- rbind(
  data.frame(from = .hub, to = .real, stringsAsFactors = FALSE),
  data.frame(from = .real, to = .ring_to, stringsAsFactors = FALSE),
  data.frame(from = .real, to = .hub, stringsAsFactors = FALSE)
)
.dead_urls <- function(k) {
  if (k == 0) character(0) else sprintf("https://ex.com/dead%04d", seq_len(k))
}
.make_edges <- function(k) {
  if (k == 0) {
    return(.base_edges)
  }
  rbind(
    .base_edges,
    data.frame(from = .hub, to = .dead_urls(k), stringsAsFactors = FALSE)
  )
}
.run <- function(k, prior_exclude_waste) {
  args <- list(
    edge_list_df = .make_edges(k),
    clean_edge_urls = FALSE,
    prior_verbose = FALSE,
    prior_exclude_waste = prior_exclude_waste
  )
  d <- .dead_urls(k)
  if (length(d) > 0) {
    args$status_df <- data.frame(url = d, status_code = 404L)
  }
  do.call(pagerank, args)
}
.share <- function(pr, urls) sum(pr$pagerank[pr$node_name %in% urls])

describe("the manufactured collapse is removed but the real decline survives", {
  it("keeps far more mass on real pages than uniform teleport does", {
    real_nodes <- c(.hub, .real)
    k0 <- .run(0, TRUE)
    excl <- .run(60, TRUE)
    unif <- .run(60, FALSE)

    real0 <- .share(k0, real_nodes)
    real_excl <- .share(excl, real_nodes)
    real_unif <- .share(unif, real_nodes)

    # (1) The decline SURVIVES: with 60 broken links draining the hub's budget,
    # real pages hold strictly less mass than with none. This is correct and
    # must not be papered over.
    expect_lt(real_excl, real0)
    # (2) But it is NOT the uniform-teleport collapse: excluding the class from
    # teleport leaves the real pages with far more mass than uniform does.
    expect_gt(real_excl, real_unif)
    expect_gt(real_excl, 2 * real_unif)
  })

  it("stops dead pages from inflating as their teleport share is zeroed", {
    dead <- .dead_urls(60)
    excl <- .run(60, TRUE)
    unif <- .run(60, FALSE)
    # The whole dead cohort collects less under exclusion than under uniform...
    expect_lt(.share(excl, dead), .share(unif, dead))
    # ...and every single dead page scores strictly lower once its teleport is
    # removed (it keeps only the trickle it collects through its one inlink).
    per_excl <- excl$pagerank[excl$node_name %in% dead]
    per_unif <- unif$pagerank[unif$node_name %in% dead]
    expect_true(all(per_excl < per_unif))
  })

  it("declines monotonically in K under exclusion (budget really is spent)", {
    real_nodes <- c(.hub, .real)
    real_k20 <- .share(.run(20, TRUE), real_nodes)
    real_k60 <- .share(.run(60, TRUE), real_nodes)
    expect_gt(real_k20, real_k60)
  })
})

describe("zeroed teleport entries still converge", {
  it("returns a proper stationary vector with no NA, negative, or zero score", {
    for (k in c(10, 40, 80)) {
      pr <- .run(k, TRUE)
      expect_false(anyNA(pr$pagerank))
      expect_true(all(pr$pagerank > 0))
      expect_true(is.finite(sum(pr$pagerank)))
      # Reported mass is < 1 (the rest evaporates), but strictly positive.
      expect_gt(sum(pr$pagerank), 0)
      expect_lte(sum(pr$pagerank), 1 + 1e-9)
    }
  })
})

describe("mass still reconciles to 1 with teleport exclusion in play", {
  it("holds reported + sink + leaked + hidden = 1 with the class excluded", {
    pr <- .run(40, TRUE)
    mass <- attr(pr, "transition_audit")$mass
    expect_equal(mass$total, 1)
    expect_equal(
      mass$reported + mass$sink + mass$leaked + mass$hidden, 1
    )
  })

  it("does not double-count when exclusion and a leak sink both apply", {
    # An out-of-scope leak sink AND an excluded 404 class, at once: teleport
    # exclusion and sink routing must partition the same stationary vector, not
    # overlap. All four buckets are exercised here.
    edges <- data.frame(
      from = c("https://in.test/a", "https://in.test/a", "https://in.test/b"),
      to = c("https://in.test/dead", "https://out.test/x", "https://in.test/a"),
      stringsAsFactors = FALSE
    )
    pr <- pagerank(
      edges,
      status_df = data.frame(url = "https://in.test/dead", status_code = 500L),
      keep_hosts = "in.test",
      out_of_scope_fold = "leak",
      prior_verbose = FALSE
    )
    mass <- attr(pr, "transition_audit")$mass
    expect_equal(mass$total, 1)
    expect_gt(mass$sink, 0)
  })
})

describe("the toggle is inert where there is nothing to exclude", {
  it("leaves a plain graph (no class, no prior) scoring the same either way", {
    edges <- data.frame(from = c("a", "b", "c"), to = c("b", "c", "a"))
    on <- pagerank(edges, prior_exclude_waste = TRUE, prior_verbose = FALSE)
    off <- pagerank(edges, prior_exclude_waste = FALSE, prior_verbose = FALSE)
    # Subsetting drops the transition_audit attribute (its config records the
    # flag and so differs); the scores themselves must be identical.
    cols <- c("node_name", "pagerank")
    expect_equal(on[cols], off[cols])
  })

  it("adds no prior_weight column when excluding without a prior_df", {
    edges <- data.frame(from = c("A", "A"), to = c("B", "dead"))
    pr <- pagerank(
      edges,
      status_df = data.frame(url = "dead", status_code = 404L),
      clean_edge_urls = FALSE, prior_verbose = FALSE
    )
    expect_false("prior_weight" %in% names(pr))
  })
})

describe("interaction with an authority prior", {
  it("gives the excluded class exactly zero teleport prior", {
    edges <- data.frame(
      from = c("https://s.test/a", "https://s.test/a", "https://s.test/b"),
      to = c("https://s.test/b", "https://s.test/dead", "https://s.test/a"),
      stringsAsFactors = FALSE
    )
    prior <- data.frame(
      url = c("https://s.test/a", "https://s.test/dead"),
      weight = c(50, 50)
    )
    pr <- pagerank(
      edges,
      status_df = data.frame(url = "https://s.test/dead", status_code = 404L),
      prior_df = prior,
      prior_exclude_waste = TRUE,
      prior_verbose = FALSE
    )
    expect_equal(pr$prior_weight[pr$node_name == "https://s.test/dead"], 0)
  })
})

describe("validation", {
  it("rejects a non-logical or NA prior_exclude_waste", {
    edges <- data.frame(from = "a", to = "b")
    expect_error(
      pagerank(edges, prior_exclude_waste = "yes"),
      "prior_exclude_waste"
    )
    expect_error(
      pagerank(edges, prior_exclude_waste = NA),
      "prior_exclude_waste"
    )
    expect_error(
      pagerank(edges, prior_exclude_waste = c(TRUE, FALSE)),
      "prior_exclude_waste"
    )
  })
})

describe("provenance", {
  it("records the flag in the transition_audit config", {
    edges <- data.frame(from = "a", to = "b")
    on <- attr(pagerank(edges, prior_verbose = FALSE), "transition_audit")
    off <- attr(
      pagerank(edges, prior_exclude_waste = FALSE, prior_verbose = FALSE),
      "transition_audit"
    )
    expect_true(on$config$prior_exclude_waste)
    expect_false(off$config$prior_exclude_waste)
  })
})
