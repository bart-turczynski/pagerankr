context("presets")

nf_edges <- function() {
  data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "A", "A"),
    nofollow = c(FALSE, TRUE, FALSE, FALSE)
  )
}

describe("pr_preset()", {
  it("returns a named list of pagerank() arguments", {
    raw <- pr_preset("raw")
    expect_type(raw, "list")
    expect_equal(raw$nofollow_action, "keep")
    expect_false(raw$drop_isolates_flag)
    expect_equal(raw$self_loops, "keep")
    expect_equal(raw$out_of_scope_fold, "keep")
  })

  it("returns the declared bundle", {
    declared <- pr_preset("declared")
    expect_equal(declared$nofollow_action, "evaporate")
    expect_equal(declared$robots_blocked_action, "vanish")
    expect_true(declared$drop_isolates_flag)
    expect_equal(declared$self_loops, "drop")
    expect_equal(declared$out_of_scope_fold, "relabel")
  })

  it("only ever names real pagerank() arguments", {
    formal_names <- names(formals(pagerank))
    for (bundle in pagerankr:::.pr_preset_registry()) {
      expect_named(bundle)
      expect_length(setdiff(names(bundle), formal_names), 0)
    }
  })

  it("rejects an unknown preset name and lists the available ones", {
    expect_error(pr_preset("nope"), "Unknown preset")
    expect_error(pr_preset("nope"), "raw")
  })

  it("rejects a non-string name", {
    expect_error(pr_preset(1), "single preset name")
    expect_error(pr_preset(c("raw", "declared")), "single preset name")
    expect_error(pr_preset(NA_character_), "single preset name")
  })
})

describe("preset = on pagerank()", {
  it("leaves default behavior unchanged when preset is NULL", {
    edges <- nf_edges()
    expect_equal(
      pagerank(edges, nofollow_col = "nofollow", clean_edge_urls = FALSE),
      pagerank(
        edges,
        nofollow_col = "nofollow",
        clean_edge_urls = FALSE,
        preset = NULL
      )
    )
  })

  it("applies a named preset", {
    edges <- nf_edges()
    default <- pagerank(
      edges,
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE
    )
    raw <- pagerank(
      edges,
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      preset = "raw"
    )
    # Default evaporates the nofollowed edge; "raw" keeps it, so raw retains
    # the full unit of PageRank mass.
    expect_lt(sum(default$pagerank), 1)
    expect_equal(sum(raw$pagerank), 1)
    expect_equal(attr(raw, "transition_audit")$mass$sink, 0)
  })

  it("accepts a pr_preset() result as a list", {
    edges <- nf_edges()
    by_name <- pagerank(
      edges,
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      preset = "raw"
    )
    by_list <- pagerank(
      edges,
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      preset = pr_preset("raw")
    )
    expect_equal(by_name, by_list)
  })

  it("accepts a hand-rolled bundle", {
    edges <- nf_edges()
    expect_equal(
      pagerank(
        edges,
        nofollow_col = "nofollow",
        clean_edge_urls = FALSE,
        preset = list(nofollow_action = "keep")
      )$pagerank,
      pagerank(
        edges,
        nofollow_col = "nofollow",
        clean_edge_urls = FALSE,
        nofollow_action = "keep"
      )$pagerank
    )
  })

  it("is spliceable through do.call()", {
    edges <- nf_edges()
    expect_equal(
      do.call(
        pagerank,
        c(
          list(edges, nofollow_col = "nofollow", clean_edge_urls = FALSE),
          pr_preset("raw")
        )
      ),
      pagerank(
        edges,
        nofollow_col = "nofollow",
        clean_edge_urls = FALSE,
        preset = "raw"
      )
    )
  })

  it("applies the declared preset's robots handling", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
    indexability <- data.frame(
      url = "C",
      indexability_status = "Blocked by robots.txt"
    )
    declared <- pagerank(
      edges,
      indexability_df = indexability,
      clean_edge_urls = FALSE,
      preset = "declared"
    )
    # robots_blocked_action = "vanish": the blocked page leaves the results.
    expect_false("C" %in% declared$node_name)
    expect_gt(attr(declared, "transition_audit")$mass$hidden, 0)
  })
})

describe("preset precedence (explicit arg > preset > default)", {
  it("an explicit argument wins over the preset", {
    edges <- nf_edges()
    overridden <- pagerank(
      edges,
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      preset = "raw",
      nofollow_action = "evaporate"
    )
    expect_equal(
      attr(overridden, "transition_audit")$config$nofollow_action,
      "evaporate"
    )
    expect_lt(sum(overridden$pagerank), 1)
    # The preset's other values still apply -- only the named one is protected.
    expect_false(attr(overridden, "transition_audit")$config$drop_isolates_flag)
  })

  it("an explicit argument wins over a list preset", {
    edges <- nf_edges()
    overridden <- pagerank(
      edges,
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      preset = pr_preset("raw"),
      nofollow_action = "evaporate"
    )
    expect_lt(sum(overridden$pagerank), 1)
  })

  it("an explicit argument matched positionally still wins", {
    edges <- data.frame(from = c("A", "B", "ISO"), to = c("B", "C", NA))
    # drop_isolates_flag is the 7th formal; "raw" would set it FALSE.
    positional <- pagerank(
      edges,
      NULL,
      FALSE,
      TRUE,
      list(),
      "drop",
      TRUE,
      preset = "raw"
    )
    expect_false("ISO" %in% positional$node_name)
  })

  it("holds through a ...-forwarding wrapper", {
    edges <- nf_edges()
    raw <- trustrank(
      edges,
      seeds = "A",
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      preset = "raw"
    )
    overridden <- trustrank(
      edges,
      seeds = "A",
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      preset = "raw",
      nofollow_action = "evaporate"
    )
    expect_equal(sum(raw$pagerank), 1)
    expect_lt(sum(overridden$pagerank), 1)
  })
})

describe("preset validation", {
  it("rejects a preset that is neither a name nor a list", {
    edges <- nf_edges()
    expect_error(
      pagerank(edges, clean_edge_urls = FALSE, preset = 1),
      "must be a preset name"
    )
  })

  it("rejects unnamed and duplicated bundle entries", {
    edges <- nf_edges()
    expect_error(
      pagerank(edges, clean_edge_urls = FALSE, preset = list("keep")),
      "must be named"
    )
    dupes <- stats::setNames(list(0.5, 0.6), c("damping", "damping"))
    expect_error(
      pagerank(edges, clean_edge_urls = FALSE, preset = dupes),
      "Duplicated"
    )
  })

  it("rejects reserved and unknown arguments in a bundle", {
    edges <- nf_edges()
    expect_error(
      pagerank(
        edges,
        clean_edge_urls = FALSE,
        preset = list(edge_list_df = edges)
      ),
      "cannot set"
    )
    expect_error(
      pagerank(edges, clean_edge_urls = FALSE, preset = list(preset = "raw")),
      "cannot set"
    )
    expect_error(
      pagerank(edges, clean_edge_urls = FALSE, preset = list(dampng = 0.5)),
      "does not have"
    )
  })
})

describe(".pr_apply_preset()", {
  it("reports which arguments it applied", {
    env <- new.env(parent = emptyenv())
    applied <- pagerankr:::.pr_apply_preset(
      "raw",
      quote(pagerank(edges, nofollow_action = "drop")),
      env
    )
    expect_equal(
      sort(applied),
      sort(c("self_loops", "drop_isolates_flag", "out_of_scope_fold"))
    )
    expect_false(exists("nofollow_action", envir = env, inherits = FALSE))
    expect_equal(get("self_loops", envir = env), "keep")
  })

  it("does nothing for a NULL preset", {
    env <- new.env(parent = emptyenv())
    expect_length(
      pagerankr:::.pr_apply_preset(NULL, quote(pagerank(e)), env),
      0
    )
    expect_length(ls(env), 0)
  })
})
