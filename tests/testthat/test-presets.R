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
    expect_equal(declared$robots_blocked_action, "show")
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
    spliced <- do.call(
      pagerank,
      c(
        list(edges, nofollow_col = "nofollow", clean_edge_urls = FALSE),
        pr_preset("raw")
      )
    )
    via_preset <- pagerank(
      edges,
      nofollow_col = "nofollow",
      clean_edge_urls = FALSE,
      preset = "raw"
    )
    # Same scores, same configuration. The audit's `preset` provenance field
    # differs on purpose: splicing passes plain arguments, so there is no
    # preset to attribute the run to.
    expect_equal(spliced$pagerank, via_preset$pagerank)
    strip_preset <- function(x) {
      cfg <- attr(x, "transition_audit")$config
      cfg[setdiff(names(cfg), "preset")]
    }
    expect_equal(strip_preset(spliced), strip_preset(via_preset))
    expect_null(attr(spliced, "transition_audit")$config$preset)
    expect_equal(attr(via_preset, "transition_audit")$config$preset, "raw")
  })

  it("pins the declared preset to the package defaults", {
    # "declared" is a pure pin: every value in the bundle must equal the
    # corresponding `pagerank()` default, so asking for the declared view can
    # never change a result -- it only records that the view was intended.
    defaults <- formals(pagerank)
    for (arg in names(pr_preset("declared"))) {
      default <- eval(defaults[[arg]])
      expect_equal(
        pr_preset("declared")[[arg]],
        if (is.character(default)) default[[1]] else default,
        info = arg
      )
    }
  })

  it("leaves results untouched under the declared preset", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
    indexability <- data.frame(
      url = "C",
      indexability_status = "Blocked by robots.txt"
    )
    args <- list(
      edges,
      indexability_df = indexability,
      clean_edge_urls = FALSE
    )
    declared <- do.call(pagerank, c(args, list(preset = "declared")))
    baseline <- do.call(pagerank, args)
    # robots_blocked_action = "show": the blocked page stays in the results,
    # holding the authority it collects.
    expect_true("C" %in% declared$node_name)
    expect_equal(declared$pagerank, baseline$pagerank)
  })
})

placed_edges <- function() {
  # B is linked from the main content, C only from the footer.
  data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "A", "A"),
    region = c("content", "footer", "content", "content")
  )
}

describe("the reversed preset", {
  it("flips the graph", {
    edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
    expect_equal(
      pagerank(edges, preset = "reversed")$pagerank,
      pagerank(edges, reverse = TRUE)$pagerank
    )
  })

  it("is a no-op under topic_feeder_pagerank(), which owns reverse", {
    # The wrapper names `reverse = TRUE` itself, so precedence (explicit arg >
    # preset) leaves the preset with nothing to change -- it must not error.
    edges <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "A"))
    seeds <- data.frame(url = "A", weight = 1)
    suppressMessages({
      with_preset <- topic_feeder_pagerank(edges, seeds, preset = "reversed")
      baseline <- topic_feeder_pagerank(edges, seeds)
    })
    expect_equal(with_preset$pagerank, baseline$pagerank)
  })
})

describe("the content preset", {
  it("names all five placements, so no region silently keeps weight 1", {
    weights <- pr_preset("content")$placement_weights
    expect_setequal(
      names(weights),
      c("content", "nav", "header", "footer", "aside")
    )
    expect_equal(unname(weights[["content"]]), 1)
    expect_true(all(weights[names(weights) != "content"] == 0.1))
  })

  it("sets placement weights only, leaving graph hygiene at the defaults", {
    # "content" is orthogonal to the hygiene presets: the defaults already are
    # the "declared" view, so the bundle must not restate them.
    expect_named(pr_preset("content"), "placement_weights")
  })

  it("downweights links found outside the main content", {
    edges <- placed_edges()
    scores <- pagerank(edges, preset = "content", placement_col = "region")
    b <- scores$pagerank[scores$node_name == "B"]
    c_score <- scores$pagerank[scores$node_name == "C"]
    expect_gt(b, c_score)

    # Unweighted, the two are indistinguishable -- A splits its vote evenly.
    flat <- pagerank(edges)
    expect_equal(
      flat$pagerank[flat$node_name == "B"],
      flat$pagerank[flat$node_name == "C"]
    )
  })

  it("downweights rather than drops: no region disappears from the graph", {
    edges <- placed_edges()
    scores <- pagerank(edges, preset = "content", placement_col = "region")
    expect_setequal(scores$node_name, c("A", "B", "C"))
  })

  it("records the placement factors in the transition audit", {
    edges <- placed_edges()
    scores <- pagerank(edges, preset = "content", placement_col = "region")
    config <- attr(scores, "transition_audit")$config
    expect_equal(config$preset, "content")
    expect_equal(config$placement$placement_col, "region")
    expect_equal(
      config$placement$placement_weights,
      pr_preset("content")$placement_weights
    )
  })

  it("works through pagerank(), not just the Screaming Frog wrapper", {
    # The edge list is a plain data frame with no crawler columns: placement is
    # crawler-neutral, so the preset must ride on bare pagerank().
    expect_s3_class(
      pagerank(placed_edges(), preset = "content", placement_col = "region"),
      "data.frame"
    )
  })

  it("errors without placement_col, naming the preset that set the policy", {
    err <- expect_error(
      pagerank(placed_edges(), preset = "content"),
      "requires `placement_col`"
    )
    expect_match(conditionMessage(err), "preset = \"content\"", fixed = TRUE)
  })

  it("does not blame a preset when the caller typed the argument", {
    err <- expect_error(
      pagerank(placed_edges(), placement_weights = c(nav = 0.1)),
      "requires `placement_col`"
    )
    expect_false(grepl("preset", conditionMessage(err), fixed = TRUE))
  })

  it("still lets an explicit placement_weights win over the preset", {
    edges <- placed_edges()
    override <- c(content = 1, nav = 1, header = 1, footer = 1, aside = 1)
    scores <- pagerank(
      edges,
      preset = "content",
      placement_col = "region",
      placement_weights = override
    )
    expect_equal(
      attr(scores, "transition_audit")$config$placement$placement_weights,
      override
    )
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

describe("preset provenance in the transition audit", {
  audit_of <- function(...) {
    attr(pagerank(...), "transition_audit")
  }

  it("records the preset name for a registry preset", {
    edges <- nf_edges()
    audit <- audit_of(edges, clean_edge_urls = FALSE, preset = "raw")
    expect_equal(audit$config$preset, "raw")
  })

  it("records \"custom\" for a hand-rolled bundle", {
    edges <- nf_edges()
    audit <- audit_of(
      edges,
      clean_edge_urls = FALSE,
      preset = list(nofollow_action = "keep")
    )
    expect_equal(audit$config$preset, "custom")
  })

  it("records NULL when no preset was used", {
    edges <- nf_edges()
    audit <- audit_of(edges, clean_edge_urls = FALSE)
    expect_null(audit$config$preset)
  })

  it("distinguishes a preset run from the same arguments typed by hand", {
    edges <- nf_edges()
    by_preset <- audit_of(edges, clean_edge_urls = FALSE, preset = "declared")
    by_hand <- audit_of(
      edges,
      clean_edge_urls = FALSE,
      self_loops = "drop",
      drop_isolates_flag = TRUE,
      nofollow_action = "evaporate",
      out_of_scope_fold = "relabel"
    )
    expect_equal(by_preset$config$preset, "declared")
    expect_null(by_hand$config$preset)
    # Everything else about the two configs is identical -- the preset field is
    # the only thing carrying which named view was asked for.
    expect_equal(
      by_preset$config[setdiff(names(by_preset$config), "preset")],
      by_hand$config[setdiff(names(by_hand$config), "preset")]
    )
  })

  it("records the preset through a ...-forwarding wrapper", {
    edges <- nf_edges()
    audit <- attr(
      trustrank(edges, seeds = "A", clean_edge_urls = FALSE, preset = "raw"),
      "transition_audit"
    )
    expect_equal(audit$config$preset, "raw")
  })

  it("prints the preset only when one was used", {
    edges <- nf_edges()
    with_preset <- capture.output(
      print(audit_of(edges, clean_edge_urls = FALSE, preset = "raw"))
    )
    without <- capture.output(print(audit_of(edges, clean_edge_urls = FALSE)))
    expect_true(any(grepl("Preset:\\s+raw", with_preset)))
    expect_false(any(grepl("Preset:", without, fixed = TRUE)))
  })
})
