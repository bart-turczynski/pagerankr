describe("simulate_changes basic functionality", {
  # Shared test graph: A -> B -> C -> A (triangle)
  base_edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A")
  )

  it("returns a comparison data frame with node_status", {
    result <- simulate_changes(
      base_edges,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    expect_s3_class(result, "data.frame")
    expect_false(is.null(attr(result, "summary")))
    expect_true("node_status" %in% names(result))
    expect_true(all(result$node_status == "normal"))
  })

  it("with no changes, baseline and proposed are identical", {
    result <- simulate_changes(
      base_edges,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    # All deltas should be 0
    expect_true(all(result$delta == 0))
    s <- attr(result, "summary")
    expect_equal(s$mean_abs_delta, 0)
    expect_equal(s$nodes_gained, 0)
    expect_equal(s$nodes_lost, 0)
  })

  it("attaches the full proposed pagerank() result", {
    result <- simulate_changes(
      base_edges,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    proposed <- attr(result, "proposed")
    expect_s3_class(proposed, "data.frame")
    expect_true("node_name" %in% names(proposed))
    expect_false(is.null(attr(proposed, "transition_audit")))
  })

  it("attaches a change manifest", {
    result <- simulate_changes(
      base_edges,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    m <- attr(result, "manifest")
    expect_type(m, "list")
    expect_equal(m$links_added, 0L)
    expect_equal(m$links_removed, 0L)
    expect_length(m$unknown_targets, 0)
  })
})

describe("simulate_changes adding links", {
  base_edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A")
  )

  it("adding a link to a page increases its PR", {
    # Add A -> C (A now links to both B and C)
    add <- data.frame(from = "A", to = "C")
    result <- simulate_changes(
      base_edges,
      add_links_df = add,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    c_row <- result[result$node_name == "C", ]
    # C should gain PR (gets a direct link from A now)
    expect_gt(c_row$delta, 0)
    expect_equal(attr(result, "manifest")$links_added, 1L)
  })

  it("adding a new node warns and flags it new-target", {
    add <- data.frame(from = "A", to = "NewPage")
    expect_warning(
      result <- simulate_changes(
        base_edges,
        add_links_df = add,
        clean_edge_urls = FALSE,
        clean_redirect_urls = FALSE
      ),
      "NewPage"
    )
    s <- attr(result, "summary")
    expect_gt(s$nodes_gained, 0)
    expect_true("NewPage" %in% result$node_name)
    new_row <- result[result$node_name == "NewPage", ]
    expect_equal(new_row$node_status, "new-target")
    expect_true("NewPage" %in% attr(result, "manifest")$unknown_targets)
  })

  it("keeps a weighted edge schema when adding links", {
    weighted <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      weight = c(1, 1, 1)
    )
    # The added link supplies its own weight; the schema (weight column) must
    # survive the bind so pagerank(weight_col=) still finds it. Existing node
    # C, so no unknown-target warning.
    add <- data.frame(from = "A", to = "C", weight = 2)
    result <- simulate_changes(
      weighted,
      add_links_df = add,
      weight_col = "weight",
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    expect_s3_class(result, "data.frame")
  })
})

describe("simulate_changes removing links", {
  base_edges <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "C", "A")
  )

  it("removing a link reduces PR for the target page", {
    # Remove A -> C
    remove <- data.frame(from = "A", to = "C")
    result <- simulate_changes(
      base_edges,
      remove_links_df = remove,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    c_row <- result[result$node_name == "C", ]
    # C loses a direct link from A, so its PR should decrease
    expect_lt(c_row$delta, 0)
    expect_equal(attr(result, "manifest")$links_removed, 1L)
  })
})

describe("simulate_changes redirect_urls_df (retire semantics)", {
  it("retires the source: it disappears from the proposed set", {
    # B -> A, A -> X, Z -> C. Retire A -> C.
    edges <- data.frame(
      from = c("B", "A", "Z"),
      to = c("A", "X", "C")
    )
    redir <- data.frame(from = "A", to = "C")
    result <- simulate_changes(
      edges,
      redirect_urls_df = redir,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      drop_isolates_flag = TRUE
    )
    a_row <- result[result$node_name == "A", ]
    # A is folded into C: present in baseline, absent (NA) in proposed.
    expect_true(is.na(a_row$pagerank_proposed))
    # C inherits A's inbound authority -> it gains.
    c_row <- result[result$node_name == "C", ]
    expect_gt(c_row$delta, 0)
  })

  it("strips the source's outedges (retire, not move)", {
    # The bug: folding a live source relabels its outlink A->X to C->X, so the
    # target would inherit the source's outlinks. Retire must strip them.
    edges <- data.frame(
      from = c("B", "A", "Z"),
      to = c("A", "X", "C")
    )
    redir <- data.frame(from = "A", to = "C")
    result <- simulate_changes(
      edges,
      redirect_urls_df = redir,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE,
      drop_isolates_flag = TRUE
    )
    # X's only inbound was A->X. With retire, that edge is stripped and C does
    # NOT inherit it, so X has no inbound/outbound and drops out (NA proposed).
    x_row <- result[result$node_name == "X", ]
    expect_true(is.na(x_row$pagerank_proposed))
  })

  it("passes inbound authority through at 100% (no redirect decay)", {
    # Pure relabel: retire A -> C where A has no outlinks. C should end up with
    # exactly the mass A held (same graph, one node renamed).
    edges <- data.frame(
      from = c("H", "H", "P"),
      to = c("A", "P", "H")
    )
    redir <- data.frame(from = "A", to = "C")
    result <- simulate_changes(
      edges,
      redirect_urls_df = redir,
      on_unknown_target = "allow", # C is a fresh node; relabel, not a warning
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    a_base <- result[result$node_name == "A", "pagerank_baseline"]
    c_prop <- result[result$node_name == "C", "pagerank_proposed"]
    # A was a pure sink (no outlinks); relabeling A -> C conserves its score.
    expect_equal(c_prop, a_base, tolerance = 1e-6)
  })

  it("overrides a prior baseline redirect and records it in the manifest", {
    # Baseline redirect A -> B; changeset repoints A -> C.
    edges <- data.frame(
      from = c("X", "W", "V"),
      to = c("A", "B", "C")
    )
    baseline_redir <- data.frame(from = "A", to = "B")
    change <- data.frame(from = "A", to = "C")
    result <- simulate_changes(
      edges,
      redirects_df = baseline_redir,
      redirect_urls_df = change,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    # In baseline X -> A folds to B; in proposed it folds to C instead.
    c_row <- result[result$node_name == "C", ]
    b_row <- result[result$node_name == "B", ]
    expect_gt(c_row$delta, 0) # C gains X's authority
    expect_lt(b_row$delta, 0) # B loses it
    expect_true("A" %in% attr(result, "manifest")$redirects_overrode)
  })
})

describe("simulate_changes on_unknown_target", {
  base_edges <- data.frame(from = c("A", "B"), to = c("B", "A"))

  it("warns by default on an unknown redirect target", {
    redir <- data.frame(from = "A", to = "BrandNew")
    expect_warning(
      simulate_changes(
        base_edges,
        redirect_urls_df = redir,
        clean_edge_urls = FALSE,
        clean_redirect_urls = FALSE
      ),
      "BrandNew"
    )
  })

  it("errors when on_unknown_target = 'error'", {
    redir <- data.frame(from = "A", to = "BrandNew")
    expect_error(
      simulate_changes(
        base_edges,
        redirect_urls_df = redir,
        on_unknown_target = "error",
        clean_edge_urls = FALSE,
        clean_redirect_urls = FALSE
      ),
      "BrandNew"
    )
  })

  it("is silent when on_unknown_target = 'allow'", {
    redir <- data.frame(from = "A", to = "BrandNew")
    expect_no_warning(
      simulate_changes(
        base_edges,
        redirect_urls_df = redir,
        on_unknown_target = "allow",
        clean_edge_urls = FALSE,
        clean_redirect_urls = FALSE
      )
    )
  })
})

describe("simulate_changes validation", {
  edges <- data.frame(from = "A", to = "B")

  it("errors on non-dataframe edge_list_df", {
    expect_error(simulate_changes("bad"), "data frame")
  })

  it("errors on non-dataframe add_links_df", {
    expect_error(
      simulate_changes(edges, add_links_df = "bad"),
      "data frame"
    )
  })

  it("errors on non-dataframe remove_links_df", {
    expect_error(
      simulate_changes(edges, remove_links_df = 42),
      "data frame"
    )
  })

  it("errors on non-dataframe redirect_urls_df", {
    expect_error(
      simulate_changes(edges, redirect_urls_df = "bad"),
      "data frame"
    )
  })

  it("errors on non-dataframe redirects_df", {
    expect_error(
      simulate_changes(edges, redirects_df = "bad"),
      "data frame"
    )
  })

  it("errors when add_links_df is missing required columns", {
    bad_add <- data.frame(x = "A", y = "B")
    expect_error(
      simulate_changes(edges, add_links_df = bad_add),
      "columns"
    )
  })

  it("errors when redirect_urls_df is missing required columns", {
    bad <- data.frame(x = "A", y = "B")
    expect_error(
      simulate_changes(edges, redirect_urls_df = bad),
      "columns"
    )
  })

  it("errors on a duplicate source with distinct targets (strict)", {
    dup <- data.frame(from = c("A", "A"), to = c("B", "C"))
    expect_error(
      simulate_changes(edges, redirect_urls_df = dup),
      "multiple distinct targets"
    )
  })

  it("allows an exact-duplicate redirect row", {
    dup <- data.frame(from = c("A", "A"), to = c("B", "B"))
    # A -> B twice is not a conflict; B is a known node so no unknown warning.
    result <- simulate_changes(
      edges,
      redirect_urls_df = dup,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    expect_s3_class(result, "data.frame")
  })

  it("rejects the removed add_redirects_df argument with guidance", {
    gone <- data.frame(from = "A", to = "B")
    expect_error(
      simulate_changes(edges, add_redirects_df = gone),
      "redirect_urls_df"
    )
  })
})

describe("simulate_changes passthrough args", {
  base_edges <- data.frame(
    from = c("A", "B"), to = c("B", "A")
  )

  it("passes ... to pagerank()", {
    result <- simulate_changes(base_edges,
      clean_edge_urls = FALSE,
      damping = 0.5
    )
    expect_s3_class(result, "data.frame")
  })

  it("uses custom labels", {
    result <- simulate_changes(base_edges,
      clean_edge_urls = FALSE,
      label_baseline = "before",
      label_proposed = "after"
    )
    expect_true("pagerank_before" %in% names(result))
    expect_true("pagerank_after" %in% names(result))
  })
})
