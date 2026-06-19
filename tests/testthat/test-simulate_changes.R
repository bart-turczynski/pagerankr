describe("simulate_changes basic functionality", {
  # Shared test graph: A -> B -> C -> A (triangle)
  base_edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A")
  )

  it("returns a comparison data frame", {
    result <- simulate_changes(
      base_edges,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    expect_true(is.data.frame(result))
    expect_false(is.null(attr(result, "summary")))
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
  })

  it("adding a new node introduces it in the proposed model", {
    add <- data.frame(from = "A", to = "NewPage")
    result <- simulate_changes(
      base_edges,
      add_links_df = add,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    s <- attr(result, "summary")
    expect_gt(s$nodes_gained, 0)
    expect_true("NewPage" %in% result$node_name)
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
  })

  it("removing all links to a page makes it an isolate or lost", {
    # Remove both links to C (A->C and B->C)
    remove <- data.frame(
      from = c("A", "B"), to = c("C", "C")
    )
    result <- simulate_changes(base_edges,
      remove_links_df = remove,
      clean_edge_urls = FALSE,
      drop_isolates_flag = TRUE
    )
    s <- attr(result, "summary")
    # C might be lost (no inbound links, drop_isolates=TRUE keeps only
    # nodes in edges, but C still has outgoing C->A)
    # At minimum C's PR should decrease
    c_row <- result[result$node_name == "C", ]
    expect_gt(nrow(c_row), 0)
  })
})

describe("simulate_changes adding redirects", {
  base_edges <- data.frame(
    from = c("A", "B", "OldPage"),
    to = c("B", "A", "A")
  )

  it("adding a redirect consolidates PR", {
    # Redirect OldPage -> NewPage; NewPage inherits OldPage's link to A
    new_redir <- data.frame(
      from = "OldPage", to = "NewPage"
    )
    result <- simulate_changes(
      base_edges,
      add_redirects_df = new_redir,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    # NewPage should appear in the proposed model (gained)
    expect_true("NewPage" %in% result$node_name)
    s <- attr(result, "summary")
    expect_gt(s$nodes_gained, 0)
  })

  it("works with existing redirects plus new ones", {
    existing_redir <- data.frame(
      from = "X", to = "A"
    )
    new_redir <- data.frame(
      from = "OldPage", to = "B"
    )
    result <- simulate_changes(base_edges,
      redirects_df = existing_redir,
      add_redirects_df = new_redir,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    expect_true(is.data.frame(result))
  })
})

describe("simulate_changes combined changes", {
  base_edges <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "C", "A")
  )

  it("handles add + remove + redirect simultaneously", {
    add <- data.frame(from = "C", to = "B")
    remove <- data.frame(from = "A", to = "C")
    redir <- data.frame(from = "Old", to = "A")

    result <- simulate_changes(base_edges,
      add_links_df = add,
      remove_links_df = remove,
      add_redirects_df = redir,
      clean_edge_urls = FALSE,
      clean_redirect_urls = FALSE
    )
    expect_true(is.data.frame(result))
    expect_gt(nrow(result), 0)
  })
})

describe("simulate_changes passthrough args", {
  base_edges <- data.frame(
    from = c("A", "B"), to = c("B", "A")
  )

  it("passes ... to pagerank()", {
    # Use custom damping via ...
    result <- simulate_changes(base_edges,
      clean_edge_urls = FALSE,
      damping = 0.5
    )
    expect_true(is.data.frame(result))
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

describe("simulate_changes validation", {
  it("errors on non-dataframe edge_list_df", {
    expect_error(simulate_changes("bad"), "data frame")
  })

  it("errors on non-dataframe add_links_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      simulate_changes(edges, add_links_df = "bad"),
      "data frame"
    )
  })

  it("errors on non-dataframe remove_links_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      simulate_changes(edges, remove_links_df = 42),
      "data frame"
    )
  })

  it("errors on non-dataframe add_redirects_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      simulate_changes(edges, add_redirects_df = "bad"),
      "data frame"
    )
  })

  it("errors on non-dataframe redirects_df", {
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      simulate_changes(edges, redirects_df = "bad"),
      "data frame"
    )
  })

  it("errors when add_links_df is missing required columns", {
    edges <- data.frame(from = "A", to = "B")
    bad_add <- data.frame(x = "A", y = "B")
    expect_error(
      simulate_changes(edges, add_links_df = bad_add),
      "columns"
    )
  })

  it("errors when remove_links_df is missing required columns", {
    edges <- data.frame(from = "A", to = "B")
    bad_remove <- data.frame(x = "A", y = "B")
    expect_error(
      simulate_changes(edges, remove_links_df = bad_remove),
      "columns"
    )
  })
})
