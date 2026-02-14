# Tests for resolve_links()

describe("resolve_links basic functionality", {
  it("resolves redirects and deduplicates edges", {
    edges <- data.frame(
      from = c("A", "B", "C", "A"),
      to = c("B", "C", "D", "B"),
      stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("B", "C"),
      to = c("B_final", "C_final"),
      stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, redirects, clean_urls = FALSE)
    # B -> B_final, C -> C_final in both from and to columns
    # Duplicate A -> B_final should be collapsed
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    # All references to B and C should be resolved
    expect_false("B" %in% c(result$from, result$to))
    expect_false("C" %in% c(result$from, result$to))
  })

  it("works without redirects (just dedup)", {
    edges <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "B", "C"),
      stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, clean_urls = FALSE)
    expect_equal(nrow(result), 2)  # A->B deduped, B->C stays
    expect_equal(sort(result$from), c("A", "B"))
  })

  it("works with NULL redirects", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "C"), stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, redirects_df = NULL, clean_urls = FALSE)
    expect_equal(nrow(result), 2)
  })

  it("drops self-loops by default", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "C"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = "B", to = "A", stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, redirects, clean_urls = FALSE)
    # A -> B becomes A -> A (self-loop) after redirect B -> A
    # Self-loop should be dropped by default
    self_loops_present <- any(result$from == result$to)
    expect_false(self_loops_present)
  })

  it("keeps self-loops when self_loops = 'keep'", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "C"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = "B", to = "A", stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, redirects, self_loops = "keep",
                            clean_urls = FALSE)
    # A -> A self-loop should be present
    self_loops_present <- any(result$from == result$to)
    expect_true(self_loops_present)
  })
})


describe("resolve_links input validation", {
  it("errors on non-data-frame edge_list_df", {
    expect_error(
      resolve_links("not a df", clean_urls = FALSE),
      regexp = "must be a data frame"
    )
  })

  it("errors on missing columns", {
    edges <- data.frame(x = "A", y = "B", stringsAsFactors = FALSE)
    expect_error(
      resolve_links(edges, clean_urls = FALSE),
      regexp = "must have"
    )
  })

  it("errors on non-data-frame redirects_df", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(
      resolve_links(edges, redirects_df = "not a df", clean_urls = FALSE),
      regexp = "must be a data frame or NULL"
    )
  })

  it("errors on invalid clean_urls", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(
      resolve_links(edges, clean_urls = "yes"),
      regexp = "must be TRUE or FALSE"
    )
  })
})


describe("resolve_links policy passthrough", {
  it("passes duplicate_from_policy to resolve_redirects", {
    edges <- data.frame(from = "X", to = "A", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("A", "A"), to = c("B", "C"), stringsAsFactors = FALSE
    )
    # strict should error
    expect_error(
      resolve_links(edges, redirects, clean_urls = FALSE),
      regexp = "Ambiguous redirect"
    )
    # first_wins should not error
    result <- resolve_links(edges, redirects, clean_urls = FALSE,
                            duplicate_from_policy = "first_wins")
    expect_equal(result$to, "B")
  })

  it("passes loop_handling to resolve_redirects", {
    edges <- data.frame(from = "X", to = "A", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("A", "B"), to = c("B", "A"), stringsAsFactors = FALSE
    )
    # error should error
    expect_error(
      resolve_links(edges, redirects, clean_urls = FALSE),
      regexp = "Redirect cycle detected"
    )
    # prune_loop should not error
    result <- resolve_links(edges, redirects, clean_urls = FALSE,
                            loop_handling = "prune_loop")
    expect_true(is.data.frame(result))
  })
})


describe("resolve_links custom column names", {
  it("works with custom column names", {
    edges <- data.frame(
      source = c("A", "B"), target = c("B", "C"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      old_url = "B", new_url = "B_final", stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, redirects, clean_urls = FALSE,
                            edge_from_col = "source",
                            edge_to_col = "target",
                            redirect_from_col = "old_url",
                            redirect_to_col = "new_url")
    expect_true("B_final" %in% c(result$source, result$target))
    expect_false("B" %in% c(result$source, result$target))
  })
})


describe("resolve_links preserves extra columns", {
  it("keeps extra columns from edge_list_df", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("B", "C"),
      weight = c(1.0, 2.0),
      stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, clean_urls = FALSE)
    expect_true("weight" %in% names(result))
  })
})


describe("resolve_links chain resolution", {
  it("resolves multi-hop chains", {
    edges <- data.frame(from = "Start", to = "A", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "Final"),
      stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, redirects, clean_urls = FALSE)
    expect_equal(result$to, "Final")
  })

  it("handles convergent redirects (diamond)", {
    edges <- data.frame(
      from = c("X", "Y"), to = c("A", "B"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("A", "B"), to = c("Final", "Final"), stringsAsFactors = FALSE
    )
    result <- resolve_links(edges, redirects, clean_urls = FALSE)
    # X -> Final and Y -> Final
    expect_true(all(result$to == "Final"))
    # If X and Y are different, both edges remain
    expect_equal(nrow(result), 2)
  })
})
