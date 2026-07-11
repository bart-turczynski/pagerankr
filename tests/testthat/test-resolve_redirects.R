context("resolve_redirects")

describe("resolve_redirects basic functionality", {
  it("resolves direct redirects", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(from = "B", to = "C")
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$from, "A")
    expect_equal(resolved$to, "C")
  })

  it("resolves transitive (chained) redirects", {
    edges <- data.frame(from = "X", to = "Y")
    redirects <- data.frame(
      from = c("Y", "Z", "AA"),
      to = c("Z", "FINAL_Z", "FINAL_AA")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$from, "X")
    expect_equal(resolved$to, "FINAL_Z")

    edges2 <- data.frame(from = "AA", to = "Y")
    resolved2 <- resolve_redirects(edges2, redirects)
    expect_equal(resolved2$from, "FINAL_AA")
    expect_equal(resolved2$to, "FINAL_Z")
  })

  it("handles URLs not in the redirect map", {
    edges <- data.frame(from = "A", to = "B")
    # B is not in redirects
    redirects <- data.frame(
      from = "C", to = "D"
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$from, "A") # A is not in redirects
    expect_equal(resolved$to, "B") # B is not resolved
  })

  it("handles empty edge list", {
    edges_empty <- data.frame(
      from = character(0), to = character(0)
    )
    redirects <- data.frame(from = "A", to = "B")
    resolved <- resolve_redirects(edges_empty, redirects)
    expect_equal(nrow(resolved), 0)
    expect_named(resolved, c("from", "to"))
  })

  it("handles empty redirect list", {
    edges <- data.frame(from = "A", to = "B")
    redirects_empty <- data.frame(
      from = character(0), to = character(0)
    )
    resolved <- resolve_redirects(edges, redirects_empty)
    expect_equal(resolved$from, "A")
    expect_equal(resolved$to, "B")
    expect_equal(resolved, edges)
  })

  it("handles NA values in edge list gracefully", {
    edges_na <- data.frame(
      from = c("A", NA, "C"),
      to = c(NA, "B", "D")
    )
    redirects <- data.frame(
      from = c("A", "D"),
      to = c("FINAL_A", "FINAL_D")
    )
    resolved <- resolve_redirects(edges_na, redirects)
    expect_equal(resolved$from, c("FINAL_A", NA, "C"))
    expect_equal(resolved$to, c(NA, "B", "FINAL_D"))
  })

  it("handles NA values in redirect list (rules with NA are ignored)", {
    edges <- data.frame(
      from = c("A", "B"), to = c("X", "Y")
    )
    redirects_na <- data.frame(
      from = c("X", NA, "Y", "Z"),
      to = c("FINAL_X", "W", NA, "FINAL_Z")
    )
    # X -> FINAL_X. NA rules ignored. Y stays Y.
    resolved <- resolve_redirects(edges, redirects_na)
    expect_equal(resolved$to, c("FINAL_X", "Y"))
  })

  it("works with custom column names", {
    edges_custom <- data.frame(
      source = "Page1", destination = "Page2"
    )
    redirects_custom <- data.frame(
      orig = "Page2", new = "Page2_Final"
    )
    resolved <- resolve_redirects(
      edges_custom, redirects_custom,
      edge_from_col = "source",
      edge_to_col = "destination",
      redirect_from_col = "orig",
      redirect_to_col = "new"
    )
    expect_equal(resolved$source, "Page1")
    expect_equal(resolved$destination, "Page2_Final")
  })
})

describe("resolve_redirects error handling", {
  it("detects and errors on redirect cycles", {
    edges <- data.frame(from = "Start", to = "L1")
    redirects_cycle <- data.frame(
      from = c("L1", "L2", "L3"),
      to = c("L2", "L3", "L1")
    )
    expect_error(
      resolve_redirects(edges, redirects_cycle),
      regexp = "Redirect cycle detected: L1 -> L2 -> L3 -> L1"
    )
    # Self-refs are silently filtered, not cycles.
    # Multi-hop cycles (L1->L2->L3->L1) still error.
  })

  it("detects and errors on redirect ambiguities", {
    edges <- data.frame(
      from = "Source", to = "A_link"
    )
    redirects_ambiguous <- data.frame(
      from = c("A_link", "A_link", "Other"),
      to = c("Target1", "Target2", "TargetOther")
    )
    expect_error(
      resolve_redirects(edges, redirects_ambiguous),
      regexp = "Ambiguous redirect.*A_link"
    )

    # One distinct target (not an error)
    redirects_not_ambiguous <- data.frame(
      from = c("A_link", "A_link"),
      to = c("Target1", "Target1")
    )
    resolved_ok <- resolve_redirects(edges, redirects_not_ambiguous)
    expect_equal(resolved_ok$to, "Target1")
  })

  it("errors on invalid input types", {
    bad_df <- data.frame(f = "a", t = "b")
    expect_error(resolve_redirects(list(), bad_df))
    expect_error(resolve_redirects(bad_df, list()))
  })

  it("errors if columns are missing and df is not empty", {
    df <- data.frame(
      fcol = "a", tcol = "b"
    )
    rdf <- data.frame(
      from_col = "b", to_col = "c"
    )
    expect_error(resolve_redirects(df, rdf))
    expect_error(resolve_redirects(df, rdf, edge_from_col = "x"))
    expect_error(
      resolve_redirects(df, rdf, redirect_from_col = "y")
    )
  })
})

describe("resolve_redirects with more complex scenarios", {
  it("handles a mix of direct, chained, and no redirects", {
    edges <- data.frame(
      source = c("A", "B", "C", "D", "E"),
      target = c("X", "Y", "Z", "P", "Q")
    )
    redirects <- data.frame(
      orig = c("X", "Y", "Z1", "P", "R"),
      final = c(
        "X_final", "Y_intermediate",
        "Z_final", "P_final", "R_final"
      )
    )
    redirects_chain <- data.frame(
      orig = "Y_intermediate", final = "Y_final"
    )
    all_redirects <- rbind(redirects, redirects_chain)

    resolved <- resolve_redirects(
      edges, all_redirects,
      edge_from_col = "source",
      edge_to_col = "target",
      redirect_from_col = "orig",
      redirect_to_col = "final"
    )

    expected_targets <- c(
      "X_final", "Y_final", "Z", "P_final", "Q"
    )
    expect_equal(resolved$target, expected_targets)
    # Sources are not in redirect from column
    expect_equal(resolved$source, edges$source)
  })

  it("multiple edges resolve correctly", {
    edges <- data.frame(
      from = c("N1", "N1", "N2", "N3"),
      to = c("R1", "R2", "R1", "R3")
    )
    redirects <- data.frame(
      from = c("R1", "R2", "R3"),
      to = c("FINAL_R1", "FINAL_R2", "FINAL_R3")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(
      resolved$from, c("N1", "N1", "N2", "N3")
    )
    expect_equal(
      resolved$to,
      c("FINAL_R1", "FINAL_R2", "FINAL_R1", "FINAL_R3")
    )
  })

  it("resolves URLs in both from and to columns of edges", {
    edges <- data.frame(
      from = "A", to = "B"
    )
    redirects <- data.frame(
      from = c("A", "B"),
      to = c("A_final", "B_final")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$from, "A_final")
    expect_equal(resolved$to, "B_final")
  })
})

describe("resolve_redirects self-referencing redirect handling", {
  it("silently filters self-referencing redirects (from == to)", {
    edges <- data.frame(
      from = "Initial", to = "S"
    )
    redirects_self <- data.frame(
      from = "S", to = "S"
    )
    # S -> S silently filtered. S stays S.
    resolved <- resolve_redirects(edges, redirects_self)
    expect_equal(resolved$from, "Initial")
    expect_equal(resolved$to, "S")
  })

  it("filters self-refs while keeping valid ones", {
    edges <- data.frame(
      from = c("A", "B"), to = c("X", "Y")
    )
    redirects <- data.frame(
      from = c("X", "Y", "Z"),
      to = c("X", "Y_final", "Z_final")
    )
    # X -> X is self-ref (filtered). Y -> Y_final is valid.
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$to, c("X", "Y_final"))
  })

  it("returns edge list unchanged when all self-refs", {
    edges <- data.frame(
      from = "A", to = "B"
    )
    redirects <- data.frame(
      from = c("X", "Y"),
      to = c("X", "Y")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved, edges)
  })

  it("errors when redirects_df is not a data frame", {
    edges <- data.frame(
      from = "A", to = "B"
    )
    expect_error(
      resolve_redirects(edges, "not_a_df"), "data frame"
    )
  })

  it("errors when redirects_df is missing required columns", {
    edges <- data.frame(
      from = "A", to = "B"
    )
    redirects <- data.frame(
      x = "A", y = "B"
    )
    expect_error(resolve_redirects(edges, redirects), "columns")
  })
})

# ===========================================================================
# duplicate_from_policy tests
# ===========================================================================

describe("duplicate_from_policy = 'strict' (default)", {
  it("errors on conflicting redirects", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(
      from = c("B", "B"), to = c("C", "D")
    )
    expect_error(
      resolve_redirects(edges, redirects),
      "Ambiguous redirect.*B.*C, D"
    )
  })

  it("allows exact duplicate redirects (same from and to)", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(
      from = c("B", "B"), to = c("C", "C")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$to, "C")
  })

  it("passes through clean redirects unchanged", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(from = "B", to = "C")
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "strict"
    )
    expect_equal(resolved$to, "C")
  })
})

describe("duplicate_from_policy = 'first_wins'", {
  it("keeps the first target for conflicting sources", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(
      from = c("B", "B", "B"), to = c("C", "D", "E")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "first_wins"
    )
    expect_equal(resolved$to, "C")
  })

  it("preserves non-conflicting redirects alongside conflicting ones", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y")
    )
    redirects <- data.frame(
      from = c("B", "B", "Y"), to = c("C", "D", "Z")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "first_wins"
    )
    expect_equal(resolved$to, c("C", "Z"))
  })

  it("works with redirect chains after conflict resolution", {
    edges <- data.frame(from = "A", to = "B")
    # B -> C (first), B -> D (second, dropped); C -> Final
    redirects <- data.frame(
      from = c("B", "B", "C"), to = c("C", "D", "Final")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "first_wins"
    )
    expect_equal(resolved$to, "Final")
  })
})

describe("duplicate_from_policy = 'last_wins'", {
  it("keeps the last target for conflicting sources", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(
      from = c("B", "B", "B"), to = c("C", "D", "E")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "last_wins"
    )
    expect_equal(resolved$to, "E")
  })

  it("preserves non-conflicting redirects", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y")
    )
    redirects <- data.frame(
      from = c("B", "B", "Y"), to = c("C", "D", "Z")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "last_wins"
    )
    expect_equal(resolved$to, c("D", "Z"))
  })
})

describe("duplicate_from_policy = 'most_frequent'", {
  it("picks the most common target", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(
      from = c("B", "B", "B", "B"),
      to = c("C", "D", "D", "D")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "most_frequent"
    )
    expect_equal(resolved$to, "D")
  })

  it("breaks ties by first occurrence", {
    edges <- data.frame(from = "A", to = "B")
    # C appears first (2x), D appears second (2x) -- tie, C wins
    redirects <- data.frame(
      from = c("B", "B", "B", "B"),
      to = c("C", "C", "D", "D")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "most_frequent"
    )
    expect_equal(resolved$to, "C")
  })

  it("preserves non-conflicting redirects", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y")
    )
    redirects <- data.frame(
      from = c("B", "B", "B", "Y"),
      to = c("C", "D", "D", "Z")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "most_frequent"
    )
    expect_equal(resolved$to, c("D", "Z"))
  })
})

describe("duplicate_from_policy = 'prune_source'", {
  it("removes all redirects from conflicting sources", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(
      from = c("B", "B"), to = c("C", "D")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "prune_source"
    )
    # B has conflicting targets -> pruned -> B stays unresolved
    expect_equal(resolved$to, "B")
  })

  it("preserves non-conflicting redirects when conflicting ones are pruned", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y")
    )
    redirects <- data.frame(
      from = c("B", "B", "Y"), to = c("C", "D", "Z")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "prune_source"
    )
    # B is pruned (conflicting), Y -> Z is kept
    expect_equal(resolved$to, c("B", "Z"))
  })

  it("returns edge list unchanged when all sources conflict", {
    edges <- data.frame(
      from = c("A"), to = c("B")
    )
    redirects <- data.frame(
      from = c("B", "B", "A", "A"),
      to = c("C", "D", "E", "F")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "prune_source"
    )
    expect_equal(resolved$from, "A")
    expect_equal(resolved$to, "B")
  })
})

describe("duplicate_from_policy = 'resolve_if_consistent'", {
  it("allows exact duplicate redirects (all same target)", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(
      from = c("B", "B", "B"), to = c("C", "C", "C")
    )
    resolved <- resolve_redirects(
      edges, redirects,
      duplicate_from_policy = "resolve_if_consistent"
    )
    expect_equal(resolved$to, "C")
  })

  it("errors on true conflicts (different targets)", {
    edges <- data.frame(from = "A", to = "B")
    redirects <- data.frame(
      from = c("B", "B"), to = c("C", "D")
    )
    expect_error(
      resolve_redirects(edges, redirects,
        duplicate_from_policy = "resolve_if_consistent"
      ),
      "Ambiguous redirect.*B.*C, D"
    )
  })

  it("handles mix of consistent duplicates and unique redirects", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y")
    )
    redirects <- data.frame(
      from = c("B", "B", "Y"), to = c("C", "C", "Z")
    )
    resolved <- resolve_redirects(
      edges, redirects,
      duplicate_from_policy = "resolve_if_consistent"
    )
    expect_equal(resolved$to, c("C", "Z"))
  })
})

describe("duplicate_from_policy integration", {
  it("works with redirect chains after conflict resolution", {
    edges <- data.frame(from = "Start", to = "A")
    # A -> B (most frequent, 3x), A -> C (1x); B -> Final
    redirects <- data.frame(
      from = c("A", "A", "A", "A", "B"),
      to = c("B", "B", "B", "C", "Final")
    )
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "most_frequent"
    )
    # A -> B (most frequent) -> Final (chain)
    expect_equal(resolved$to, "Final")
  })

  it("works with self-ref filtering before conflict detection", {
    edges <- data.frame(from = "A", to = "B")
    # B -> B (self-ref, filtered), B -> C, B -> D
    redirects <- data.frame(
      from = c("B", "B", "B"), to = c("B", "C", "D")
    )
    # After self-ref removal: B -> C, B -> D (conflict)
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "first_wins"
    )
    expect_equal(resolved$to, "C")
  })

  it("passthrough via pagerank() works", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    redirects <- data.frame(
      from = c("A", "A"), to = c("X", "Y")
    )
    # Should not error with first_wins
    pr <- pagerank(edges,
      redirects_df = redirects,
      duplicate_from_policy = "first_wins",
      clean_edge_urls = FALSE, clean_redirect_urls = FALSE
    )
    expect_s3_class(pr, "data.frame")
    expect_gt(nrow(pr), 0)
  })
})


# =============================================================================
# Graph-based redirect resolution and loop_handling tests
# =============================================================================

describe("loop_handling = 'error' (default)", {
  it("errors on simple redirect cycle (A -> B -> C -> A)", {
    edges <- data.frame(from = "X", to = "A")
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    expect_error(
      resolve_redirects(edges, redirects),
      regexp = "Redirect cycle detected"
    )
  })

  it("errors on two-node cycle (A -> B -> A)", {
    edges <- data.frame(from = "X", to = "A")
    redirects <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    expect_error(
      resolve_redirects(edges, redirects),
      regexp = "Redirect cycle detected"
    )
  })

  it("error message includes readable cycle path", {
    edges <- data.frame(from = "X", to = "A")
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    expect_error(
      resolve_redirects(edges, redirects),
      regexp = "A -> B -> C -> A"
    )
  })
})


describe("loop_handling = 'prune_loop'", {
  it("removes cycle edges and leaves URLs unresolved", {
    edges <- data.frame(
      from = c("X", "Y"),
      to = c("A", "D")
    )
    # A -> B -> C -> A is a cycle; D -> E is not
    redirects <- data.frame(
      from = c("A", "B", "C", "D"),
      to = c("B", "C", "A", "E")
    )
    resolved <- resolve_redirects(
      edges, redirects,
      loop_handling = "prune_loop"
    )
    # A stays as A (cycle pruned), D becomes E
    expect_equal(resolved$to[resolved$from == "X"], "A")
    expect_equal(resolved$to[resolved$from == "Y"], "E")
  })

  it("handles cycle where all redirects are in loop", {
    edges <- data.frame(
      from = "X", to = "A"
    )
    redirects <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    resolved <- resolve_redirects(
      edges, redirects,
      loop_handling = "prune_loop"
    )
    # A -> B -> A is a cycle, both pruned. A stays as A.
    expect_equal(resolved$to, "A")
  })

  it("preserves chains that feed into a cycle node but aren't in the cycle", {
    edges <- data.frame(from = "Start", to = "P")
    # P -> Q (linear), Q -> R -> S -> Q (cycle)
    redirects <- data.frame(
      from = c("P", "Q", "R", "S"),
      to = c("Q", "R", "S", "Q")
    )
    resolved <- resolve_redirects(
      edges, redirects,
      loop_handling = "prune_loop"
    )
    # Q, R, S are in a cycle -> their edges are pruned
    # P -> Q still exists as a linear redirect, so P resolves to Q
    # Q is now terminal (its outgoing edge was pruned)
    expect_equal(resolved$to, "Q")
  })
})


describe("loop_handling = 'break_arrow'", {
  it("breaks cycle by keeping highest in-degree node as sink", {
    edges <- data.frame(
      from = c("X", "Y"),
      to = c("A", "D")
    )
    # A -> B -> C -> A is a cycle; D -> E is linear.
    # break_arrow picks the sink by GLOBAL in-degree (external inbound counts):
    # here the redirect graph has no external inbound to the cycle, so A/B/C are
    # all in-degree 1 and the first vertex (A) is picked. (The X/Y edges are in
    # the edge list, not the redirect graph, so they don't affect sink choice.)
    redirects <- data.frame(
      from = c("A", "B", "C", "D"),
      to = c("B", "C", "A", "E")
    )
    resolved <- resolve_redirects(
      edges, redirects,
      loop_handling = "break_arrow"
    )
    # D -> E should still work
    expect_equal(resolved$to[resolved$from == "Y"], "E")
    # A should resolve to something (not error)
    expect_false(is.na(resolved$to[resolved$from == "X"]))
  })

  it("break_arrow resolves chain through broken cycle", {
    edges <- data.frame(from = "X", to = "A")
    # A -> B -> C -> A cycle. If A is picked as sink (outgoing A->B removed),
    # then B -> C -> A, so A resolves to A (terminal)
    # If C is picked (outgoing C->A removed), then A -> B -> C (terminal)
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    resolved <- resolve_redirects(
      edges, redirects,
      loop_handling = "break_arrow"
    )
    # Should not error and should resolve to something
    expect_type(resolved$to, "character")
    expect_false(is.na(resolved$to))
  })

  it("break_arrow with asymmetric in-degree picks correct sink", {
    edges <- data.frame(from = "X", to = "A")
    # A -> B, C -> B, B -> A creates a cycle {A, B} with B having in-degree 2
    # So B should be kept as sink, B->A removed, A -> B resolves to B
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "A", "B")
    )
    resolved <- resolve_redirects(
      edges, redirects,
      loop_handling = "break_arrow"
    )
    # A -> B (B is sink, B->A removed). X -> A -> B
    expect_equal(resolved$to, "B")
  })
})


describe(".build_canonical_map cycle guard", {
  it("terminates on a residual cycle instead of hanging", {
    # Defensive guard: a cyclic graph should never reach .build_canonical_map
    # (loop_handling breaks cycles upstream), but if one ever did, the traversal
    # must break rather than spin forever. Feed it an unbroken 2-cycle directly.
    g <- igraph::graph_from_data_frame(
      data.frame(from = c("A", "B"), to = c("B", "A")),
      directed = TRUE
    )
    m <- .build_canonical_map(g)
    expect_length(m, 2)
    # No hang, no NA; both nodes resolve to a single terminal within the cycle.
    expect_false(any(is.na(m)))
    expect_true(all(m %in% c("A", "B")))
  })
})


describe("graph-based resolution: complex topologies", {
  it("resolves long chain (5 hops)", {
    edges <- data.frame(from = "Start", to = "A")
    redirects <- data.frame(
      from = c("A", "B", "C", "D", "E"),
      to = c("B", "C", "D", "E", "Final")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$to, "Final")
  })

  it("resolves diamond/convergent redirects", {
    # A -> C, B -> C (both redirect to same target)
    edges <- data.frame(
      from = c("X", "Y"), to = c("A", "B")
    )
    redirects <- data.frame(
      from = c("A", "B"), to = c("C", "C")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$to, c("C", "C"))
  })

  it("handles mixed: some URLs redirect, some don't", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("D", "E", "F")
    )
    redirects <- data.frame(
      from = c("D"), to = c("D_final")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$to, c("D_final", "E", "F"))
    expect_equal(resolved$from, c("A", "B", "C"))
  })

  it("resolves from-column URLs too", {
    edges <- data.frame(
      from = c("OldPage", "Other"),
      to = c("Target", "Target")
    )
    redirects <- data.frame(
      from = c("OldPage"), to = c("NewPage")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$from, c("NewPage", "Other"))
  })

  it("handles tree-shaped redirects (one source, branching via edges)", {
    edges <- data.frame(
      from = c("Hub", "Hub", "Hub"),
      to = c("A", "B", "C")
    )
    redirects <- data.frame(
      from = c("A", "B"),
      to = c("A_final", "B_final")
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$to, c("A_final", "B_final", "C"))
  })
})


describe("loop_handling passthrough via pagerank()", {
  it("pagerank() accepts loop_handling = 'prune_loop'", {
    edges <- data.frame(
      from = c("X", "Y"), to = c("Y", "X")
    )
    redirects <- data.frame(
      from = c("X", "Z"), to = c("Z", "X")
    )
    # With loop_handling = "error" this would fail
    pr <- pagerank(edges,
      redirects_df = redirects,
      loop_handling = "prune_loop",
      clean_edge_urls = FALSE, clean_redirect_urls = FALSE
    )
    expect_s3_class(pr, "data.frame")
    expect_gt(nrow(pr), 0)
  })

  it("pagerank() accepts loop_handling = 'break_arrow'", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    redirects <- data.frame(
      from = c("A", "C"), to = c("C", "A")
    )
    pr <- pagerank(edges,
      redirects_df = redirects,
      loop_handling = "break_arrow",
      clean_edge_urls = FALSE, clean_redirect_urls = FALSE
    )
    expect_s3_class(pr, "data.frame")
    expect_gt(nrow(pr), 0)
  })

  it("pagerank() default loop_handling = 'error' still errors on cycles", {
    edges <- data.frame(from = "X", to = "A")
    redirects <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    expect_error(
      pagerank(edges,
        redirects_df = redirects,
        clean_edge_urls = FALSE, clean_redirect_urls = FALSE
      ),
      regexp = "Redirect cycle detected"
    )
  })
})


describe("loop_handling with duplicate_from_policy interaction", {
  it("prune_loop works with first_wins policy", {
    edges <- data.frame(from = "X", to = "A")
    redirects <- data.frame(
      from = c("A", "A", "B"),
      to = c("B", "C", "A")
    )
    # first_wins: A -> B (first), then B -> A creates cycle
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "first_wins",
      loop_handling = "prune_loop"
    )
    expect_s3_class(resolved, "data.frame")
    # A -> B -> A cycle pruned, so A stays as A
    expect_equal(resolved$to, "A")
  })

  it("break_arrow works with most_frequent policy", {
    edges <- data.frame(from = "X", to = "A")
    redirects <- data.frame(
      from = c("A", "A", "A", "B"),
      to = c("B", "B", "C", "A")
    )
    # most_frequent: A -> B (appears 2x vs C's 1x), then B -> A creates cycle
    resolved <- resolve_redirects(edges, redirects,
      duplicate_from_policy = "most_frequent",
      loop_handling = "break_arrow"
    )
    expect_s3_class(resolved, "data.frame")
    expect_false(is.na(resolved$to))
  })
})
