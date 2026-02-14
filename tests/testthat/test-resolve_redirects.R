context("resolve_redirects")

# source("../../R/utils.R") # .trace_redirect_path
# source("../../R/resolve_redirects.R")

describe("resolve_redirects basic functionality", {
  it("resolves direct redirects", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(from = "B", to = "C", stringsAsFactors = FALSE)
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$from, "A")
    expect_equal(resolved$to, "C")
  })

  it("resolves transitive (chained) redirects", {
    edges <- data.frame(from = "X", to = "Y", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("Y", "Z", "AA"), 
      to = c("Z", "FINAL_Z", "FINAL_AA"), 
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$from, "X")
    expect_equal(resolved$to, "FINAL_Z")
    
    edges2 <- data.frame(from = "AA", to = "Y", stringsAsFactors = FALSE)
    resolved2 <- resolve_redirects(edges2, redirects)
    expect_equal(resolved2$from, "FINAL_AA")
    expect_equal(resolved2$to, "FINAL_Z")
  })

  it("handles URLs not in the redirect map", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(from = "C", to = "D", stringsAsFactors = FALSE) # B is not in redirects
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$from, "A") # A is not in redirects
    expect_equal(resolved$to, "B")   # B is not resolved
  })
  
  it("handles empty edge list", {
    edges_empty <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    redirects <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    resolved <- resolve_redirects(edges_empty, redirects)
    expect_equal(nrow(resolved), 0)
    expect_equal(names(resolved), c("from", "to"))
  })
  
  it("handles empty redirect list", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects_empty <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    resolved <- resolve_redirects(edges, redirects_empty)
    expect_equal(resolved$from, "A")
    expect_equal(resolved$to, "B")
    expect_equal(resolved, edges)
  })
  
  it("handles NA values in edge list gracefully", {
    edges_na <- data.frame(from = c("A", NA, "C"), to = c(NA, "B", "D"), stringsAsFactors = FALSE)
    redirects <- data.frame(from = c("A", "D"), to = c("FINAL_A", "FINAL_D"), stringsAsFactors = FALSE)
    resolved <- resolve_redirects(edges_na, redirects)
    expect_equal(resolved$from, c("FINAL_A", NA, "C"))
    expect_equal(resolved$to, c(NA, "B", "FINAL_D"))
  })
  
  it("handles NA values in redirect list (rules with NA are ignored)", {
    edges <- data.frame(from = c("A", "B"), to = c("X", "Y"), stringsAsFactors = FALSE)
    redirects_na <- data.frame(
        from = c("X", NA, "Y", "Z"), 
        to = c("FINAL_X", "W", NA, "FINAL_Z"), 
        stringsAsFactors = FALSE
    )
    # Expected: X -> FINAL_X. Rule with NA from is ignored. Rule Y -> NA is ignored. Z is not in edges.
    resolved <- resolve_redirects(edges, redirects_na)
    expect_equal(resolved$to, c("FINAL_X", "Y")) # Y remains Y as its redirect to NA is invalid
  })
  
  it("works with custom column names", {
    edges_custom <- data.frame(source = "Page1", destination = "Page2", stringsAsFactors = FALSE)
    redirects_custom <- data.frame(orig = "Page2", new = "Page2_Final", stringsAsFactors = FALSE)
    resolved <- resolve_redirects(edges_custom, redirects_custom, 
                                  edge_from_col = "source", edge_to_col = "destination",
                                  redirect_from_col = "orig", redirect_to_col = "new")
    expect_equal(resolved$source, "Page1")
    expect_equal(resolved$destination, "Page2_Final")
  })
})

describe("resolve_redirects error handling", {
  it("detects and errors on redirect cycles", {
    edges <- data.frame(from = "Start", to = "L1", stringsAsFactors = FALSE)
    redirects_cycle <- data.frame(
      from = c("L1", "L2", "L3"), 
      to = c("L2", "L3", "L1"), # L1 -> L2 -> L3 -> L1
      stringsAsFactors = FALSE
    )
    expect_error(resolve_redirects(edges, redirects_cycle),
                 regexp = "Redirect cycle detected for URL 'L1'. Path: L1 -> L2 -> L3 -> L1")

    # Self-referencing redirects (from == to) are silently filtered, not treated as cycles.
    # Moved to its own test below. Multi-hop cycles (L1->L2->L3->L1) still error.
  })

  it("detects and errors on redirect ambiguities", {
    edges <- data.frame(from = "Source", to = "A_link", stringsAsFactors = FALSE)
    redirects_ambiguous <- data.frame(
      from = c("A_link", "A_link", "Other"), 
      to = c("Target1", "Target2", "TargetOther"), 
      stringsAsFactors = FALSE
    )
    # A_link maps to Target1 and Target2
    expect_error(resolve_redirects(edges, redirects_ambiguous),
                 regexp = "Ambiguous redirect: URL 'A_link' maps to multiple distinct targets: Target1, Target2")
                 
    # Check with one distinct target (not an error)
    redirects_not_ambiguous <- data.frame(
        from = c("A_link", "A_link"),
        to = c("Target1", "Target1"),
        stringsAsFactors = FALSE
    )
    resolved_ok <- resolve_redirects(edges, redirects_not_ambiguous)
    expect_equal(resolved_ok$to, "Target1")
  })
  
  it("errors on invalid input types", {
      expect_error(resolve_redirects(list(), data.frame(f="a",t="b")))
      expect_error(resolve_redirects(data.frame(f="a",t="b"), list()))
  })
  
  it("errors if columns are missing and df is not empty", {
      df <- data.frame(fcol = "a", tcol = "b", stringsAsFactors = FALSE)
      rdf <- data.frame(from_col = "b", to_col = "c", stringsAsFactors = FALSE)
      expect_error(resolve_redirects(df, rdf)) # Default cols "from", "to" missing
      expect_error(resolve_redirects(df, rdf, edge_from_col = "x"))
      expect_error(resolve_redirects(df, rdf, redirect_from_col = "y"))
  })
})

describe("resolve_redirects with more complex scenarios", {
    it("handles a mix of direct, chained, and no redirects", {
        edges = data.frame(
            source = c("A", "B", "C", "D", "E"),
            target = c("X", "Y", "Z", "P", "Q"),
            stringsAsFactors = FALSE
        )
        redirects = data.frame(
            orig = c("X", "Y", "Z1", "P", "R"),
            final = c("X_final", "Y_intermediate", "Z_final", "P_final", "R_final"),
            stringsAsFactors = FALSE
        )
        redirects_chain = data.frame(orig = "Y_intermediate", final = "Y_final", stringsAsFactors = FALSE)
        all_redirects = rbind(redirects, redirects_chain)
        
        resolved <- resolve_redirects(edges, all_redirects, 
                                      edge_from_col = "source", edge_to_col = "target",
                                      redirect_from_col = "orig", redirect_to_col = "final")
        
        expected_targets <- c("X_final", "Y_final", "Z", "P_final", "Q")
        # A -> X -> X_final
        # B -> Y -> Y_intermediate -> Y_final
        # C -> Z (no redirect for Z or Z1 in edges, but Z1->Z_final exists in redirects if C was Z1)
        # D -> P -> P_final
        # E -> Q (no redirect for Q)
        expect_equal(resolved$target, expected_targets)
        expect_equal(resolved$source, edges$source) # sources A,B,C,D,E are not in redirect from column
    })
    
    it("multiple edges resolve correctly", {
        edges <- data.frame(
            from = c("N1", "N1", "N2", "N3"),
            to =   c("R1", "R2", "R1", "R3"),
            stringsAsFactors = FALSE
        )
        redirects <- data.frame(
            from = c("R1", "R2", "R3"),
            to =   c("FINAL_R1", "FINAL_R2", "FINAL_R3"),
            stringsAsFactors = FALSE
        )
        resolved <- resolve_redirects(edges, redirects)
        expect_equal(resolved$from, c("N1", "N1", "N2", "N3"))
        expect_equal(resolved$to, c("FINAL_R1", "FINAL_R2", "FINAL_R1", "FINAL_R3"))
    })

    it("resolves redirects when URLs appear in both from and to columns of edges", {
        edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
        redirects <- data.frame(from = c("A", "B"), to = c("A_final", "B_final"), stringsAsFactors = FALSE)
        resolved <- resolve_redirects(edges, redirects)
        expect_equal(resolved$from, "A_final")
        expect_equal(resolved$to, "B_final")
    })
})

describe("resolve_redirects self-referencing redirect handling", {
    it("silently filters self-referencing redirects (from == to)", {
        edges <- data.frame(from = "Initial", to = "S", stringsAsFactors = FALSE)
        redirects_self <- data.frame(from = "S", to = "S", stringsAsFactors = FALSE)
        # Self-referencing redirect S -> S should be silently filtered, not an error.
        # "S" remains "S" since no valid redirect applies.
        resolved <- resolve_redirects(edges, redirects_self)
        expect_equal(resolved$from, "Initial")
        expect_equal(resolved$to, "S")
    })

    it("filters self-referencing redirects while keeping valid ones", {
        edges <- data.frame(from = c("A", "B"), to = c("X", "Y"), stringsAsFactors = FALSE)
        redirects <- data.frame(
            from = c("X", "Y", "Z"),
            to =   c("X", "Y_final", "Z_final"),
            stringsAsFactors = FALSE
        )
        # X -> X is self-referencing, silently filtered. Y -> Y_final is valid.
        resolved <- resolve_redirects(edges, redirects)
        expect_equal(resolved$to, c("X", "Y_final"))
    })

    it("returns edge list unchanged when all redirects are self-referencing", {
        edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
        redirects <- data.frame(
            from = c("X", "Y"),
            to =   c("X", "Y"),
            stringsAsFactors = FALSE
        )
        resolved <- resolve_redirects(edges, redirects)
        expect_equal(resolved, edges)
    })

    it("errors when redirects_df is not a data frame", {
        edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
        expect_error(resolve_redirects(edges, "not_a_df"), "data frame")
    })

    it("errors when redirects_df is missing required columns", {
        edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
        redirects <- data.frame(x = "A", y = "B", stringsAsFactors = FALSE)
        expect_error(resolve_redirects(edges, redirects), "columns")
    })
})

# ===========================================================================
# duplicate_from_policy tests
# ===========================================================================

describe("duplicate_from_policy = 'strict' (default)", {
  it("errors on conflicting redirects", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("B", "B"), to = c("C", "D"), stringsAsFactors = FALSE
    )
    expect_error(resolve_redirects(edges, redirects),
                 "Ambiguous redirect.*B.*C, D")
  })

  it("allows exact duplicate redirects (same from and to)", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("B", "B"), to = c("C", "C"), stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects)
    expect_equal(resolved$to, "C")
  })

  it("passes through clean redirects unchanged", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(from = "B", to = "C", stringsAsFactors = FALSE)
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "strict")
    expect_equal(resolved$to, "C")
  })
})

describe("duplicate_from_policy = 'first_wins'", {
  it("keeps the first target for conflicting sources", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("B", "B", "B"), to = c("C", "D", "E"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "first_wins")
    expect_equal(resolved$to, "C")
  })

  it("preserves non-conflicting redirects alongside conflicting ones", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("B", "B", "Y"), to = c("C", "D", "Z"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "first_wins")
    expect_equal(resolved$to, c("C", "Z"))
  })

  it("works with redirect chains after conflict resolution", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    # B -> C (first), B -> D (second, dropped); C -> Final
    redirects <- data.frame(
      from = c("B", "B", "C"), to = c("C", "D", "Final"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "first_wins")
    expect_equal(resolved$to, "Final")
  })
})

describe("duplicate_from_policy = 'last_wins'", {
  it("keeps the last target for conflicting sources", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("B", "B", "B"), to = c("C", "D", "E"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "last_wins")
    expect_equal(resolved$to, "E")
  })

  it("preserves non-conflicting redirects", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("B", "B", "Y"), to = c("C", "D", "Z"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "last_wins")
    expect_equal(resolved$to, c("D", "Z"))
  })
})

describe("duplicate_from_policy = 'most_frequent'", {
  it("picks the most common target", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("B", "B", "B", "B"),
      to = c("C", "D", "D", "D"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "most_frequent")
    expect_equal(resolved$to, "D")
  })

  it("breaks ties by first occurrence", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    # C appears first (2x), D appears second (2x) -- tie, C wins
    redirects <- data.frame(
      from = c("B", "B", "B", "B"),
      to = c("C", "C", "D", "D"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "most_frequent")
    expect_equal(resolved$to, "C")
  })

  it("preserves non-conflicting redirects", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("B", "B", "B", "Y"),
      to = c("C", "D", "D", "Z"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "most_frequent")
    expect_equal(resolved$to, c("D", "Z"))
  })
})

describe("duplicate_from_policy = 'prune_source'", {
  it("removes all redirects from conflicting sources", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("B", "B"), to = c("C", "D"), stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "prune_source")
    # B has conflicting targets -> pruned -> B stays unresolved
    expect_equal(resolved$to, "B")
  })

  it("preserves non-conflicting redirects when conflicting ones are pruned", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("B", "B", "Y"), to = c("C", "D", "Z"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "prune_source")
    # B is pruned (conflicting), Y -> Z is kept
    expect_equal(resolved$to, c("B", "Z"))
  })

  it("returns edge list unchanged when all sources conflict", {
    edges <- data.frame(
      from = c("A"), to = c("B"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("B", "B", "A", "A"),
      to = c("C", "D", "E", "F"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "prune_source")
    expect_equal(resolved$from, "A")
    expect_equal(resolved$to, "B")
  })
})

describe("duplicate_from_policy = 'resolve_if_consistent'", {
  it("allows exact duplicate redirects (all same target)", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("B", "B", "B"), to = c("C", "C", "C"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(
      edges, redirects,
      duplicate_from_policy = "resolve_if_consistent"
    )
    expect_equal(resolved$to, "C")
  })

  it("errors on true conflicts (different targets)", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    redirects <- data.frame(
      from = c("B", "B"), to = c("C", "D"), stringsAsFactors = FALSE
    )
    expect_error(
      resolve_redirects(edges, redirects,
                        duplicate_from_policy = "resolve_if_consistent"),
      "Ambiguous redirect.*B.*C, D"
    )
  })

  it("handles mix of consistent duplicates and unique redirects", {
    edges <- data.frame(
      from = c("A", "X"), to = c("B", "Y"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("B", "B", "Y"), to = c("C", "C", "Z"),
      stringsAsFactors = FALSE
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
    edges <- data.frame(from = "Start", to = "A", stringsAsFactors = FALSE)
    # A -> B (most frequent, 3x), A -> C (1x); B -> Final
    redirects <- data.frame(
      from = c("A", "A", "A", "A", "B"),
      to = c("B", "B", "B", "C", "Final"),
      stringsAsFactors = FALSE
    )
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "most_frequent")
    # A -> B (most frequent) -> Final (chain)
    expect_equal(resolved$to, "Final")
  })

  it("works with self-ref filtering before conflict detection", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    # B -> B (self-ref, filtered), B -> C, B -> D
    redirects <- data.frame(
      from = c("B", "B", "B"), to = c("B", "C", "D"),
      stringsAsFactors = FALSE
    )
    # After self-ref removal: B -> C, B -> D (conflict)
    resolved <- resolve_redirects(edges, redirects,
                                  duplicate_from_policy = "first_wins")
    expect_equal(resolved$to, "C")
  })

  it("passthrough via pagerank() works", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A"), stringsAsFactors = FALSE
    )
    redirects <- data.frame(
      from = c("A", "A"), to = c("X", "Y"), stringsAsFactors = FALSE
    )
    # Should not error with first_wins
    pr <- pagerank(edges, redirects_df = redirects,
                   duplicate_from_policy = "first_wins",
                   clean_edge_urls = FALSE, clean_redirect_urls = FALSE)
    expect_true(is.data.frame(pr))
    expect_true(nrow(pr) > 0)
  })
}) 