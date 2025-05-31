context("get_unique_edges")

# source("../../R/get_unique_edges.R")

describe("get_unique_edges basic functionality", {
  it("removes exact duplicate edges", {
    edges <- data.frame(
      from = c("A", "B", "A", "C"), 
      to = c("B", "C", "B", "C"), 
      stringsAsFactors = FALSE
    )
    unique_e <- get_unique_edges(edges)
    expect_equal(nrow(unique_e), 3)
    expect_true(all(dim(unique_e) == c(3,2)))
    expect_equal(unique_e[order(unique_e$from, unique_e$to),], 
                 data.frame(from=c("A","B","C"), to=c("B","C","C"), stringsAsFactors = FALSE)[order(c("A","B","C"), c("B","C","C")),])
  })

  it("handles self-loops: drop (default)", {
    edges_sl <- data.frame(
      from = c("A", "B", "B", "C"), 
      to = c("B", "B", "C", "D"), 
      stringsAsFactors = FALSE
    )
    # B->B is a self-loop
    unique_dropped <- get_unique_edges(edges_sl, self_loops = "drop")
    expect_equal(nrow(unique_dropped), 2)
    expect_false(any(unique_dropped$from == "B" & unique_dropped$to == "B"))
    expect_equal(unique_dropped[order(unique_dropped$from, unique_dropped$to),],
                 data.frame(from=c("A","B"), to=c("B","C"), stringsAsFactors = FALSE)[order(c("A","B"),c("B","C")),])
  })

  it("handles self-loops: keep", {
    edges_sl <- data.frame(
      from = c("A", "B", "B", "C", "D"), 
      to = c("B", "B", "C", "D", "D"), 
      stringsAsFactors = FALSE
    )
    # B->B and D->D are self-loops
    unique_kept <- get_unique_edges(edges_sl, self_loops = "keep")
    expect_equal(nrow(unique_kept), 4) # A->B, B->B, B->C, D->D (C->D is missing in original) -> No, C->D not present. A->B, B->B, B->C, D->D
    # A->B, B->B, B->C, C->D, D->D. Duplicates are removed first.
    # Original has: A->B, B->B, B->C, C->D, D->D. All are unique. B->B and D->D are self-loops.
    # Expected: A->B, B->B, B->C, C->D, D->D if C->D was there. It's not.
    # The edges are A->B, B->B (self), B->C, C->D (NOT in original data), D->D (self)
    # So, actually edges are: A->B, B->B, B->C, D->D (from original data)
    # After unique (no change): A->B, B->B, B->C, D->D
    expect_true(any(unique_kept$from == "B" & unique_kept$to == "B"))
    expect_true(any(unique_kept$from == "D" & unique_kept$to == "D"))
    expect_equal(unique_kept[order(unique_kept$from, unique_kept$to),],
                 data.frame(from=c("A","B","B","D"), to=c("B","B","C","D"), stringsAsFactors=FALSE)[order(c("A","B","B","D"),c("B","B","C","D")),])
  })
  
  it("handles cases where all edges are self-loops and dropped", {
      edges <- data.frame(from=c("A","B"), to=c("A","B"), stringsAsFactors=FALSE)
      unique_dropped <- get_unique_edges(edges, self_loops="drop")
      expect_equal(nrow(unique_dropped), 0)
      expect_equal(names(unique_dropped), c("from", "to"))
  })
  
  it("works with custom column names", {
    edges_custom <- data.frame(
      source_node = c("X", "Y", "X", "Z"), 
      target_node = c("Y", "Y", "Y", "Z"), 
      stringsAsFactors = FALSE
    )
    # Z->Z is self-loop. X->Y is duplicated.
    unique_c <- get_unique_edges(edges_custom, from_col = "source_node", to_col = "target_node", self_loops = "drop")
    expect_equal(nrow(unique_c), 2)
    expect_equal(names(unique_c), c("source_node", "target_node"))
    expect_false(any(unique_c$source_node == "Z" & unique_c$target_node == "Z"))
    # Expected: X->Y, Y->Y
    expect_equal(unique_c[order(unique_c$source_node, unique_c$target_node),],
                 data.frame(source_node=c("X","Y"), target_node=c("Y","Y"), stringsAsFactors=FALSE)[order(c("X","Y"),c("Y","Y")),])

    unique_c_kept <- get_unique_edges(edges_custom, from_col = "source_node", to_col = "target_node", self_loops = "keep")    
    expect_equal(nrow(unique_c_kept), 3) # X->Y, Y->Y, Z->Z
    expect_true(any(unique_c_kept$source_node == "Z" & unique_c_kept$target_node == "Z"))
  })
  
  it("handles NA values correctly (NAs make edges distinct unless all parts are NA and identical)", {
    edges_na <- data.frame(
      from = c("A", NA, "A", "B", NA, "X"), 
      to =   c("B", "C", "B", "D", "C", NA),
      stringsAsFactors = FALSE
    )
    # Duplicates: A->B (appears twice). NA->C (appears twice).
    # Self-loops: None that are non-NA. NA == NA is NA, so (NA,NA) would be self-loop if not for NA handling in `==`.
    # The implementation checks `!is.na(from) & !is.na(to) & (from == to)`
    unique_na_drop <- get_unique_edges(edges_na, self_loops = "drop")
    # Expected: A->B, NA->C, B->D, X->NA. (4 unique rows)
    expect_equal(nrow(unique_na_drop), 4)

    # Verify specific unique rows (order might vary, so check content)
    # Convert to a canonical representation for comparison (e.g., paste from and to)
    expected_pairs_drop <- sort(c("A_B", "NA_C", "B_D", "X_NA"))
    actual_pairs_drop <- sort(paste(unique_na_drop$from, unique_na_drop$to, sep="_"))
    expect_equal(actual_pairs_drop, expected_pairs_drop)
    
    unique_na_keep <- get_unique_edges(edges_na, self_loops = "keep")
    # Same as drop in this case as there are no non-NA self-loops
    expect_equal(nrow(unique_na_keep), 4)
    actual_pairs_keep <- sort(paste(unique_na_keep$from, unique_na_keep$to, sep="_"))
    expect_equal(actual_pairs_keep, expected_pairs_drop)
    
    # Test with actual NA self-loop potential if logic was different, but current logic excludes it.
    edges_na_sl <- data.frame(from=c(NA, "A"), to=c(NA, "A"), stringsAsFactors = FALSE)
    res_sl_na_drop <- get_unique_edges(edges_na_sl, self_loops="drop") # A->A drops, NA->NA not considered SL
    expect_equal(nrow(res_sl_na_drop), 1)
    expect_equal(res_sl_na_drop$from[1], NA_character_)
    expect_equal(res_sl_na_drop$to[1], NA_character_)
    
    res_sl_na_keep <- get_unique_edges(edges_na_sl, self_loops="keep") # A->A kept, NA->NA kept
    expect_equal(nrow(res_sl_na_keep), 2)
  })
  
  it("handles empty data frame", {
    df_empty <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    res_empty <- get_unique_edges(df_empty)
    expect_equal(nrow(res_empty), 0)
    expect_equal(names(res_empty), c("from", "to"))
    
    df_empty_custom <- data.frame(src = character(0), tgt = character(0), stringsAsFactors = FALSE)
    res_empty_c <- get_unique_edges(df_empty_custom, from_col="src", to_col="tgt")
    expect_equal(nrow(res_empty_c), 0)
    expect_equal(names(res_empty_c), c("src", "tgt"))
  })
  
  it("handles data frame that becomes empty after self-loop removal", {
    edges <- data.frame(from = c("A", "B"), to = c("A", "B"), stringsAsFactors = FALSE)
    unique_e <- get_unique_edges(edges, self_loops = "drop")
    expect_equal(nrow(unique_e), 0)
    expect_equal(names(unique_e), c("from", "to"))
    expect_true(is.character(unique_e$from) && is.character(unique_e$to)) # Check types of empty cols
  })
  
  it("input columns as factors", {
      edges_factor <- data.frame(
          from = factor(c("A", "B", "A")), 
          to = factor(c("B", "C", "B"))
      )
      unique_e <- get_unique_edges(edges_factor)
      expect_equal(nrow(unique_e), 2)
      expect_true(is.character(unique_e$from))
      expect_true(is.character(unique_e$to))
      # Expected: A->B, B->C
      expect_equal(unique_e[order(unique_e$from, unique_e$to),],
                   data.frame(from=c("A","B"), to=c("B","C"), stringsAsFactors=FALSE)[order(c("A","B"),c("B","C")),])
  })
  
  it("errors on invalid input types or missing columns", {
      expect_error(get_unique_edges(list()))
      df <- data.frame(fcol="a", tcol="b")
      expect_error(get_unique_edges(df)) # Default cols from/to missing
      expect_error(get_unique_edges(df, from_col = "non_existent"))
  })
}) 