context("get_unique_edges")

describe("get_unique_edges basic functionality", {
  it("removes exact duplicate edges", {
    edges <- data.frame(
      from = c("A", "B", "A", "C"),
      to = c("B", "C", "B", "C")
    )
    unique_e <- get_unique_edges(edges)
    expect_equal(nrow(unique_e), 2)
    expect_true(all(dim(unique_e) == c(2, 2)))
    expect_equal(
      unique_e[order(unique_e$from, unique_e$to), ],
      data.frame(
        from = c("A", "B"),
        to = c("B", "C")
      )[order(c("A", "B"), c("B", "C")), ]
    )
  })

  it("handles self-loops: drop (default)", {
    edges_sl <- data.frame(
      from = c("A", "B", "B", "C"),
      to = c("B", "B", "C", "D")
    )
    # B->B is a self-loop
    unique_dropped <- get_unique_edges(edges_sl, self_loops = "drop")
    expect_equal(nrow(unique_dropped), 3)
    expect_false(any(unique_dropped$from == "B" & unique_dropped$to == "B"))
    expect_equal(
      unique_dropped[order(unique_dropped$from, unique_dropped$to), ],
      data.frame(
        from = c("A", "B", "C"),
        to = c("B", "C", "D")
      )[order(c("A", "B", "C"), c("B", "C", "D")), ]
    )
  })

  it("handles self-loops: keep", {
    edges_sl <- data.frame(
      from = c("A", "B", "B", "C", "D"),
      to = c("B", "B", "C", "D", "D")
    )
    # B->B and D->D are self-loops
    unique_kept <- get_unique_edges(edges_sl, self_loops = "keep")
    expect_equal(nrow(unique_kept), 5) # A->B, B->B, B->C, C->D, D->D
    expect_true(any(unique_kept$from == "B" & unique_kept$to == "B"))
    expect_true(any(unique_kept$from == "D" & unique_kept$to == "D"))
    expect_equal(
      unique_kept[order(unique_kept$from, unique_kept$to), ],
      data.frame(
        from = c("A", "B", "B", "C", "D"),
        to = c("B", "B", "C", "D", "D")
      )[order(c("A", "B", "B", "C", "D"), c("B", "B", "C", "D", "D")), ]
    )
  })

  it("handles cases where all edges are self-loops and dropped", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("A", "B")
    )
    unique_dropped <- get_unique_edges(edges, self_loops = "drop")
    expect_equal(nrow(unique_dropped), 0)
    expect_named(unique_dropped, c("from", "to"))
  })

  it("works with custom column names", {
    edges_custom <- data.frame(
      source_node = c("X", "Y", "X", "Z"),
      target_node = c("Y", "Y", "Y", "Z")
    )
    # Only the X to Y edge is unique; the Z to Z edge is a self-loop
    unique_c <- get_unique_edges(
      edges_custom,
      from_col = "source_node",
      to_col = "target_node",
      self_loops = "drop"
    )
    expect_equal(nrow(unique_c), 1)
    expect_named(unique_c, c("source_node", "target_node"))
    expect_false(
      any(unique_c$source_node == "Z" & unique_c$target_node == "Z")
    )
    # Expected result is the X to Y edge
    expect_equal(
      unique_c[order(unique_c$source_node, unique_c$target_node), ],
      data.frame(
        source_node = c("X"),
        target_node = c("Y")
      )[order(c("X"), c("Y")), ]
    )

    unique_c_kept <- get_unique_edges(
      edges_custom,
      from_col = "source_node",
      to_col = "target_node",
      self_loops = "keep"
    )
    expect_equal(nrow(unique_c_kept), 3) # X->Y, Y->Y, Z->Z
    expect_true(
      any(unique_c_kept$source_node == "Z" & unique_c_kept$target_node == "Z")
    )
    expect_true(
      any(unique_c_kept$source_node == "Y" & unique_c_kept$target_node == "Y")
    )
    expect_equal(
      unique_c_kept[
        order(unique_c_kept$source_node, unique_c_kept$target_node),
      ],
      data.frame(
        source_node = c("X", "Y", "Z"),
        target_node = c("Y", "Y", "Z")
      )[order(c("X", "Y", "Z"), c("Y", "Y", "Z")), ]
    )
  })

  it("handles NA values correctly (NAs are dropped)", {
    edges_na <- data.frame(
      from = c("A", NA, "A", "B", NA, "X"),
      to = c("B", "C", "B", "D", "C", NA)
    )
    # After dropping NA-containing edges, only A->B, B->D remain
    unique_na_drop <- get_unique_edges(edges_na, self_loops = "drop")
    expect_equal(nrow(unique_na_drop), 2)
    expect_equal(unique_na_drop$from, c("A", "B"))
    expect_equal(unique_na_drop$to, c("B", "D"))

    unique_na_keep <- get_unique_edges(edges_na, self_loops = "keep")
    expect_equal(nrow(unique_na_keep), 2)
    expect_equal(unique_na_keep$from, c("A", "B"))
    expect_equal(unique_na_keep$to, c("B", "D"))
  })

  it("handles empty data frame", {
    df_empty <- data.frame(
      from = character(0),
      to = character(0)
    )
    res_empty <- get_unique_edges(df_empty)
    expect_equal(nrow(res_empty), 0)
    expect_named(res_empty, c("from", "to"))

    df_empty_custom <- data.frame(
      src = character(0),
      tgt = character(0)
    )
    res_empty_c <- get_unique_edges(
      df_empty_custom,
      from_col = "src",
      to_col = "tgt"
    )
    expect_equal(nrow(res_empty_c), 0)
    expect_named(res_empty_c, c("src", "tgt"))
  })

  it("handles data frame that becomes empty after self-loop removal", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("A", "B")
    )
    unique_e <- get_unique_edges(edges, self_loops = "drop")
    expect_equal(nrow(unique_e), 0)
    expect_named(unique_e, c("from", "to"))
    # Check types of empty columns
    expect_type(unique_e$from, "character")
    expect_type(unique_e$to, "character")
  })

  it("input columns as factors", {
    edges_factor <- data.frame(
      from = factor(c("A", "B", "A")),
      to = factor(c("B", "C", "B"))
    )
    unique_e <- get_unique_edges(edges_factor)
    expect_equal(nrow(unique_e), 2)
    expect_type(unique_e$from, "character")
    expect_type(unique_e$to, "character")
    # Expected edges are A to B and B to C
    expect_equal(
      unique_e[order(unique_e$from, unique_e$to), ],
      data.frame(
        from = c("A", "B"),
        to = c("B", "C")
      )[order(c("A", "B"), c("B", "C")), ]
    )
  })

  it("errors on invalid input types or missing columns", {
    expect_error(get_unique_edges(list()))
    df <- data.frame(fcol = "a", tcol = "b")
    expect_error(get_unique_edges(df)) # Default cols from/to missing
    expect_error(get_unique_edges(df, from_col = "non_existent"))
  })

  it("handles data frame with columns having all NAs (all dropped)", {
    df_all_na <- data.frame(
      from = c(NA_character_, NA_character_),
      to = c(NA_character_, NA_character_)
    )
    unique_all_na <- get_unique_edges(df_all_na)
    expect_equal(nrow(unique_all_na), 0)
    expect_named(unique_all_na, c("from", "to"))
  })
})

describe("get_unique_edges preserves extra columns", {
  it("preserves extra columns during deduplication", {
    edges <- data.frame(
      from = c("A", "B", "A", "C"),
      to = c("B", "C", "B", "D"),
      weight = c(1, 2, 3, 4),
      nofollow = c(FALSE, TRUE, FALSE, FALSE)
    )
    result <- get_unique_edges(edges, self_loops = "drop")
    expect_equal(nrow(result), 3) # A->B, B->C, C->D
    expect_true("weight" %in% names(result))
    expect_true("nofollow" %in% names(result))
    # First occurrence of A->B (weight=1) should be kept
    ab_row <- result[result$from == "A" & result$to == "B", ]
    expect_equal(ab_row$weight, 1)
    expect_false(ab_row$nofollow)
  })

  it("preserves extra columns during self-loop removal", {
    edges <- data.frame(
      from = c("A", "B", "B"),
      to = c("B", "B", "C"),
      score = c(10, 20, 30)
    )
    result <- get_unique_edges(edges, self_loops = "drop")
    expect_equal(nrow(result), 2) # A->B, B->C (B->B removed)
    expect_true("score" %in% names(result))
    expect_equal(result$score[result$from == "A"], 10)
    expect_equal(result$score[result$from == "B"], 30)
  })

  it("preserves extra column structure when all rows are dropped", {
    edges <- data.frame(
      from = c("A", "B"),
      to = c("A", "B"),
      weight = c(1, 2)
    )
    result <- get_unique_edges(edges, self_loops = "drop")
    expect_equal(nrow(result), 0)
    expect_true("weight" %in% names(result))
    expect_named(result, c("from", "to", "weight"))
  })

  it("deduplicates by from/to only, not by extra columns", {
    edges <- data.frame(
      from = c("A", "A"),
      to = c("B", "B"),
      weight = c(1, 99)
    )
    result <- get_unique_edges(edges, self_loops = "keep")
    expect_equal(nrow(result), 1)
    # First occurrence kept
    expect_equal(result$weight, 1)
  })

  it("handles truly empty data.frame() with non-default col names", {
    # 0 rows, 0 cols, custom from/to names
    empty_df <- data.frame()
    result <- get_unique_edges(empty_df, from_col = "src", to_col = "dst")
    expect_equal(nrow(result), 0)
    expect_true("src" %in% names(result))
    expect_true("dst" %in% names(result))
  })

  it("handles empty df with wrong columns (0 rows but some cols)", {
    # Has columns but not the right ones, and is empty
    df <- data.frame(
      x = character(0),
      y = character(0)
    )
    result <- get_unique_edges(df, from_col = "x", to_col = "y")
    expect_equal(nrow(result), 0)
  })
})
