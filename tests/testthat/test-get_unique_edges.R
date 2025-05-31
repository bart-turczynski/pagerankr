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
    expect_equal(nrow(unique_e), 2)
    expect_true(all(dim(unique_e) == c(2,2)))
    expect_equal(unique_e[order(unique_e$from, unique_e$to),], 
                 data.frame(from=c("A","B"), to=c("B","C"), stringsAsFactors = FALSE)[order(c("A","B"), c("B","C")),])
  })

  it("handles self-loops: drop (default)", {
    edges_sl <- data.frame(
      from = c("A", "B", "B", "C"), 
      to = c("B", "B", "C", "D"), 
      stringsAsFactors = FALSE
    )
    # B->B is a self-loop
    unique_dropped <- get_unique_edges(edges_sl, self_loops = "drop")
    expect_equal(nrow(unique_dropped), 3)
    expect_false(any(unique_dropped$from == "B" & unique_dropped$to == "B"))
    expect_equal(unique_dropped[order(unique_dropped$from, unique_dropped$to),],
                 data.frame(from=c("A","B","C"), to=c("B","C","D"), stringsAsFactors = FALSE)[order(c("A","B","C"),c("B","C","D")),])
  })

  it("handles self-loops: keep", {
    edges_sl <- data.frame(
      from = c("A", "B", "B", "C", "D"), 
      to = c("B", "B", "C", "D", "D"), 
      stringsAsFactors = FALSE
    )
    # B->B and D->D are self-loops
    unique_kept <- get_unique_edges(edges_sl, self_loops = "keep")
    expect_equal(nrow(unique_kept), 5) # A->B, B->B, B->C, C->D, D->D
    expect_true(any(unique_kept$from == "B" & unique_kept$to == "B"))
    expect_true(any(unique_kept$from == "D" & unique_kept$to == "D"))
    expect_equal(unique_kept[order(unique_kept$from, unique_kept$to),],
                 data.frame(from=c("A","B","B","C","D"), to=c("B","B","C","D","D"), stringsAsFactors=FALSE)[order(c("A","B","B","C","D"),c("B","B","C","D","D")),])
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
    # Only X->Y is unique, Z->Z is a self-loop
    unique_c <- get_unique_edges(edges_custom, from_col = "source_node", to_col = "target_node", self_loops = "drop")
    expect_equal(nrow(unique_c), 1)
    expect_equal(names(unique_c), c("source_node", "target_node"))
    expect_false(any(unique_c$source_node == "Z" & unique_c$target_node == "Z"))
    # Expected: X->Y
    expect_equal(unique_c[order(unique_c$source_node, unique_c$target_node),],
                 data.frame(source_node=c("X"), target_node=c("Y"), stringsAsFactors=FALSE)[order(c("X"),c("Y")),])

    unique_c_kept <- get_unique_edges(edges_custom, from_col = "source_node", to_col = "target_node", self_loops = "keep")    
    expect_equal(nrow(unique_c_kept), 3) # X->Y, Y->Y, Z->Z
    expect_true(any(unique_c_kept$source_node == "Z" & unique_c_kept$target_node == "Z"))
    expect_true(any(unique_c_kept$source_node == "Y" & unique_c_kept$target_node == "Y"))
    expect_equal(unique_c_kept[order(unique_c_kept$source_node, unique_c_kept$target_node),],
                 data.frame(source_node=c("X","Y","Z"), target_node=c("Y","Y","Z"), stringsAsFactors=FALSE)[order(c("X","Y","Z"),c("Y","Y","Z")),])
  })
  
  it("handles NA values correctly (NAs are dropped)", {
    edges_na <- data.frame(
      from = c("A", NA, "A", "B", NA, "X"), 
      to =   c("B", "C", "B", "D", "C", NA),
      stringsAsFactors = FALSE
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
  
  it("handles data frame with columns having all NAs (all dropped)", {
    df_all_na <- data.frame(from = c(NA_character_, NA_character_), to = c(NA_character_, NA_character_), stringsAsFactors = FALSE)
    unique_all_na <- get_unique_edges(df_all_na)
    expect_equal(nrow(unique_all_na), 0)
    expect_equal(names(unique_all_na), c("from", "to"))
  })
}) 