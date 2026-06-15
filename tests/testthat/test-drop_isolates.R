context("drop_isolates")

describe("drop_isolates basic functionality", {
  it("drop = FALSE: returns all unique non-NA nodes from edge list", {
    edges <- data.frame(
      from = c("A", "B", "C", NA, "D", "A"),
      to = c("B", "C", "A", "E", NA, "F"),
      stringsAsFactors = FALSE
    )
    # Unique non-NA nodes: A, B, C, D, E, F
    all_nodes_df <- drop_isolates(edges, drop = FALSE, node_col_name = "vertex")
    expect_equal(nrow(all_nodes_df), 6)
    expect_equal(names(all_nodes_df), "vertex")
    expect_equal(
      sort(all_nodes_df$vertex),
      sort(c("A", "B", "C", "D", "E", "F"))
    )
  })

  it("drop = TRUE: returns only nodes participating in complete edges", {
    edges <- data.frame(
      from = c("A", "B", "C", "Isolated", NA, "E"),
      to = c("B", "C", "A", NA, "Orphan", "F"),
      stringsAsFactors = FALSE
    )
    # Complete edges: A->B, B->C, C->A, E->F. Connected nodes: A,B,C,E,F
    # "Isolated" appears only in row with NA to: isolate
    # (not in a complete edge)
    # "Orphan" appears only in row with NA from: isolate
    # (not in a complete edge)
    active_nodes_df <- drop_isolates(edges, drop = TRUE)
    expect_equal(names(active_nodes_df), "node_name")
    expected_active <- sort(c("A", "B", "C", "E", "F"))
    expect_equal(sort(active_nodes_df$node_name), expected_active)

    # More explicit test: V3 only appears in partial row (V3 -> NA)
    edges2 <- data.frame(
      from = c("V1", "V2", "V3"),
      to = c("V2", "V1", NA),
      stringsAsFactors = FALSE
    )
    # V1, V2 are in complete edges. V3 is only in a partial row (isolate).
    active2 <- drop_isolates(edges2, drop = TRUE)
    expect_equal(sort(active2$node_name), sort(c("V1", "V2")))
  })

  it("handles custom column names for edges and output", {
    edges_custom <- data.frame(
      source = c("S1", "S2"),
      target = c("T1", "T1"),
      stringsAsFactors = FALSE
    )
    # All rows are complete edges, so drop=TRUE and drop=FALSE
    # return the same nodes
    all_custom <- drop_isolates(edges_custom,
      drop = FALSE,
      from_col = "source", to_col = "target", node_col_name = "my_nodes"
    )
    expect_equal(nrow(all_custom), 3) # S1, S2, T1
    expect_equal(names(all_custom), "my_nodes")
    expect_equal(sort(all_custom$my_nodes), sort(c("S1", "S2", "T1")))

    active_custom <- drop_isolates(edges_custom,
      drop = TRUE,
      from_col = "source", to_col = "target", node_col_name = "active_verts"
    )
    expect_equal(nrow(active_custom), 3) # All rows are complete, so same result
    expect_equal(names(active_custom), "active_verts")
    expect_equal(sort(active_custom$active_verts), sort(c("S1", "S2", "T1")))
  })

  it("handles empty edge list", {
    empty_e <- data.frame(
      from = character(0),
      to = character(0),
      stringsAsFactors = FALSE
    )
    res_drop_false <- drop_isolates(empty_e, drop = FALSE)
    expect_equal(nrow(res_drop_false), 0)
    expect_equal(names(res_drop_false), "node_name")

    res_drop_true <- drop_isolates(empty_e, drop = TRUE)
    expect_equal(nrow(res_drop_true), 0)
    expect_equal(names(res_drop_true), "node_name")
  })

  it("handles edge list with all NAs in relevant columns", {
    edges_all_na <- data.frame(
      from = NA_character_,
      to = NA_character_,
      stringsAsFactors = FALSE
    )
    res_na_false <- drop_isolates(edges_all_na, drop = FALSE)
    expect_equal(nrow(res_na_false), 0)

    res_na_true <- drop_isolates(edges_all_na, drop = TRUE)
    expect_equal(nrow(res_na_true), 0)

    edges_mix_na <- data.frame(
      from = c(NA, NA),
      to = c(NA, NA),
      other = 1:2,
      stringsAsFactors = FALSE
    )
    res_mix_false <- drop_isolates(edges_mix_na, drop = FALSE)
    expect_equal(nrow(res_mix_false), 0)
  })

  it("output column name is correctly applied", {
    edges <- data.frame(from = "X", to = "Y", stringsAsFactors = FALSE)
    res <- drop_isolates(edges, node_col_name = "vertex_id")
    expect_equal(names(res), "vertex_id")
  })

  it("sorts output for consistency", {
    edges <- data.frame(
      from = c("Z", "A", "M"),
      to = c("Y", "B", "P"),
      stringsAsFactors = FALSE
    )
    res <- drop_isolates(edges, drop = FALSE)
    expect_equal(res$node_name, sort(c("Z", "A", "M", "Y", "B", "P")))
  })

  it("errors on invalid input types or missing columns", {
    expect_error(drop_isolates(list()))
    df <- data.frame(fcol = "a", tcol = "b")
    expect_error(drop_isolates(df)) # Default cols from/to missing
    expect_error(drop_isolates(df, from_col = "non_existent"))
    expect_error(drop_isolates(
      edges_df = df,
      from_col = "fcol",
      to_col = "tcol",
      node_col_name = 123
    ))
    expect_error(drop_isolates(
      edges_df = df,
      from_col = "fcol",
      to_col = "tcol",
      drop = "TRUE"
    ))
  })
})

describe("drop_isolates isolate detection with partial rows", {
  it("drop=TRUE and drop=FALSE produce different results when isolates exist", {
    # Partial rows: "Orphan" only appears in a row with NA from,
    # "Dead" only appears in a row with NA to.
    edges <- data.frame(
      from = c("A", "B", NA, "Dead"),
      to = c("B", "A", "Orphan", NA),
      stringsAsFactors = FALSE
    )

    all_nodes <- drop_isolates(edges, drop = FALSE)
    active_nodes <- drop_isolates(edges, drop = TRUE)

    # drop=FALSE: full vertex universe (A, B, Dead, Orphan)
    expect_equal(sort(all_nodes$node_name), sort(c("A", "B", "Dead", "Orphan")))
    # drop=TRUE: only nodes from complete edges (A, B)
    expect_equal(sort(active_nodes$node_name), sort(c("A", "B")))

    # They must differ
    expect_false(nrow(all_nodes) == nrow(active_nodes))
  })

  it("drop=TRUE and drop=FALSE return the same result when no isolates exist", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      stringsAsFactors = FALSE
    )

    all_nodes <- drop_isolates(edges, drop = FALSE)
    active_nodes <- drop_isolates(edges, drop = TRUE)

    expect_equal(sort(all_nodes$node_name), sort(active_nodes$node_name))
  })

  it("drop=TRUE returns empty when all rows are partial", {
    edges <- data.frame(
      from = c("A", NA),
      to = c(NA, "B"),
      stringsAsFactors = FALSE
    )

    active_nodes <- drop_isolates(edges, drop = TRUE)
    all_nodes <- drop_isolates(edges, drop = FALSE)

    expect_equal(nrow(active_nodes), 0)
    expect_equal(sort(all_nodes$node_name), sort(c("A", "B")))
  })

  it("errors when drop is not a single logical", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(drop_isolates(edges, drop = "yes"), "single logical")
    expect_error(drop_isolates(edges, drop = c(TRUE, FALSE)), "single logical")
  })

  it("errors when node_col_name is invalid", {
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(drop_isolates(edges, node_col_name = ""), "non-empty")
    expect_error(drop_isolates(edges, node_col_name = 123), "non-empty")
  })
})
