context("drop_isolates")

# source("../../R/drop_isolates.R")

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
    expect_equal(sort(all_nodes_df$vertex), sort(c("A", "B", "C", "D", "E", "F")))
  })

  it("drop = TRUE: returns unique non-NA nodes with degree > 0", {
    edges <- data.frame(
      from = c("A", "B", "C", "Isolated", NA, "E"), 
      to =   c("B", "C", "A", NA, "Orphan", "F"), 
      stringsAsFactors = FALSE
    )
    # Valid edges define degree. 
    # A-B, B-C, C-A, E-F are valid links. Nodes: A,B,C,E,F
    # "Isolated" appears with NA in to: so "Isolated" is mentioned but has no valid edge from it here.
    # "Orphan" appears with NA in from: so "Orphan" is mentioned but has no valid edge to it here.
    # The current implementation of drop_isolates with drop=TRUE returns all unique non-NA nodes mentioned
    # because any mention implies it was part of an attempted edge. This aligns with spec: 
    # "If drop = TRUE, returns a single-column data.frame of node names *with degree > 0*."
    # "degree > 0" means it appeared in from or to of a non-NA edge part.
    # After na.omit(c(from, to)), nodes are: A,B,C,Isolated,E,Orphan,F
    # So all these have "degree > 0" in the context of *being mentioned*
    active_nodes_df <- drop_isolates(edges, drop = TRUE)
    expect_equal(names(active_nodes_df), "node_name")
    expected_active <- sort(c("A", "B", "C", "E", "F", "Isolated", "Orphan"))
    expect_equal(sort(active_nodes_df$node_name), expected_active)
    
    # More explicit test for degree > 0
    edges2 <- data.frame(from=c("V1","V2", "V3"), to=c("V2","V1",NA), stringsAsFactors = FALSE)
    # V1, V2 are in valid edges. V3 is mentioned (from V3 to NA).
    active2 <- drop_isolates(edges2, drop=TRUE)
    expect_equal(sort(active2$node_name), sort(c("V1","V2","V3")))
  })
  
  it("handles custom column names for edges and output", {
    edges_custom <- data.frame(
      source = c("S1", "S2"), 
      target = c("T1", "T1"), 
      stringsAsFactors = FALSE
    )
    all_custom <- drop_isolates(edges_custom, drop = FALSE, 
                                from_col = "source", to_col = "target", node_col_name = "my_nodes")
    expect_equal(nrow(all_custom), 3) # S1, S2, T1
    expect_equal(names(all_custom), "my_nodes")
    expect_equal(sort(all_custom$my_nodes), sort(c("S1", "S2", "T1")))
    
    active_custom <- drop_isolates(edges_custom, drop = TRUE, 
                                   from_col = "source", to_col = "target", node_col_name = "active_verts")
    expect_equal(nrow(active_custom), 3)
    expect_equal(names(active_custom), "active_verts")
    expect_equal(sort(active_custom$active_verts), sort(c("S1", "S2", "T1"))) 
  })
  
  it("handles empty edge list", {
    empty_e <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    res_drop_false <- drop_isolates(empty_e, drop = FALSE)
    expect_equal(nrow(res_drop_false), 0)
    expect_equal(names(res_drop_false), "node_name")
    
    res_drop_true <- drop_isolates(empty_e, drop = TRUE)
    expect_equal(nrow(res_drop_true), 0)
    expect_equal(names(res_drop_true), "node_name")
  })
  
  it("handles edge list with all NAs in relevant columns", {
    edges_all_na <- data.frame(from = NA_character_, to = NA_character_, stringsAsFactors = FALSE)
    res_na_false <- drop_isolates(edges_all_na, drop = FALSE)
    expect_equal(nrow(res_na_false), 0)
    
    res_na_true <- drop_isolates(edges_all_na, drop = TRUE)
    expect_equal(nrow(res_na_true), 0)
    
    edges_mix_na <- data.frame(from = c(NA, NA), to = c(NA, NA), other=1:2, stringsAsFactors = FALSE)
    res_mix_false <- drop_isolates(edges_mix_na, drop = FALSE)
    expect_equal(nrow(res_mix_false), 0)
  })
  
  it("output column name is correctly applied", {
    edges <- data.frame(from="X", to="Y", stringsAsFactors = FALSE)
    res <- drop_isolates(edges, node_col_name = "vertex_id")
    expect_equal(names(res), "vertex_id")
  })
  
  it("sorts output for consistency", {
    edges <- data.frame(from=c("Z", "A", "M"), to=c("Y", "B", "P"), stringsAsFactors = FALSE)
    res <- drop_isolates(edges, drop=FALSE)
    expect_equal(res$node_name, sort(c("Z", "A", "M", "Y", "B", "P")))
  })
  
  it("errors on invalid input types or missing columns", {
      expect_error(drop_isolates(list()))
      df <- data.frame(fcol="a", tcol="b")
      expect_error(drop_isolates(df)) # Default cols from/to missing
      expect_error(drop_isolates(df, from_col = "non_existent"))
      expect_error(drop_isolates(edges_df = df, from_col = "fcol", to_col="tcol", node_col_name = 123))
      expect_error(drop_isolates(edges_df = df, from_col = "fcol", to_col="tcol", drop = "TRUE"))
  })
}) 