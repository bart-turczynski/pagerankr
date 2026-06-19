# Tests for export_graph()

describe("export_graph GraphML format", {
  it("writes a valid GraphML file", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".graphml")
    on.exit(unlink(tmp))

    result <- export_graph(pr, edges, tmp, format = "graphml")
    expect_equal(result, tmp)
    expect_true(file.exists(tmp))
    content <- readLines(tmp)
    expect_true(any(grepl("graphml", content, ignore.case = TRUE)))
  })

  it("includes pagerank as vertex attribute", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".graphml")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp, format = "graphml")
    content <- paste(readLines(tmp), collapse = "\n")
    expect_true(grepl("pagerank", content, fixed = TRUE))
  })
})


describe("export_graph DOT format", {
  it("writes a valid DOT file", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".dot")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp, format = "dot")
    expect_true(file.exists(tmp))
    content <- readLines(tmp)
    expect_true(any(grepl("digraph pagerank", content, fixed = TRUE)))
    expect_true(any(grepl("->", content, fixed = TRUE)))
    # Check PR values are included
    full <- paste(content, collapse = "\n")
    expect_true(grepl("PR:", full, fixed = TRUE))
  })

  it("handles URLs with special characters", {
    edges <- data.frame(
      from = c("http://example.com/a"),
      to = c("http://example.com/b")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".dot")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp, format = "dot")
    expect_true(file.exists(tmp))
  })
})


describe("export_graph edgelist format", {
  it("writes edge and node CSV files", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".csv")
    on.exit({
      base <- tools::file_path_sans_ext(tmp)
      unlink(paste0(base, "_edges.csv"))
      unlink(paste0(base, "_nodes.csv"))
    })

    expect_message(
      export_graph(pr, edges, tmp, format = "edgelist"),
      "Wrote:"
    )

    base <- tools::file_path_sans_ext(tmp)
    expect_true(file.exists(paste0(base, "_edges.csv")))
    expect_true(file.exists(paste0(base, "_nodes.csv")))

    nodes <- read.csv(paste0(base, "_nodes.csv"))
    expect_true("pagerank" %in% names(nodes))
  })
})


describe("export_graph with extra attributes", {
  it("includes edge attributes", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A"),
      weight = c(1.0, 2.0)
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".graphml")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp,
      format = "graphml",
      edge_attrs = "weight"
    )
    content <- paste(readLines(tmp), collapse = "\n")
    expect_true(grepl("weight", content, fixed = TRUE))
  })
})


describe("export_graph pajek format", {
  it("writes a pajek file", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".net")
    on.exit(unlink(tmp))

    result <- export_graph(pr, edges, tmp, format = "pajek")
    expect_equal(result, tmp)
    expect_true(file.exists(tmp))
  })
})


describe("export_graph node_attrs parameter", {
  it("includes additional node attributes from pagerank_df", {
    edges <- data.frame(
      from = c("A", "B", "C"), to = c("B", "C", "A")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    pr$my_rank <- seq_len(nrow(pr))
    tmp <- tempfile(fileext = ".graphml")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp,
      format = "graphml",
      node_attrs = list(rank = "my_rank")
    )
    content <- paste(readLines(tmp), collapse = "\n")
    expect_true(grepl("rank", content, fixed = TRUE))
  })
})


describe("export_graph missing vertices in edge list", {
  it("adds vertices from edge list not in pagerank_df", {
    # pagerank_df only has A, but edges reference A and B
    pr <- data.frame(node_name = "A", pagerank = 1.0)
    edges <- data.frame(from = "A", to = "B")
    tmp <- tempfile(fileext = ".graphml")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp, format = "graphml")
    content <- paste(readLines(tmp), collapse = "\n")
    # B should be present as a vertex
    expect_true(grepl("B", content, fixed = TRUE))
  })
})


describe("export_graph DOT with zero pagerank", {
  it("handles max_pr == 0 without error", {
    pr <- data.frame(
      node_name = c("A", "B"), pagerank = c(0, 0)
    )
    edges <- data.frame(from = "A", to = "B")
    tmp <- tempfile(fileext = ".dot")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp, format = "dot")
    expect_true(file.exists(tmp))
    content <- readLines(tmp)
    expect_true(any(grepl("digraph pagerank", content, fixed = TRUE)))
  })
})


describe("export_graph edgelist with no file extension", {
  it("defaults to csv extension when file has none", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A")
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = "")
    on.exit({
      unlink(paste0(tmp, "_edges.csv"))
      unlink(paste0(tmp, "_nodes.csv"))
    })

    expect_message(
      export_graph(pr, edges, tmp, format = "edgelist"),
      "Wrote:"
    )
    expect_true(file.exists(paste0(tmp, "_edges.csv")))
    expect_true(file.exists(paste0(tmp, "_nodes.csv")))
  })
})


describe("export_graph input validation", {
  it("errors on non-data-frame pagerank_df", {
    expect_error(
      export_graph("bad", data.frame(from = "A", to = "B"), tempfile()),
      "must be a data frame"
    )
  })

  it("errors on missing PR url column", {
    pr <- data.frame(x = "A", y = 0.5)
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      export_graph(pr, edges, tempfile(), format = "graphml"),
      "must have.*node_name"
    )
  })

  it("errors on missing PR score column", {
    pr <- data.frame(node_name = "A", wrong = 0.5)
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      export_graph(pr, edges, tempfile(), format = "graphml"),
      "must have.*pagerank"
    )
  })

  it("errors on non-data-frame edge_list_df", {
    pr <- data.frame(node_name = "A", pagerank = 0.5)
    expect_error(
      export_graph(pr, "not_a_df", tempfile(), format = "graphml"),
      "must be a data frame"
    )
  })

  it("errors on missing edge list columns", {
    pr <- data.frame(node_name = "A", pagerank = 0.5)
    edges <- data.frame(x = "A", y = "B")
    expect_error(
      export_graph(pr, edges, tempfile(), format = "graphml"),
      "must have.*from.*to"
    )
  })

  it("errors on invalid file path", {
    pr <- data.frame(node_name = "A", pagerank = 0.5)
    edges <- data.frame(from = "A", to = "B")
    expect_error(
      export_graph(pr, edges, 123, format = "graphml"),
      "must be a single file path"
    )
  })
})
