# Tests for export_graph()

describe("export_graph GraphML format", {
  it("writes a valid GraphML file", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to   = c("B", "C", "A"),
      stringsAsFactors = FALSE
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
      from = c("A", "B"), to = c("B", "A"), stringsAsFactors = FALSE
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".graphml")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp, format = "graphml")
    content <- paste(readLines(tmp), collapse = "\n")
    expect_true(grepl("pagerank", content))
  })
})


describe("export_graph DOT format", {
  it("writes a valid DOT file", {
    edges <- data.frame(
      from = c("A", "B", "C"),
      to   = c("B", "C", "A"),
      stringsAsFactors = FALSE
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".dot")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp, format = "dot")
    expect_true(file.exists(tmp))
    content <- readLines(tmp)
    expect_true(any(grepl("digraph pagerank", content)))
    expect_true(any(grepl("->", content)))
    # Check PR values are included
    full <- paste(content, collapse = "\n")
    expect_true(grepl("PR:", full))
  })

  it("handles URLs with special characters", {
    edges <- data.frame(
      from = c("http://example.com/a"),
      to   = c("http://example.com/b"),
      stringsAsFactors = FALSE
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
      from = c("A", "B"), to = c("B", "A"), stringsAsFactors = FALSE
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

    nodes <- read.csv(paste0(base, "_nodes.csv"), stringsAsFactors = FALSE)
    expect_true("pagerank" %in% names(nodes))
  })
})


describe("export_graph with extra attributes", {
  it("includes edge attributes", {
    edges <- data.frame(
      from = c("A", "B"), to = c("B", "A"),
      weight = c(1.0, 2.0), stringsAsFactors = FALSE
    )
    pr <- pagerank(edges, clean_edge_urls = FALSE)
    tmp <- tempfile(fileext = ".graphml")
    on.exit(unlink(tmp))

    export_graph(pr, edges, tmp, format = "graphml",
                 edge_attrs = "weight")
    content <- paste(readLines(tmp), collapse = "\n")
    expect_true(grepl("weight", content))
  })
})


describe("export_graph input validation", {
  it("errors on non-data-frame pagerank_df", {
    expect_error(
      export_graph("bad", data.frame(from="A", to="B"), tempfile()),
      "must be a data frame"
    )
  })

  it("errors on missing PR columns", {
    pr <- data.frame(x = "A", y = 0.5, stringsAsFactors = FALSE)
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(
      export_graph(pr, edges, tempfile(), format = "graphml"),
      "must have.*node_name"
    )
  })

  it("errors on invalid file path", {
    pr <- data.frame(node_name = "A", pagerank = 0.5, stringsAsFactors = FALSE)
    edges <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(
      export_graph(pr, edges, 123, format = "graphml"),
      "must be a single file path"
    )
  })
})
