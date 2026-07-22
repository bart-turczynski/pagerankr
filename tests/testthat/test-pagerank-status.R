context("pagerank status_df input contract")

# A small graph where X is a candidate response-dead page: it receives an
# inlink from A and (unusually) also has an outlink to B.
status_edges <- function() {
  data.frame(
    from = c("A", "B", "X", "A"),
    to = c("X", "A", "B", "B"),
    stringsAsFactors = FALSE
  )
}

dead_count <- function(res) {
  attr(res, "transition_audit")$dropped$n_status_dead
}

describe("status_df classification", {
  it("counts a 4xx in-graph page as response-dead", {
    st <- data.frame(url = c("A", "B", "X"), status_code = c(200L, 200L, 404L))
    res <- pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 1L)
    expect_true(attr(res, "transition_audit")$config$has_status)
    # Response accounting never breaks normalization.
    expect_equal(sum(res[[2]]), 1, tolerance = 1e-8)
  })

  it("treats 4xx and 5xx identically (no transient/permanent split)", {
    edges <- data.frame(
      from = c("A", "B", "X", "Y"), to = c("X", "A", "B", "B"),
      stringsAsFactors = FALSE
    )
    st <- data.frame(
      url = c("A", "B", "X", "Y"),
      status_code = c(200L, 200L, 503L, 404L)
    )
    res <- pagerank(edges, status_df = st, clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 2L)
  })

  it("does not treat 3xx as response-dead (redirects are separate)", {
    st <- data.frame(url = c("A", "B", "X"), status_code = c(200L, 200L, 301L))
    res <- pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 0L)
  })

  it("does not treat codes below 400 as response-dead", {
    st <- data.frame(url = c("A", "B", "X"), status_code = c(200L, 204L, 399L))
    res <- pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 0L)
  })

  it("ignores missing or unparseable status codes (treated as live)", {
    st <- data.frame(
      url = c("A", "B", "X"),
      status_code = c("200", "n/a", NA),
      stringsAsFactors = FALSE
    )
    res <- pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 0L)
  })

  it("coerces character status codes", {
    st <- data.frame(
      url = c("A", "B", "X"),
      status_code = c("200", "200", "404"),
      stringsAsFactors = FALSE
    )
    res <- pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 1L)
  })

  it("excludes dead URLs that are not vertices in the graph", {
    st <- data.frame(
      url = c("A", "B", "Z"), status_code = c(200L, 200L, 404L)
    )
    res <- pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 0L)
  })

  it("reports zero and has_status = FALSE when no status_df is supplied", {
    res <- pagerank(status_edges(), clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 0L)
    expect_false(attr(res, "transition_audit")$config$has_status)
  })

  it("accepts an empty status_df without error", {
    st <- data.frame(url = character(0), status_code = integer(0))
    res <- pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE)
    expect_equal(dead_count(res), 0L)
  })

  it("honors custom status_url_col / status_col names", {
    st <- data.frame(page = c("A", "B", "X"), http = c(200L, 200L, 500L))
    res <- pagerank(
      status_edges(),
      status_df = st, status_url_col = "page", status_col = "http",
      clean_edge_urls = FALSE
    )
    expect_equal(dead_count(res), 1L)
  })
})

describe("status_df validation", {
  it("errors when the url column is missing", {
    st <- data.frame(page = "X", status_code = 404L)
    expect_error(
      pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE),
      "status_url_col"
    )
  })

  it("errors when the status column is missing", {
    st <- data.frame(url = "X", code = 404L)
    expect_error(
      pagerank(status_edges(), status_df = st, clean_edge_urls = FALSE),
      "status_col"
    )
  })

  it("errors when status_df is not a data frame", {
    expect_error(
      pagerank(status_edges(), status_df = list(url = "X"),
        clean_edge_urls = FALSE),
      "status_df.*data frame"
    )
  })

  it("errors when status_df is supplied under reverse = TRUE", {
    st <- data.frame(url = "A", status_code = 404L)
    expect_error(
      pagerank(status_edges(), status_df = st, reverse = TRUE,
        clean_edge_urls = FALSE),
      "status_df.*reverse|reverse.*status_df"
    )
  })
})
