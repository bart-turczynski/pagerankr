context("pagerank duplicate_edge_policy")

pr_value <- function(result, node) {
  result$pagerank[result$node_name == node]
}

expect_same_scores <- function(a, b, tolerance = 1e-9) {
  a <- a[order(a$node_name), c("node_name", "pagerank")]
  b <- b[order(b$node_name), c("node_name", "pagerank")]
  row.names(a) <- NULL
  row.names(b) <- NULL
  expect_equal(a$node_name, b$node_name)
  expect_equal(a$pagerank, b$pagerank, tolerance = tolerance)
}

test_that("default collapse preserves duplicate-unweighted behavior", {
  edges_dup <- data.frame(
    from = c("A", "A", "A", "B", "C"),
    to = c("B", "C", "C", "A", "A"),
    stringsAsFactors = FALSE
  )
  edges_collapsed <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "A", "A"),
    stringsAsFactors = FALSE
  )

  pr_default <- pagerank(edges_dup, clean_edge_urls = FALSE)
  pr_collapsed <- pagerank(edges_collapsed, clean_edge_urls = FALSE)

  expect_same_scores(pr_default, pr_collapsed)

  audit <- attr(pr_default, "transition_audit")
  expect_equal(audit$duplicates$policy, "collapse")
  expect_equal(audit$dropped$n_rows_duplicate, 1L)
})

test_that("count_instances gives repeated links extra transition mass", {
  edges_dup <- data.frame(
    from = c("A", "A", "A", "B", "C"),
    to = c("B", "C", "C", "A", "A"),
    stringsAsFactors = FALSE
  )
  edges_weighted <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "A", "A"),
    w = c(1, 2, 1, 1),
    stringsAsFactors = FALSE
  )

  pr_counted <- pagerank(
    edges_dup,
    duplicate_edge_policy = "count_instances",
    clean_edge_urls = FALSE
  )
  pr_weighted <- pagerank(
    edges_weighted,
    weight_col = "w",
    clean_edge_urls = FALSE
  )

  expect_same_scores(pr_counted, pr_weighted)
  expect_gt(pr_value(pr_counted, "C"), pr_value(pr_counted, "B"))

  audit <- attr(pr_counted, "transition_audit")
  expect_equal(audit$coverage$weight_col, "__pr_instance_count__")
  expect_equal(audit$duplicates$instance_count_col, "__pr_instance_count__")
  expect_equal(audit$duplicates$n_duplicate_rows, 1L)
  expect_equal(audit$duplicates$n_duplicate_instances, 2L)
  expect_equal(nrow(audit$duplicates$duplicate_edges), 1L)
  expect_equal(audit$duplicates$duplicate_edges$from, "A")
  expect_equal(audit$duplicates$duplicate_edges$to, "C")
  expect_equal(audit$duplicates$duplicate_edges$instance_count, 2L)
  expect_equal(audit$duplicates$duplicate_edges$effective_weight, 2)
})

test_that("aggregate and count_instances sum duplicate weight_col values", {
  edges <- data.frame(
    from = c("A", "A", "A", "B", "C"),
    to = c("B", "C", "C", "A", "A"),
    w = c(1, 2, 3, 1, 1),
    stringsAsFactors = FALSE
  )
  expected <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "A", "A"),
    w = c(1, 5, 1, 1),
    stringsAsFactors = FALSE
  )

  pr_aggregate <- pagerank(
    edges,
    weight_col = "w",
    duplicate_edge_policy = "aggregate",
    clean_edge_urls = FALSE
  )
  pr_counted <- pagerank(
    edges,
    weight_col = "w",
    duplicate_edge_policy = "count_instances",
    clean_edge_urls = FALSE
  )
  pr_expected <- pagerank(
    expected,
    weight_col = "w",
    clean_edge_urls = FALSE
  )

  expect_same_scores(pr_aggregate, pr_expected)
  expect_same_scores(pr_counted, pr_expected)
  expect_equal(attr(pr_counted, "transition_audit")$config$weight_col, "w")
  expect_equal(
    attr(pr_counted, "transition_audit")$config$effective_weight_col,
    "w"
  )
})

test_that("aggregate duplicate policy combines nofollow before handling", {
  edges <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("C", "C", "A", "A"),
    nofollow = c(FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  pr_aggregate <- pagerank(
    edges,
    duplicate_edge_policy = "aggregate",
    nofollow_col = "nofollow",
    nofollow_action = "drop",
    clean_edge_urls = FALSE
  )
  pr_expected <- pagerank(
    data.frame(
      from = c("B", "C"),
      to = c("A", "A"),
      stringsAsFactors = FALSE
    ),
    clean_edge_urls = FALSE
  )

  expect_same_scores(pr_aggregate, pr_expected)
  expect_equal(
    attr(pr_aggregate, "transition_audit")$duplicates$policy,
    "aggregate"
  )
})

test_that("redirect and canonical folding duplicates obey count_instances", {
  edges <- data.frame(
    from = c("A-old", "A", "A", "B", "C"),
    to = c("B", "C-old", "C", "A", "A"),
    stringsAsFactors = FALSE
  )
  redirects <- data.frame(
    from = "A-old",
    to = "A",
    stringsAsFactors = FALSE
  )
  canonicals <- data.frame(
    from = "C-old",
    to = "C",
    stringsAsFactors = FALSE
  )
  expected <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "A", "A"),
    w = c(1, 2, 1, 1),
    stringsAsFactors = FALSE
  )

  pr_counted <- pagerank(
    edges,
    redirects_df = redirects,
    canonicals_df = canonicals,
    clean_edge_urls = FALSE,
    clean_redirect_urls = FALSE,
    clean_canonical_urls = FALSE,
    duplicate_edge_policy = "count_instances"
  )
  pr_expected <- pagerank(
    expected,
    weight_col = "w",
    clean_edge_urls = FALSE
  )

  expect_same_scores(pr_counted, pr_expected)

  audit <- attr(pr_counted, "transition_audit")
  expect_true(audit$config$has_redirects)
  expect_true(audit$config$has_canonicals)
  expect_equal(audit$dropped$n_rows_duplicate, 1L)
})
