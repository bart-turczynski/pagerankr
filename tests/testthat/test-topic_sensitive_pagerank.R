# Tests for topic_sensitive_pagerank(): multi-vector personalized PageRank.

# A small multi-topic site: an "ai" cluster (/ai, /ai-demo) and a "pricing"
# cluster (/pricing), wired so each cluster's seed bias concentrates authority
# locally in a way the uniform run does not.
make_site <- function() {
  data.frame(
    from = c("/", "/", "/", "/ai", "/ai", "/ai-demo", "/blog", "/pricing"),
    to = c(
      "/ai", "/blog", "/pricing", "/ai-demo", "/pricing", "/ai", "/ai", "/"
    ),
    stringsAsFactors = FALSE
  )
}

run_tspr <- function(...) {
  suppressMessages(topic_sensitive_pagerank(...))
}

test_that("returns one column per topic plus a blended column, sorted", {
  res <- run_tspr(
    make_site(),
    topics = list(ai = c("/ai", "/ai-demo"), pricing = "/pricing"),
    clean_edge_urls = FALSE
  )

  expect_s3_class(res, "data.frame")
  expect_identical(names(res), c("node_name", "ai", "pricing", "blended"))
  # Sorted by blended descending.
  expect_equal(res$blended, sort(res$blended, decreasing = TRUE))
  # Attributes carry the normalized weights and per-topic audits.
  expect_equal(unname(attr(res, "topic_weights")), c(0.5, 0.5))
  expect_named(attr(res, "topic_audits"), c("ai", "pricing"))
})

test_that("each topic biases authority toward its own cluster", {
  topics <- list(ai = c("/ai", "/ai-demo"), pricing = "/pricing")
  res <- run_tspr(make_site(), topics = topics, clean_edge_urls = FALSE)

  ai_score <- function(node) res$ai[res$node_name == node]
  pr_score <- function(node) res$pricing[res$node_name == node]

  # /ai-demo is squarely inside the AI cluster: higher under the AI vector
  # than under the pricing vector.
  expect_gt(ai_score("/ai-demo"), pr_score("/ai-demo"))
  # /pricing is the pricing seed: higher under the pricing vector.
  expect_gt(pr_score("/pricing"), ai_score("/pricing"))
})

test_that("blended equals the weighted combination of per-topic scores", {
  topics <- list(ai = c("/ai", "/ai-demo"), pricing = "/pricing")
  w <- c(ai = 0.7, pricing = 0.3)
  res <- run_tspr(
    make_site(),
    topics = topics, topic_weights = w, clean_edge_urls = FALSE
  )

  expect_equal(
    res$blended,
    0.7 * res$ai + 0.3 * res$pricing,
    tolerance = 1e-12
  )
  expect_equal(attr(res, "topic_weights"), c(ai = 0.7, pricing = 0.3))
})

test_that("equal weights are the default and weights normalize to sum 1", {
  topics <- list(ai = "/ai", pricing = "/pricing")
  res_default <- run_tspr(make_site(), topics = topics, clean_edge_urls = FALSE)
  # Unnormalized weights with the same ratio give an identical blend.
  res_scaled <- run_tspr(
    make_site(),
    topics = topics, topic_weights = c(5, 5), clean_edge_urls = FALSE
  )
  expect_equal(res_default$blended, res_scaled$blended, tolerance = 1e-12)
  expect_equal(sum(attr(res_default, "topic_weights")), 1)
})

test_that("a single topic reproduces the equivalent prior_df pagerank run", {
  edges <- make_site()
  prior <- data.frame(url = "/ai", weight = 1, stringsAsFactors = FALSE)
  direct <- suppressMessages(
    pagerank(edges, prior_df = prior, clean_edge_urls = FALSE)
  )
  res <- run_tspr(edges, topics = list(ai = "/ai"), clean_edge_urls = FALSE)

  merged <- merge(
    direct[, c("node_name", "pagerank")],
    res[, c("node_name", "ai")],
    by = "node_name"
  )
  expect_equal(merged$ai, merged$pagerank, tolerance = 1e-10)
  # With one topic the blended column equals that topic.
  expect_equal(res$ai, res$blended, tolerance = 1e-12)
})

test_that("weighted-seed data-frame topics are accepted", {
  topics <- list(
    ai = data.frame(
      url = c("/ai", "/ai-demo"), weight = c(3, 1),
      stringsAsFactors = FALSE
    ),
    pricing = "/pricing"
  )
  res <- run_tspr(make_site(), topics = topics, clean_edge_urls = FALSE)
  expect_identical(names(res), c("node_name", "ai", "pricing", "blended"))
  expect_true(all(is.finite(res$ai)))
})

test_that("custom topic url/weight column names are honored", {
  topics <- list(
    ai = data.frame(
      page = c("/ai", "/ai-demo"), w = c(1, 1),
      stringsAsFactors = FALSE
    )
  )
  res <- run_tspr(
    make_site(),
    topics = topics, topic_url_col = "page", topic_weight_col = "w",
    clean_edge_urls = FALSE
  )
  expect_identical(names(res), c("node_name", "ai", "blended"))
})

# --- Validation / error paths ---

test_that("topics must be a non-empty uniquely named list", {
  expect_error(
    topic_sensitive_pagerank(make_site(), topics = list()),
    "non-empty named list"
  )
  expect_error(
    topic_sensitive_pagerank(make_site(), topics = list("/ai")),
    "non-empty name"
  )
  expect_error(
    topic_sensitive_pagerank(
      make_site(),
      topics = list(ai = "/ai", ai = "/pricing")
    ),
    "unique"
  )
})

test_that("reserved topic names are rejected", {
  expect_error(
    topic_sensitive_pagerank(
      make_site(),
      topics = list(node_name = "/ai")
    ),
    "must not be"
  )
  expect_error(
    topic_sensitive_pagerank(
      make_site(),
      topics = list(blended = "/ai")
    ),
    "must not be"
  )
})

test_that("topic_weights are validated", {
  topics <- list(ai = "/ai", pricing = "/pricing")
  expect_error(
    topic_sensitive_pagerank(make_site(), topics, topic_weights = c(ai = 1)),
    "same names"
  )
  expect_error(
    topic_sensitive_pagerank(make_site(), topics, topic_weights = c(1, 2, 3)),
    "one weight per topic"
  )
  expect_error(
    topic_sensitive_pagerank(make_site(), topics, topic_weights = c(-1, 2)),
    "non-negative"
  )
  expect_error(
    topic_sensitive_pagerank(make_site(), topics, topic_weights = c(0, 0)),
    "positive value"
  )
})

test_that("passing prior args directly is an error", {
  topics <- list(ai = "/ai")
  prior <- data.frame(url = "/ai", weight = 1, stringsAsFactors = FALSE)
  expect_error(
    topic_sensitive_pagerank(make_site(), topics, prior_df = prior),
    "Do not pass"
  )
  expect_error(
    topic_sensitive_pagerank(make_site(), topics, prior_url_col = "u"),
    "Do not pass"
  )
})

test_that("an empty topic seed set is rejected", {
  expect_error(
    topic_sensitive_pagerank(make_site(), topics = list(ai = character(0))),
    "no usable seed URLs"
  )
})
