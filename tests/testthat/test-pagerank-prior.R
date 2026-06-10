# Integration tests for the TIPR prior wired through pagerank().

make_edges <- function() {
  data.frame(
    from = c("a", "b", "c", "a", "c"),
    to   = c("b", "c", "a", "c", "b"),
    stringsAsFactors = FALSE
  )
}

test_that("a prior lifts the favored node above its uniform PageRank", {
  edges <- make_edges()
  prior <- data.frame(url = "b", weight = 1000, stringsAsFactors = FALSE)

  uni <- pagerank(edges, clean_edge_urls = FALSE)
  tipr <- suppressMessages(
    pagerank(edges, prior_df = prior, clean_edge_urls = FALSE)
  )

  b_uni <- uni$pagerank[uni$node_name == "b"]
  b_tipr <- tipr$pagerank[tipr$node_name == "b"]
  expect_gt(b_tipr, b_uni)
  expect_true("prior_weight" %in% names(tipr))
  expect_equal(sum(tipr$pagerank), 1, tolerance = 1e-8)
})

test_that("prior on a redirect SOURCE folds onto its target", {
  # T is a real vertex; S only exists as a redirect source.
  edges <- data.frame(
    from = c("a", "b", "t"),
    to   = c("t", "t", "a"),
    stringsAsFactors = FALSE
  )
  redirects <- data.frame(from = "s", to = "t", stringsAsFactors = FALSE)
  prior <- data.frame(url = "s", weight = 1000, stringsAsFactors = FALSE)

  res <- suppressMessages(pagerank(
    edges, redirects_df = redirects, prior_df = prior,
    clean_edge_urls = FALSE, clean_redirect_urls = FALSE
  ))

  # The folded authority lands on T, which should hold all the teleport mass.
  t_pw <- res$prior_weight[res$node_name == "t"]
  expect_equal(t_pw, 1)
  expect_false("s" %in% res$node_name)   # source folded away, not a vertex
})

test_that("without the redirect map the prior on a source is dropped (align-only)", {
  edges <- data.frame(from = c("a", "b"), to = c("t", "t"),
                      stringsAsFactors = FALSE)
  prior <- data.frame(url = "s", weight = 1000, stringsAsFactors = FALSE)

  # No redirects: 's' never folds onto a vertex -> unmatched -> uniform fallback.
  expect_warning(
    res <- pagerank(edges, prior_df = prior, clean_edge_urls = FALSE,
                    prior_verbose = TRUE),
    "matched no vertices"
  )
  # Fallback is uniform, so no single node dominates the teleport.
  expect_true(all(res$prior_weight > 0))
})

test_that("prior_inject_unmatched surfaces orphaned authority as an isolate", {
  edges <- make_edges()
  prior <- data.frame(url = c("b", "orphan"), weight = c(10, 90),
                      stringsAsFactors = FALSE)

  without <- suppressMessages(
    pagerank(edges, prior_df = prior, clean_edge_urls = FALSE)
  )
  withinj <- suppressMessages(
    pagerank(edges, prior_df = prior, clean_edge_urls = FALSE,
             prior_inject_unmatched = TRUE)
  )

  expect_false("orphan" %in% without$node_name)
  expect_true("orphan" %in% withinj$node_name)
  # The orphan carries the bulk of the teleport prior (90 of 100).
  orphan_pw <- withinj$prior_weight[withinj$node_name == "orphan"]
  expect_gt(orphan_pw, 0.5)
})

test_that("nofollow sink is excluded from teleport (no error, scores valid)", {
  edges <- data.frame(
    from = c("a", "a", "b"),
    to   = c("b", "c", "a"),
    nofollow = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  prior <- data.frame(url = c("a", "b", "c"), weight = c(1, 1, 1),
                      stringsAsFactors = FALSE)
  res <- suppressMessages(pagerank(
    edges, prior_df = prior, nofollow_col = "nofollow",
    nofollow_action = "evaporate", clean_edge_urls = FALSE
  ))
  expect_false("__pr_nofollow_sink__" %in% res$node_name)  # removed in output
  expect_true(all(res$prior_weight >= 0))
})

test_that("prior URLs are canonicalized with the same rurl settings as edges", {
  edges <- data.frame(
    from = c("https://x.com/a", "https://x.com/b"),
    to   = c("https://x.com/b", "https://x.com/a"),
    stringsAsFactors = FALSE
  )
  # Same logical page, differently cased SCHEME (rurl normalizes the scheme);
  # cleaning must bring the prior into the same namespace as the edges.
  prior <- data.frame(url = "HTTPS://x.com/a", weight = 100,
                      stringsAsFactors = FALSE)
  res <- suppressMessages(
    pagerank(edges, prior_df = prior, clean_edge_urls = TRUE)
  )
  # Some vertex received the full prior (the cleaned 'a' page).
  expect_equal(max(res$prior_weight), 1)
})
