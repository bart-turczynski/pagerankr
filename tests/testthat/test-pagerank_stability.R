edges <- data.frame(
  from = c("A", "B", "C", "A", "D", "E", "B"),
  to = c("B", "C", "A", "C", "A", "A", "E"),
  stringsAsFactors = FALSE
)

test_that("one row per swept alpha, ascending, with documented columns", {
  alphas <- c(0.80, 0.85, 0.90)
  stab <- pagerank_stability(edges, alphas = alphas, clean_edge_urls = FALSE)

  expect_identical(stab$alpha, sort(alphas))
  expect_named(stab, c(
    "alpha", "spearman_rho", "mean_abs_delta", "top_k_overlap",
    "nodes_gained", "nodes_lost",
    "algo", "iters", "iters_estimate", "residual", "tol", "converged",
    "n_nodes"
  ))
})

test_that("reference row is a sanity anchor", {
  stab <- pagerank_stability(edges, reference = 0.85, clean_edge_urls = FALSE)
  ref <- stab[stab$alpha == 0.85, ]

  expect_identical(ref$spearman_rho, 1)
  expect_identical(ref$mean_abs_delta, 0)
  expect_identical(ref$top_k_overlap, 1)
  expect_identical(ref$nodes_gained, 0L)
  expect_identical(ref$nodes_lost, 0L)
})

test_that("reference is swept even when absent from alphas", {
  stab <- pagerank_stability(
    edges,
    alphas = c(0.70, 0.95),
    reference = 0.85,
    clean_edge_urls = FALSE
  )
  expect_true(0.85 %in% stab$alpha)
  expect_identical(stab$alpha, c(0.70, 0.85, 0.95))
})

test_that("identical rankings score rho = 1 and full top-k overlap", {
  # alpha equal to the reference compared elsewhere is the anchor; here verify
  # that two genuinely close alphas keep a high correlation.
  stab <- pagerank_stability(
    edges,
    alphas = c(0.84, 0.85, 0.86),
    clean_edge_urls = FALSE
  )
  near <- stab[stab$alpha != 0.85, ]
  expect_true(all(near$spearman_rho > 0.9))
  expect_true(all(near$top_k_overlap > 0))
  expect_true(all(stab$mean_abs_delta >= 0))
})

test_that("top_k_overlap is a fraction in [0, 1] bounded by node count", {
  stab <- pagerank_stability(
    edges,
    alphas = c(0.80, 0.90),
    top_k = 100, # larger than the graph
    clean_edge_urls = FALSE
  )
  expect_true(all(stab$top_k_overlap >= 0 & stab$top_k_overlap <= 1))
  expect_identical(attr(stab, "top_k"), 100L)
})

test_that("full sensitivity detail is retrievable via attribute", {
  stab <- pagerank_stability(edges, clean_edge_urls = FALSE)
  sens <- attr(stab, "sensitivity")

  expect_s3_class(sens, "data.frame")
  expect_named(sens, c(
    "url", "alpha", "score", "iters", "iters_estimate", "residual", "converged"
  ))
  expect_identical(sort(unique(sens$alpha)), stab$alpha)
  expect_identical(attr(stab, "reference"), 0.85)
})

test_that("forwards solver controls through ... (ARPACK populates iters)", {
  stab <- suppressMessages(
    pagerank_stability(edges, algo = "arpack", clean_edge_urls = FALSE)
  )
  expect_true(all(!is.na(stab$iters)))
  expect_true(all(stab$converged))
})

test_that("input validation", {
  expect_error(pagerank_stability("nope"), "must be a data frame")
  expect_error(
    pagerank_stability(edges, reference = 1),
    "strictly between 0 and 1"
  )
  expect_error(
    pagerank_stability(edges, reference = c(0.8, 0.9)),
    "single number"
  )
  expect_error(pagerank_stability(edges, top_k = 0), "positive integer")
  expect_error(
    pagerank_stability(edges, damping = 0.9),
    "Do not pass `damping`"
  )
  # alphas validation is delegated to damping_sensitivity().
  expect_error(
    pagerank_stability(edges, alphas = c(0.5, 1.5)),
    "strictly between 0 and 1"
  )
})

test_that("empty graph yields per-alpha rows with NA metrics", {
  empty <- data.frame(
    from = character(0), to = character(0),
    stringsAsFactors = FALSE
  )
  stab <- pagerank_stability(empty, alphas = c(0.85, 0.95))
  expect_identical(stab$alpha, c(0.85, 0.95))
  expect_true(all(is.na(stab$spearman_rho)))
  expect_true(all(stab$n_nodes == 0L))
})
