edges <- data.frame(
  from = c("A", "B", "C", "A", "D"),
  to = c("B", "C", "A", "C", "A")
)

test_that("returns tidy frame keyed by (url, alpha) with documented cols", {
  alphas <- c(0.80, 0.85, 0.90)
  sens <- damping_sensitivity(edges, alphas = alphas, clean_edge_urls = FALSE)

  expect_named(sens, c(
    "url", "alpha", "score", "iters", "iters_estimate", "residual", "converged"
  ))
  # One block of rows per alpha; every node appears at every alpha.
  n_nodes <- length(unique(edges$from)) +
    length(setdiff(edges$to, edges$from))
  expect_identical(sort(unique(sens$alpha)), sort(alphas))
  expect_equal(as.numeric(table(sens$alpha)), rep(n_nodes, length(alphas)))

  # Scores sum to 1 within each alpha (uniform teleport, no leakage here).
  sums <- tapply(sens$score, sens$alpha, sum)
  expect_equal(as.numeric(sums), rep(1, length(alphas)), tolerance = 1e-8)
})

test_that("sorted by alpha ascending then score descending", {
  sens <- damping_sensitivity(
    edges,
    alphas = c(0.90, 0.75),
    clean_edge_urls = FALSE
  )
  expect_identical(sens$alpha, sort(sens$alpha))
  for (a in unique(sens$alpha)) {
    block <- sens$score[sens$alpha == a]
    expect_identical(block, sort(block, decreasing = TRUE))
  }
})

test_that("default solver leaves iters NA but populates iters_estimate", {
  sens <- damping_sensitivity(
    edges,
    alphas = c(0.85, 0.95),
    clean_edge_urls = FALSE
  )
  expect_true(all(is.na(sens$iters))) # PRPACK exposes no iteration count
  expect_true(all(is.finite(sens$iters_estimate)))
  # The estimate climbs as alpha approaches 1.
  est_85 <- unique(sens$iters_estimate[sens$alpha == 0.85])
  est_95 <- unique(sens$iters_estimate[sens$alpha == 0.95])
  expect_gt(est_95, est_85)
})

test_that("ARPACK solver populates the empirical iteration count", {
  sens <- suppressMessages(damping_sensitivity(
    edges,
    alphas = c(0.85, 0.95),
    algo = "arpack",
    clean_edge_urls = FALSE
  ))
  expect_true(all(!is.na(sens$iters)))
  expect_true(all(sens$iters >= 1L))
  expect_true(all(sens$converged))
})

test_that("convergence summary attribute has one row per alpha", {
  alphas <- c(0.75, 0.85, 0.95)
  sens <- damping_sensitivity(edges, alphas = alphas, clean_edge_urls = FALSE)
  summ <- attr(sens, "convergence")

  expect_s3_class(summ, "data.frame")
  expect_identical(summ$alpha, alphas)
  expect_named(summ, c(
    "alpha", "algo", "iters", "iters_estimate", "residual", "tol",
    "converged", "n_nodes"
  ))
  expect_true(all(summ$residual < 1e-8)) # direct solve is exact
})

test_that("forwards pagerank() arguments (e.g. reverse) through ...", {
  fwd <- damping_sensitivity(
    edges,
    alphas = 0.85,
    reverse = TRUE,
    clean_edge_urls = FALSE
  )
  ref <- pagerank(
    edges,
    damping = 0.85,
    reverse = TRUE,
    clean_edge_urls = FALSE
  )
  m <- merge(
    fwd[, c("url", "score")],
    data.frame(url = ref[[1]], ref_score = ref[[2]]),
    by = "url"
  )
  expect_equal(m$score, m$ref_score, tolerance = 1e-10)
})

test_that("duplicate alphas are dropped", {
  sens <- damping_sensitivity(
    edges,
    alphas = c(0.85, 0.85, 0.90),
    clean_edge_urls = FALSE
  )
  expect_identical(sort(unique(sens$alpha)), c(0.85, 0.90))
  expect_identical(nrow(attr(sens, "convergence")), 2L)
})

test_that("input validation", {
  expect_error(damping_sensitivity("nope"), "must be a data frame")
  expect_error(
    damping_sensitivity(edges, alphas = numeric(0)),
    "non-empty numeric"
  )
  expect_error(
    damping_sensitivity(edges, alphas = c(0.5, NA)),
    "no missing values"
  )
  expect_error(
    damping_sensitivity(edges, alphas = c(0.5, 1)),
    "strictly between 0 and 1"
  )
  expect_error(
    damping_sensitivity(edges, alphas = c(0, 0.5)),
    "strictly between 0 and 1"
  )
  expect_error(
    damping_sensitivity(edges, damping = 0.9),
    "Do not pass `damping`"
  )
})

test_that("empty graph yields an empty tidy frame with correct columns", {
  empty <- data.frame(
    from = character(0), to = character(0)
  )
  sens <- damping_sensitivity(empty, alphas = c(0.85, 0.95))
  expect_identical(nrow(sens), 0L)
  expect_named(sens, c(
    "url", "alpha", "score", "iters", "iters_estimate", "residual", "converged"
  ))
  # Summary still reports one row per alpha (with n_nodes = 0).
  expect_identical(attr(sens, "convergence")$n_nodes, c(0L, 0L))
})
