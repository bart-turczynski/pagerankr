test_that("default solver attaches a near-zero-residual convergence object", {
  edges <- data.frame(
    from = c("A", "B", "C", "A", "D"),
    to = c("B", "C", "A", "C", "A")
  )
  res <- pagerank(edges)
  conv <- attr(res, "convergence")

  expect_s3_class(conv, "pagerank_convergence")
  expect_identical(conv$algo, "prpack")
  expect_true(is.na(conv$iters)) # PRPACK exposes no iteration count
  expect_lt(conv$residual, 1e-8) # direct solve is exact
  expect_true(conv$tol_met)
})

test_that("supplying eps or niter transparently switches to ARPACK", {
  edges <- data.frame(
    from = c("A", "B", "C", "A", "D"),
    to = c("B", "C", "A", "C", "A")
  )

  expect_message(
    res <- pagerank(edges, eps = 1e-8),
    "switching `algo` to \"arpack\""
  )
  conv <- attr(res, "convergence")
  expect_identical(conv$algo, "arpack")
  expect_false(is.na(conv$iters))
  expect_gte(conv$iters, 1L)
  expect_identical(conv$eps, 1e-8)
  expect_lt(conv$residual, 1e-8)
  expect_true(conv$tol_met)

  # niter alone also switches; explicit algo silences the message.
  expect_silent(res2 <- pagerank(edges, algo = "arpack", niter = 5000))
  expect_identical(attr(res2, "convergence")$algo, "arpack")
})

test_that("PRPACK and ARPACK agree on the ranking", {
  edges <- data.frame(
    from = c("A", "B", "C", "A", "D", "E", "B"),
    to = c("B", "C", "A", "C", "A", "A", "E")
  )
  pr <- pagerank(edges)
  ar <- suppressMessages(pagerank(edges, eps = 1e-10, niter = 10000))

  m <- merge(pr, ar, by = "node_name", suffixes = c("_pr", "_ar"))
  expect_equal(m$pagerank_pr, m$pagerank_ar, tolerance = 1e-6)
  # Identical ranking order.
  expect_identical(
    order(m$pagerank_pr, m$node_name),
    order(m$pagerank_ar, m$node_name)
  )
})

test_that("residual is solver-independent across weights, reverse and prior", {
  edges <- data.frame(
    from = c("A", "B", "C", "A", "D"),
    to = c("B", "C", "A", "C", "A"),
    w = c(2, 1, 3, 1, 1)
  )

  expect_lt(attr(pagerank(edges, reverse = TRUE), "convergence")$residual, 1e-8)
  expect_lt(
    attr(pagerank(edges, weight_col = "w"), "convergence")$residual,
    1e-8
  )
  prior <- data.frame(url = c("A", "C"), weight = c(10, 5))
  res <- pagerank(edges, prior_df = prior, prior_verbose = FALSE)
  expect_lt(attr(res, "convergence")$residual, 1e-8)
})

test_that("convergence controls validate their inputs", {
  edges <- data.frame(from = "A", to = "B")

  expect_error(pagerank(edges, eps = -1), "`eps` must be")
  expect_error(pagerank(edges, eps = c(1e-3, 1e-4)), "`eps` must be")
  expect_error(pagerank(edges, niter = 0), "`niter` must be")
  expect_error(pagerank(edges, niter = -5), "`niter` must be")
})

test_that("a fractional `niter` is rejected, not truncated", {
  # `as.integer(2.7)` is 2 -- an iteration cap the caller never asked for, and
  # silently three times smaller than the request.
  edges <- data.frame(from = "A", to = "B")

  expect_error(pagerank(edges, niter = 2.7), "`niter` must be")
  expect_error(compute_pagerank(edges, niter = 2.7), "`niter` must be")
  # A whole number stored as a double is still a valid integer count.
  expect_silent(compute_pagerank(edges, algo = "arpack", niter = 100))
})

test_that("non-finite and out-of-range convergence controls are named errors", {
  edges <- data.frame(from = "A", to = "B")

  for (bad in list(Inf, -Inf, NaN, NA_real_, NA_integer_)) {
    expect_error(pagerank(edges, niter = bad), "`niter` must be")
    expect_error(compute_pagerank(edges, niter = bad), "`niter` must be")
  }
  # Above the integer range `as.integer()` yields NA plus a coercion warning,
  # which would reach ARPACK as a missing cap.
  expect_error(pagerank(edges, niter = 1e10), "`niter` must be")

  for (bad in list(Inf, NaN, NA_real_)) {
    expect_error(pagerank(edges, eps = bad), "`eps` must be")
    expect_error(compute_pagerank(edges, eps = bad), "`eps` must be")
  }
})

test_that("both entry points name the error for non-finite `damping`", {
  # `NA_real_ < 0` is NA, so the old `||` chain handed the `if` an NA and R
  # raised "missing value where TRUE/FALSE needed" instead of this message.
  edges <- data.frame(from = "A", to = "B")
  msg <- "`damping` must be a single numeric value between 0 and 1."

  for (bad in list(NA_real_, NA_integer_, NaN, Inf, -Inf, NA)) {
    expect_error(pagerank(edges, damping = bad), msg, fixed = TRUE)
    expect_error(compute_pagerank(edges, damping = bad), msg, fixed = TRUE)
  }
})

test_that("empty graphs carry no convergence attribute", {
  empty <- data.frame(
    from = character(0), to = character(0)
  )
  res <- pagerank(empty)
  expect_null(attr(res, "convergence"))
})

test_that("compute_pagerank exposes the same convergence attribute", {
  edges <- data.frame(
    from = c("A", "B", "C"), to = c("B", "C", "A")
  )
  conv <- attr(compute_pagerank(edges), "convergence")
  expect_s3_class(conv, "pagerank_convergence")
  expect_lt(conv$residual, 1e-8)
})

test_that("print.pagerank_convergence is informative", {
  edges <- data.frame(from = c("A", "B"), to = c("B", "A"))
  conv <- attr(pagerank(edges), "convergence")
  out <- paste(capture.output(print(conv)), collapse = "\n")
  expect_match(out, "PageRank Convergence")
  expect_match(out, "Residual")
  expect_match(out, "prpack")
})
