test_that("linear share is proportional to weight and sums to 1", {
  v <- c("a", "b", "c")
  prior <- data.frame(url = c("a", "b"), weight = c(900, 100),
                      stringsAsFactors = FALSE)
  p <- align_prior_to_vertices(v, prior, verbose = FALSE)

  expect_equal(sum(p), 1)
  expect_equal(p[1] / p[2], 9)   # 900:100
  expect_equal(p[3], 0)          # c has no authority, alpha = 0
})

test_that("multiple rows for the same URL are summed (raw, additive)", {
  v <- c("a", "b")
  prior <- data.frame(url = c("a", "a", "b"), weight = c(600, 300, 100),
                      stringsAsFactors = FALSE)
  p <- align_prior_to_vertices(v, prior, verbose = FALSE)
  expect_equal(p[1] / p[2], 9)   # (600+300):100
})

test_that("excluded (synthetic) nodes get exactly zero in both components", {
  v <- c("a", "b", "__pr_nofollow_sink__")
  prior <- data.frame(url = c("a", "b"), weight = c(50, 50),
                      stringsAsFactors = FALSE)
  p_auth <- align_prior_to_vertices(v, prior,
                                    exclude_nodes = "__pr_nofollow_sink__",
                                    verbose = FALSE)
  expect_equal(p_auth[3], 0)
  expect_equal(sum(p_auth), 1)

  # Even under a pure-uniform mixture the sink stays at zero.
  p_uni <- align_prior_to_vertices(v, prior, alpha = 1,
                                   exclude_nodes = "__pr_nofollow_sink__",
                                   verbose = FALSE)
  expect_equal(p_uni[3], 0)
  expect_equal(unname(p_uni[1]), 0.5)
  expect_equal(unname(p_uni[2]), 0.5)
})

test_that("alpha=1 reproduces uniform teleport over real vertices", {
  v <- c("a", "b", "c", "d")
  prior <- data.frame(url = "a", weight = 999, stringsAsFactors = FALSE)
  p <- align_prior_to_vertices(v, prior, alpha = 1, verbose = FALSE)
  expect_equal(p, rep(0.25, 4))
})

test_that("alpha mixes uniform and authority", {
  v <- c("a", "b")
  prior <- data.frame(url = "a", weight = 100, stringsAsFactors = FALSE)
  # p = 0.5*uniform(0.5,0.5) + 0.5*authority(1,0) = (0.75, 0.25)
  p <- align_prior_to_vertices(v, prior, alpha = 0.5, verbose = FALSE)
  expect_equal(unname(p), c(0.75, 0.25))
})

test_that("log transform compresses the dynamic range vs linear", {
  v <- c("a", "b")
  prior <- data.frame(url = c("a", "b"), weight = c(1000, 1),
                      stringsAsFactors = FALSE)
  lin <- align_prior_to_vertices(v, prior, transform = "none", verbose = FALSE)
  lg  <- align_prior_to_vertices(v, prior, transform = "log", verbose = FALSE)
  expect_gt(lin[1] / lin[2], lg[1] / lg[2])   # linear far more concentrated
})

test_that("vertices absent from the prior get zero under alpha=0", {
  v <- c("a", "b", "c")
  prior <- data.frame(url = "a", weight = 10, stringsAsFactors = FALSE)
  p <- align_prior_to_vertices(v, prior, verbose = FALSE)
  expect_equal(p[2], 0)
  expect_equal(p[3], 0)
  expect_equal(p[1], 1)
})

test_that("a fully-unmatched prior falls back to uniform with a warning", {
  v <- c("a", "b")
  prior <- data.frame(url = "zzz", weight = 5, stringsAsFactors = FALSE)
  expect_warning(
    p <- align_prior_to_vertices(v, prior, verbose = TRUE),
    "matched no vertices"
  )
  expect_equal(p, c(0.5, 0.5))
})

test_that("empty vertex set returns numeric(0)", {
  expect_equal(
    align_prior_to_vertices(character(0),
                            data.frame(url = "a", weight = 1),
                            verbose = FALSE),
    numeric(0)
  )
})

test_that("validation rejects bad alpha and missing columns", {
  v <- c("a", "b")
  expect_error(
    align_prior_to_vertices(v, data.frame(url = "a", weight = 1), alpha = 2,
                            verbose = FALSE),
    "alpha"
  )
  expect_error(
    align_prior_to_vertices(v, data.frame(u = "a", weight = 1),
                            verbose = FALSE),
    "must have"
  )
})

test_that("coverage diagnostics report dropped (unmatched) prior weight", {
  v <- c("a")
  prior <- data.frame(url = c("a", "gone"), weight = c(10, 40),
                      stringsAsFactors = FALSE)
  expect_message(
    align_prior_to_vertices(v, prior, verbose = TRUE),
    "1 prior URL\\(s\\) \\(sum weight 40\\)"
  )
})
