test_that("linear share is proportional to weight and sums to 1", {
  v <- c("a", "b", "c")
  prior <- data.frame(
    url = c("a", "b"), weight = c(900, 100)
  )
  p <- align_prior_to_vertices(v, prior, verbose = FALSE)

  expect_equal(sum(p), 1)
  expect_equal(p[1] / p[2], 9) # 900:100
  expect_equal(p[3], 0) # c has no authority, alpha = 0
})

test_that("multiple rows for the same URL are summed (raw, additive)", {
  v <- c("a", "b")
  prior <- data.frame(
    url = c("a", "a", "b"), weight = c(600, 300, 100)
  )
  p <- align_prior_to_vertices(v, prior, verbose = FALSE)
  expect_equal(p[1] / p[2], 9) # ratio of 600 plus 300 to 100
})

test_that("excluded (synthetic) nodes get exactly zero in both components", {
  v <- c("a", "b", "__pr_nofollow_sink__")
  prior <- data.frame(
    url = c("a", "b"), weight = c(50, 50)
  )
  p_auth <- align_prior_to_vertices(v, prior,
    exclude_nodes = "__pr_nofollow_sink__",
    verbose = FALSE
  )
  expect_equal(p_auth[3], 0)
  expect_equal(sum(p_auth), 1)

  # Even under a pure-uniform mixture the sink stays at zero.
  p_uni <- align_prior_to_vertices(v, prior,
    alpha = 1,
    exclude_nodes = "__pr_nofollow_sink__",
    verbose = FALSE
  )
  expect_equal(p_uni[3], 0)
  expect_equal(unname(p_uni[1]), 0.5)
  expect_equal(unname(p_uni[2]), 0.5)
})

test_that("alpha=1 reproduces uniform teleport over real vertices", {
  v <- c("a", "b", "c", "d")
  prior <- data.frame(url = "a", weight = 999)
  p <- align_prior_to_vertices(v, prior, alpha = 1, verbose = FALSE)
  expect_equal(p, rep(0.25, 4))
})

test_that("alpha mixes uniform and authority", {
  v <- c("a", "b")
  prior <- data.frame(url = "a", weight = 100)
  # p = 0.5*uniform(0.5,0.5) + 0.5*authority(1,0) = (0.75, 0.25)
  p <- align_prior_to_vertices(v, prior, alpha = 0.5, verbose = FALSE)
  expect_equal(unname(p), c(0.75, 0.25))
})

test_that("log transform compresses the dynamic range vs linear", {
  v <- c("a", "b")
  prior <- data.frame(
    url = c("a", "b"), weight = c(1000, 1)
  )
  lin <- align_prior_to_vertices(v, prior, transform = "none", verbose = FALSE)
  lg <- align_prior_to_vertices(v, prior, transform = "log", verbose = FALSE)
  expect_gt(lin[1] / lin[2], lg[1] / lg[2]) # linear far more concentrated
})

test_that("vertices absent from the prior get zero under alpha=0", {
  v <- c("a", "b", "c")
  prior <- data.frame(url = "a", weight = 10)
  p <- align_prior_to_vertices(v, prior, verbose = FALSE)
  expect_equal(p[2], 0)
  expect_equal(p[3], 0)
  expect_equal(p[1], 1)
})

test_that("a fully-unmatched prior falls back to uniform with a warning", {
  v <- c("a", "b")
  prior <- data.frame(url = "zzz", weight = 5)
  expect_warning(
    p <- align_prior_to_vertices(v, prior, verbose = TRUE),
    "matched no vertices"
  )
  expect_equal(p, c(0.5, 0.5))
})

test_that("alternative count metric is swappable via prior_weight_col", {
  # Source-agnostic contract: any additive raw count is a drop-in metric swap.
  # Same URLs, two count columns (e.g. referring domains vs links-to-target);
  # selecting one with prior_weight_col yields that metric's authority share.
  v <- c("a", "b", "c")
  prior <- data.frame(
    url = c("a", "b", "c"),
    ref_domains = c(900, 100, 0),
    links_to_target = c(100, 100, 800)
  )

  rd <- align_prior_to_vertices(v, prior,
    prior_weight_col = "ref_domains",
    verbose = FALSE
  )
  lt <- align_prior_to_vertices(v, prior,
    prior_weight_col = "links_to_target",
    verbose = FALSE
  )

  expect_equal(rd[1] / rd[2], 9) # 900:100 referring domains
  expect_equal(rd[3], 0) # no referring domains
  expect_equal(unname(lt), c(0.1, 0.1, 0.8)) # links-to-target share
})

test_that("swapped count metric still folds duplicate URLs by summation", {
  # The additive-count contract holds for whichever count column is selected.
  v <- c("a", "b")
  prior <- data.frame(
    url = c("a", "a", "b"),
    links_to_target = c(40, 20, 30)
  )
  p <- align_prior_to_vertices(v, prior,
    prior_weight_col = "links_to_target",
    verbose = FALSE
  )
  expect_equal(p[1] / p[2], 2) # ratio of 40 plus 20 to 30
})

test_that("empty vertex set returns numeric(0)", {
  expect_equal(
    align_prior_to_vertices(character(0),
      data.frame(url = "a", weight = 1),
      verbose = FALSE
    ),
    numeric(0)
  )
})

test_that("validation rejects bad alpha and missing columns", {
  v <- c("a", "b")
  expect_error(
    align_prior_to_vertices(v, data.frame(url = "a", weight = 1),
      alpha = 2,
      verbose = FALSE
    ),
    "alpha"
  )
  expect_error(
    align_prior_to_vertices(v, data.frame(u = "a", weight = 1),
      verbose = FALSE
    ),
    "must have"
  )
})

test_that("coverage diagnostics report dropped (unmatched) prior weight", {
  v <- c("a")
  prior <- data.frame(
    url = c("a", "gone"), weight = c(10, 40)
  )
  expect_message(
    align_prior_to_vertices(v, prior, verbose = TRUE),
    "1 prior URL\\(s\\) \\(sum weight 40\\)"
  )
})
