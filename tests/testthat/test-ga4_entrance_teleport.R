test_that("returns a prior_df-shaped data frame (url, weight) by default", {
  ga4 <- data.frame(
    url = c("https://x/a", "https://x/b"),
    entrances = c(60, 40)
  )
  out <- ga4_entrance_teleport(ga4)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("url", "weight"))
  expect_equal(out$weight[out$url == "https://x/a"], 60)
  expect_equal(out$weight[out$url == "https://x/b"], 40)
})

test_that("entrances are additive: duplicate URLs are summed", {
  ga4 <- data.frame(
    url = c("https://x/a", "https://x/a", "https://x/b"),
    entrances = c(60, 30, 10)
  )
  out <- ga4_entrance_teleport(ga4)
  expect_equal(out$weight[out$url == "https://x/a"], 90) # sum of 60 and 30
  expect_equal(out$weight[out$url == "https://x/b"], 10)
})

test_that("missing and negative entrance rows are dropped", {
  ga4 <- data.frame(
    url = c("https://x/a", "https://x/b", NA, "https://x/c"),
    entrances = c(10, -5, 99, NA)
  )
  out <- ga4_entrance_teleport(ga4)
  expect_equal(out$url, "https://x/a")
  expect_equal(out$weight, 10)
})

test_that("with vertex_names, returns a teleport vector aligned to that set", {
  ga4 <- data.frame(
    url = c("https://x/a", "https://x/b"),
    entrances = c(900, 100)
  )
  v <- c("https://x/a", "https://x/b", "https://x/c")
  p <- ga4_entrance_teleport(ga4, vertex_names = v, verbose = FALSE)

  expect_length(p, length(v))
  expect_equal(sum(p), 1)
  expect_equal(p[1] / p[2], 9) # 900:100 entrances
  expect_equal(p[3], 0) # no entrances, alpha = 0
})

test_that("UNIFORM entrances recover the uniform teleport vector", {
  # Acceptance: when entrances are uniform across the (real) vertices, the
  # entrance-biased reset degrades to the standard uniform PageRank teleport.
  v <- c("https://x/a", "https://x/b", "https://x/c", "https://x/d")
  ga4 <- data.frame(
    url = v,
    entrances = c(50, 50, 50, 50)
  )
  p <- ga4_entrance_teleport(ga4, vertex_names = v, verbose = FALSE)
  expect_equal(p, rep(1 / length(v), length(v)))
})

test_that("uniform recovery holds for any common positive count", {
  v <- c("a", "b", "c")
  ga4 <- data.frame(url = v, entrances = c(7, 7, 7))
  p <- ga4_entrance_teleport(ga4, vertex_names = v, verbose = FALSE)
  expect_equal(p, rep(1 / 3, 3))
})

test_that("alpha = 1 forces uniform teleport regardless of entrance skew", {
  v <- c("a", "b", "c", "d")
  ga4 <- data.frame(url = "a", entrances = 999)
  p <- ga4_entrance_teleport(ga4, vertex_names = v, alpha = 1, verbose = FALSE)
  expect_equal(p, rep(0.25, 4))
})

test_that("excluded synthetic nodes receive zero teleport mass", {
  v <- c("a", "b", "__pr_waste_sink__")
  ga4 <- data.frame(
    url = c("a", "b"), entrances = c(50, 50)
  )
  p <- ga4_entrance_teleport(ga4,
    vertex_names = v,
    exclude_nodes = "__pr_waste_sink__", verbose = FALSE
  )
  expect_equal(p[3], 0)
  expect_equal(sum(p), 1)
})

test_that("custom column names are honored", {
  ga4 <- data.frame(
    landing_page = c("https://x/a", "https://x/b"),
    sessions = c(3, 1)
  )
  out <- ga4_entrance_teleport(ga4,
    url_col = "landing_page",
    entrances_col = "sessions"
  )
  expect_named(out, c("url", "weight"))
  expect_equal(out$weight[out$url == "https://x/a"], 3)
})

test_that("output feeds pagerank() as an entrance-biased prior_df", {
  # Integration: the adapter output is a valid prior_df for pagerank(), and a
  # skewed entrance reset shifts mass toward the high-entrance landing page
  # relative to uniform teleport.
  edges <- data.frame(
    from = c("https://x/a", "https://x/b", "https://x/c"),
    to = c("https://x/b", "https://x/c", "https://x/a")
  )
  ga4 <- data.frame(
    url = c("https://x/a", "https://x/b", "https://x/c"),
    entrances = c(1000, 1, 1)
  )
  tp <- ga4_entrance_teleport(ga4)

  base <- suppressMessages(pagerank(edges, prior_verbose = FALSE))
  biased <- suppressMessages(
    pagerank(edges, prior_df = tp, prior_alpha = 0.15, prior_verbose = FALSE)
  )

  pick <- function(res, u) res$pagerank[res$node_name == u]
  expect_gt(pick(biased, "https://x/a"), pick(base, "https://x/a"))
})

test_that("empty entrances + vertex_names falls back to uniform with warning", {
  v <- c("a", "b")
  ga4 <- data.frame(
    url = character(0), entrances = numeric(0)
  )
  expect_warning(
    p <- ga4_entrance_teleport(ga4, vertex_names = v, verbose = TRUE),
    "matched no vertices"
  )
  expect_equal(p, c(0.5, 0.5))
})

test_that("validation rejects non-data-frame and missing columns", {
  expect_error(
    ga4_entrance_teleport(list(url = "a", entrances = 1)),
    "must be a data frame"
  )
  expect_error(
    ga4_entrance_teleport(data.frame(u = "a", entrances = 1)),
    "must have"
  )
})
