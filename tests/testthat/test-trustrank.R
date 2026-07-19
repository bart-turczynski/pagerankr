# Tests for TrustRank seed-biased PageRank: seed_prior() + trustrank().

# A site with a trusted core (/, /hub) and an untrusted region (/spam, /sink)
# that is only reachable far from the seeds.
make_trust_site <- function() {
  data.frame(
    from = c("/", "/", "/hub", "/hub", "/good", "/spam", "/spam"),
    to = c("/hub", "/good", "/good", "/deep", "/hub", "/sink", "/good")
  )
}

run_tr <- function(...) suppressMessages(trustrank(...))

# --- seed_prior() helper ---

test_that("seed_prior() builds a uniform-weight prior_df from a vector", {
  prior <- seed_prior(c("/", "/hub"))
  expect_named(prior, c("url", "weight"))
  expect_identical(prior$url, c("/", "/hub"))
  expect_equal(prior$weight, c(1, 1))
})

test_that("seed_prior() honors scalar and per-seed weights", {
  expect_equal(seed_prior(c("a", "b"), seed_weight = 5)$weight, c(5, 5))
  expect_equal(
    seed_prior(c("a", "b"), seed_weight = c(3, 1))$weight,
    c(3, 1)
  )
  expect_error(
    seed_prior(c("a", "b"), seed_weight = c(1, 2, 3)),
    "length 1 or match"
  )
})

test_that("seed_prior() accepts a data frame with custom columns", {
  seeds <- data.frame(
    page = c("/", "/hub"), trust = c(2, 1)
  )
  prior <- seed_prior(
    seeds,
    seed_url_col = "page", seed_weight_col = "trust"
  )
  expect_named(prior, c("url", "weight"))
  expect_equal(prior$weight, c(2, 1))
})

test_that("seed_prior() validates input", {
  expect_error(seed_prior(character(0)), "no usable seed URLs")
  expect_error(seed_prior(c("a", "b"), seed_weight = c(-1, 2)),
    "non-negative"
  )
  expect_error(
    seed_prior(data.frame(x = 1)),
    "must have 'url' and 'weight'"
  )
  expect_error(
    seed_prior(data.frame(url = "a", weight = 1), seed_weight = 1),
    "applies only when"
  )
  expect_error(
    seed_prior(42L),
    "character vector of URLs or a data frame"
  )
})

# --- Acceptance: seeds align/fold to vertices; trust flows from seeds ---

test_that("seed URLs align onto graph vertices and carry teleport weight", {
  edges <- make_trust_site()
  tr <- run_tr(edges, c("/", "/hub"), clean_edge_urls = FALSE)

  expect_true(all(c("/", "/hub") %in% tr$node_name))
  # The prior path attaches a per-vertex teleport weight; seeds carry it.
  expect_true("prior_weight" %in% names(tr))
  seed_pw <- tr$prior_weight[tr$node_name %in% c("/", "/hub")]
  expect_true(all(seed_pw > 0))
  # Untrusted, seed-unreachable pages get no teleport mass.
  expect_equal(tr$prior_weight[tr$node_name == "/spam"], 0)
})

test_that("a non-seed page reachable from seeds receives trust", {
  edges <- make_trust_site()
  tr <- run_tr(edges, c("/", "/hub"), clean_edge_urls = FALSE)

  # /deep is not a seed but is linked directly from the trusted /hub.
  deep <- tr$pagerank[tr$node_name == "/deep"]
  spam <- tr$pagerank[tr$node_name == "/spam"]
  expect_gt(deep, 0)
  # /spam is outside the trusted neighborhood -> less trust than /deep.
  expect_gt(deep, spam)
})

test_that("seed bias lifts the trusted core above uniform PageRank", {
  edges <- make_trust_site()
  uni <- pagerank(edges, clean_edge_urls = FALSE)
  tr <- run_tr(edges, c("/", "/hub"), clean_edge_urls = FALSE)

  hub_uni <- uni$pagerank[uni$node_name == "/hub"]
  hub_tr <- tr$pagerank[tr$node_name == "/hub"]
  expect_gt(hub_tr, hub_uni)
})

test_that("trustrank() equals pagerank() with the equivalent seed prior", {
  edges <- make_trust_site()
  prior <- seed_prior(c("/", "/hub"))
  direct <- suppressMessages(
    pagerank(edges, prior_df = prior, clean_edge_urls = FALSE)
  )
  tr <- run_tr(edges, c("/", "/hub"), clean_edge_urls = FALSE)
  expect_equal(tr$pagerank, direct$pagerank, tolerance = 1e-12)
})

test_that("trusted seeds on redirect sources fold onto their targets", {
  # S only exists as a redirect source; it should fold to T and seed T.
  edges <- data.frame(
    from = c("a", "b", "t"), to = c("t", "t", "a")
  )
  redirects <- data.frame(from = "s", to = "t")
  tr <- run_tr(
    edges, "s",
    redirects_df = redirects,
    clean_edge_urls = FALSE, clean_redirect_urls = FALSE
  )
  expect_false("s" %in% tr$node_name)
  expect_gt(tr$prior_weight[tr$node_name == "t"], 0)
})

test_that("trustrank() rejects caller-supplied prior args", {
  edges <- make_trust_site()
  prior <- seed_prior("/")
  expect_error(
    trustrank(edges, "/", prior_df = prior),
    "Do not pass"
  )
  expect_error(
    trustrank(edges, "/", prior_weight_col = "w"),
    "Do not pass"
  )
})

test_that("prior_alpha gives untrusted pages a teleport floor", {
  edges <- make_trust_site()
  tr0 <- run_tr(edges, c("/", "/hub"), clean_edge_urls = FALSE)
  tr_floor <- run_tr(
    edges, c("/", "/hub"),
    prior_alpha = 0.2, clean_edge_urls = FALSE
  )
  expect_equal(tr0$prior_weight[tr0$node_name == "/spam"], 0)
  expect_gt(tr_floor$prior_weight[tr_floor$node_name == "/spam"], 0)
})
