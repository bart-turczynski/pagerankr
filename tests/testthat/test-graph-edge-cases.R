# Deterministic fixtures for graph shapes whose convergence and mass
# diagnostics were unpinned: disconnected components, zero-weight edges, and
# the graph whose every weight is zero. Expected values are analytic where the
# graph is small enough to solve by hand, so a change in the teleport or sink
# model fails here with an arithmetic diff rather than a shrug.

test_that("disconnected closed components each hold their teleport share", {
  # Two components with no edge between them: a 2-cycle {A, B} and a 3-node
  # closed component {C, D, E}. Neither has a dangling node, so no mass can
  # cross the boundary: a closed component's stationary mass is exactly its
  # share of the uniform teleport vector, n_component / n_total.
  edges <- data.frame(
    from = c("A", "B", "C", "D", "E", "D"),
    to = c("B", "A", "D", "E", "C", "C")
  )
  res <- pagerank(edges)

  expect_identical(sort(res$node_name), c("A", "B", "C", "D", "E"))
  expect_equal(sum(res$pagerank), 1)

  comp1 <- res$pagerank[res$node_name %in% c("A", "B")]
  comp2 <- res$pagerank[res$node_name %in% c("C", "D", "E")]
  expect_equal(sum(comp1), 2 / 5)
  expect_equal(sum(comp2), 3 / 5)
  # {A, B} is symmetric, so the component's mass splits evenly.
  expect_equal(comp1, c(0.2, 0.2))

  conv <- attr(res, "convergence")
  expect_lt(conv$residual, 1e-8)
  expect_true(conv$tol_met)

  counts <- attr(res, "transition_audit")$counts
  expect_identical(counts$n_input_rows, 6L)
  expect_identical(counts$n_edges, 6L)
  expect_identical(counts$n_vertices, 5L)

  mass <- attr(res, "transition_audit")$mass
  expect_equal(mass$reported, 1)
  expect_equal(mass$sink, 0)
  expect_equal(mass$hidden, 0)

  # Determinism: the solver is not seeded, so a repeat run must be identical.
  expect_identical(pagerank(edges), res)
})

test_that("a dangling node moves mass across the component boundary", {
  # Same two components, but E is now dangling (no outgoing edge). Dangling
  # mass is redistributed over the whole teleport vector, not confined to the
  # component that leaked it, so {A, B} ends up ABOVE its 2/5 share. Pinned
  # because it is the one case where "disconnected" does not mean independent.
  edges <- data.frame(
    from = c("A", "B", "C", "D"),
    to = c("B", "A", "D", "E")
  )
  res <- pagerank(edges)

  expect_identical(sort(res$node_name), c("A", "B", "C", "D", "E"))
  expect_equal(sum(res$pagerank), 1)
  expect_gt(sum(res$pagerank[res$node_name %in% c("A", "B")]), 2 / 5)
  expect_equal(
    res$pagerank[match(c("A", "B"), res$node_name)],
    c(0.3554449726752, 0.3554449726752),
    tolerance = 1e-9
  )

  # The redistribution is igraph's, not the waste sink's: nothing evaporates.
  expect_equal(attr(res, "transition_audit")$mass$sink, 0)
  expect_lt(attr(res, "convergence")$residual, 1e-8)
})

test_that("both solvers agree on a disconnected graph", {
  edges <- data.frame(
    from = c("A", "B", "C", "D", "E", "D"),
    to = c("B", "A", "D", "E", "C", "C")
  )
  pr <- pagerank(edges)
  ar <- suppressMessages(pagerank(edges, eps = 1e-12, niter = 10000))

  m <- merge(pr, ar, by = "node_name", suffixes = c("_pr", "_ar"))
  expect_equal(m$pagerank_pr, m$pagerank_ar, tolerance = 1e-8)
})

test_that("a zero-weight edge delivers no authority to its target", {
  # A -> B carries weight 0; A -> C, B -> C and C -> A carry weight 1. B is
  # therefore reachable only through a zero-weight edge and collects nothing
  # but its teleport share, (1 - 0.85) / 3 = 0.05. Solving the remaining two
  # equations by hand gives A = 0.05 + 0.85 * C and C = 0.05 + 0.85 * (A + B),
  # hence A = 0.128625 / 0.2775.
  edges <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "C", "A"),
    w = c(0, 1, 1, 1)
  )
  res <- pagerank(edges, weight_col = "w")

  expect_identical(sort(res$node_name), c("A", "B", "C"))
  expect_equal(sum(res$pagerank), 1)
  expect_equal(
    res$pagerank[match(c("A", "B", "C"), res$node_name)],
    c(0.128625 / 0.2775, 0.05, 0.0925 + 0.85 * (0.128625 / 0.2775))
  )

  # A zero-weight edge and no edge at all are the same graph to the solver.
  dropped <- edges[edges$w > 0, , drop = FALSE]
  expect_equal(
    res$pagerank[order(res$node_name)],
    {
      d <- pagerank(dropped, weight_col = "w")
      d$pagerank[order(d$node_name)]
    }
  )

  expect_lt(attr(res, "convergence")$residual, 1e-8)
  expect_identical(attr(res, "transition_audit")$counts$n_vertices, 3L)
  expect_identical(pagerank(edges, weight_col = "w"), res)
})

test_that("a graph whose every weight is zero is rejected by name", {
  # Every source has all-zero outgoing weight, so there is no transition matrix
  # to normalize. This must fail as a named weight error, not as a division by
  # zero downstream.
  edges <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    w = c(0, 0, 0)
  )

  expect_error(
    pagerank(edges, weight_col = "w"),
    "3 source(s) with all-zero outgoing weights",
    fixed = TRUE
  )
  expect_error(
    compute_pagerank(edges, weight_col = "w"),
    "3 source(s) with all-zero outgoing weights",
    fixed = TRUE
  )

  # The same condition is reportable rather than fatal under `action = "none"`,
  # which is where the per-source detail the error message points at lives.
  report <- validate_edge_weights(edges, weight_col = "w", action = "none")
  expect_identical(sort(report$source), c("A", "B", "C"))
  expect_true(all(report$all_zero))
  expect_false(any(report$valid))
})
