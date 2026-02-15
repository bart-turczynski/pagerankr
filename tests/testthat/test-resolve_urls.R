# Tests for resolve_urls()

describe("resolve_urls basic functionality", {
  it("resolves simple redirects", {
    redirects <- data.frame(
      from = c("A", "B"),
      to   = c("B", "Final"),
      stringsAsFactors = FALSE
    )
    result <- resolve_urls(c("A", "B"), redirects)
    expect_equal(result$resolved, c("Final", "Final"))
    expect_true(all(result$changed))
  })

  it("leaves non-redirected URLs unchanged", {
    redirects <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    result <- resolve_urls(c("A", "X", "Y"), redirects)
    expect_equal(result$resolved, c("B", "X", "Y"))
    expect_equal(result$changed, c(TRUE, FALSE, FALSE))
  })

  it("resolves multi-hop chains", {
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to   = c("B", "C", "Final"),
      stringsAsFactors = FALSE
    )
    result <- resolve_urls("A", redirects)
    expect_equal(result$resolved, "Final")
  })

  it("preserves NA inputs", {
    redirects <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    result <- resolve_urls(c("A", NA), redirects)
    expect_equal(result$resolved[1], "B")
    expect_true(is.na(result$resolved[2]))
  })

  it("handles empty URL vector", {
    redirects <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    result <- resolve_urls(character(0), redirects)
    expect_equal(nrow(result), 0)
  })

  it("handles empty redirects", {
    redirects <- data.frame(from = character(0), to = character(0),
                            stringsAsFactors = FALSE)
    result <- resolve_urls(c("A", "B"), redirects)
    expect_equal(result$resolved, c("A", "B"))
    expect_false(any(result$changed))
  })
})


describe("resolve_urls policy passthrough", {
  it("errors on conflicting redirects with strict policy", {
    redirects <- data.frame(
      from = c("A", "A"), to = c("B", "C"), stringsAsFactors = FALSE
    )
    expect_error(resolve_urls("A", redirects), "Ambiguous redirect")
  })

  it("works with first_wins for conflicts", {
    redirects <- data.frame(
      from = c("A", "A"), to = c("B", "C"), stringsAsFactors = FALSE
    )
    result <- resolve_urls("A", redirects,
                           duplicate_from_policy = "first_wins")
    expect_equal(result$resolved, "B")
  })

  it("errors on loops with error policy", {
    redirects <- data.frame(
      from = c("A", "B"), to = c("B", "A"), stringsAsFactors = FALSE
    )
    expect_error(resolve_urls("A", redirects), "Redirect cycle detected")
  })

  it("handles loops with prune_loop", {
    redirects <- data.frame(
      from = c("A", "B"), to = c("B", "A"), stringsAsFactors = FALSE
    )
    result <- resolve_urls("A", redirects, loop_handling = "prune_loop")
    expect_true(is.data.frame(result))
    # A stays as A since cycle is pruned
    expect_equal(result$resolved, "A")
  })
})


describe("resolve_urls input validation", {
  it("errors on non-character urls", {
    redirects <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    expect_error(resolve_urls(123, redirects), "must be a character vector")
  })

  it("errors on non-data-frame redirects", {
    expect_error(resolve_urls("A", "not a df"), "must be a data frame")
  })

  it("errors on missing columns", {
    redirects <- data.frame(x = "A", y = "B", stringsAsFactors = FALSE)
    expect_error(resolve_urls("A", redirects), "must have")
  })
})


describe("resolve_urls edge cases with empty results after preprocessing", {
  it("returns unchanged when all redirects are self-referencing", {
    redirects <- data.frame(
      from = c("A", "B"),
      to   = c("A", "B"),
      stringsAsFactors = FALSE
    )
    result <- resolve_urls(c("A", "B"), redirects)
    expect_equal(result$resolved, c("A", "B"))
    expect_false(any(result$changed))
  })

  it("returns unchanged when preprocessing prunes all conflicts", {
    # prune_source removes ALL redirects from conflicting sources
    redirects <- data.frame(
      from = c("A", "A"),
      to   = c("B", "C"),
      stringsAsFactors = FALSE
    )
    result <- resolve_urls("A", redirects, duplicate_from_policy = "prune_source")
    expect_equal(result$resolved, "A")
    expect_false(result$changed)
  })
})


describe("resolve_urls self-referencing handling", {
  it("filters self-referencing redirects silently", {
    redirects <- data.frame(
      from = c("A", "B"),
      to   = c("A", "C"),
      stringsAsFactors = FALSE
    )
    result <- resolve_urls(c("A", "B"), redirects)
    # A -> A is self-ref (filtered), so A stays as A
    expect_equal(result$resolved[1], "A")
    # B -> C works normally
    expect_equal(result$resolved[2], "C")
  })
})
