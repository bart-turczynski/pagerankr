describe("resolve_canonical_urls()", {
  it("resolves canonical chains and leaves unmapped URLs unchanged", {
    canonicals <- data.frame(
      from = c("A", "B"),
      to = c("B", "C"),
      stringsAsFactors = FALSE
    )

    result <- resolve_canonical_urls(c("A", "B", "X", NA), canonicals)

    expect_identical(result$original, c("A", "B", "X", NA))
    expect_identical(result$resolved, c("C", "C", "X", NA))
    expect_identical(result$changed, c(TRUE, TRUE, FALSE, FALSE))
    expect_identical(result$signal, c("canonical", "canonical", NA, NA))
  })

  it("treats self-canonicals as no-ops", {
    canonicals <- data.frame(
      from = c("A", "B"),
      to = c("A", "C"),
      stringsAsFactors = FALSE
    )

    result <- resolve_canonical_urls(c("A", "B"), canonicals)

    expect_identical(result$resolved, c("A", "C"))
    expect_identical(result$changed, c(FALSE, TRUE))
  })

  it("uses canonical duplicate and loop policies from build_fold_map", {
    duplicates <- data.frame(
      from = c("A", "A"),
      to = c("B", "C"),
      stringsAsFactors = FALSE
    )
    loop <- data.frame(
      from = c("A", "B"),
      to = c("B", "A"),
      stringsAsFactors = FALSE
    )

    expect_error(resolve_canonical_urls("A", duplicates), "Ambiguous")

    first <- resolve_canonical_urls(
      "A",
      duplicates,
      canonical_duplicate_from_policy = "first_wins"
    )
    expect_identical(first$resolved, "B")

    expect_error(resolve_canonical_urls("A", loop), "cycle")
    pruned <- resolve_canonical_urls(
      "A",
      loop,
      canonical_loop_handling = "prune_loop"
    )
    expect_identical(pruned$resolved, "A")
    expect_false(pruned$changed)
  })

  it("supports custom canonical column names", {
    canonicals <- data.frame(
      declared = c("A", "B"),
      canonical = c("B", "C"),
      stringsAsFactors = FALSE
    )

    result <- resolve_canonical_urls(
      "A",
      canonicals,
      canonical_from_col = "declared",
      canonical_to_col = "canonical"
    )

    expect_identical(result$resolved, "C")
  })
})

describe("resolve_canonicals()", {
  it("folds edge endpoints through canonical declarations", {
    edges <- data.frame(
      from = c("A", "X"),
      to = c("B", "Y"),
      weight = c(1, 2),
      stringsAsFactors = FALSE
    )
    canonicals <- data.frame(
      from = c("A", "B"),
      to = c("C", "D"),
      stringsAsFactors = FALSE
    )

    result <- resolve_canonicals(edges, canonicals)

    expect_identical(result$from, c("C", "X"))
    expect_identical(result$to, c("D", "Y"))
    expect_identical(result$weight, c(1, 2))
    expect_identical(
      attr(result, "fold_map")$signal,
      c("canonical", "canonical")
    )
  })

  it("supports custom edge and canonical columns", {
    edges <- data.frame(
      source_url = "A",
      target_url = "B",
      stringsAsFactors = FALSE
    )
    canonicals <- data.frame(
      declared = "B",
      canonical = "C",
      stringsAsFactors = FALSE
    )

    result <- resolve_canonicals(
      edges,
      canonicals,
      edge_from_col = "source_url",
      edge_to_col = "target_url",
      canonical_from_col = "declared",
      canonical_to_col = "canonical"
    )

    expect_identical(result$source_url, "A")
    expect_identical(result$target_url, "C")
  })
})

describe("resolve_folded_urls()", {
  it("resolves mixed canonical and redirect chains", {
    redirects <- data.frame(from = "B", to = "C", stringsAsFactors = FALSE)
    canonicals <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)

    result <- resolve_folded_urls(c("A", "B", "X"), redirects, canonicals)

    expect_identical(result$resolved, c("C", "C", "X"))
    expect_identical(result$changed, c(TRUE, TRUE, FALSE))
    expect_identical(result$signal, c("canonical", "redirect", NA))
  })

  it("matches applying build_fold_map manually", {
    redirects <- data.frame(
      from = c("B", "D"),
      to = c("C", "E"),
      stringsAsFactors = FALSE
    )
    canonicals <- data.frame(
      from = c("A", "X"),
      to = c("B", "D"),
      stringsAsFactors = FALSE
    )
    urls <- c("A", "B", "X", "Z")

    result <- resolve_folded_urls(urls, redirects, canonicals)
    fold_map <- build_fold_map(redirects, canonicals)
    manual <- pagerankr:::.apply_fold_map(
      urls,
      stats::setNames(fold_map$to, fold_map$from)
    )

    expect_identical(result$resolved, manual)
    expect_identical(attr(result, "fold_map"), fold_map)
  })

  it("uses canonical_conflict_policy for same-source conflicts", {
    redirects <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
    canonicals <- data.frame(from = "A", to = "D", stringsAsFactors = FALSE)

    redirect_wins <- resolve_folded_urls("A", redirects, canonicals)
    expect_identical(redirect_wins$resolved, "B")
    expect_identical(redirect_wins$signal, "redirect")
    expect_true(attr(redirect_wins, "conflicts")$disagrees)
    expect_identical(
      attr(redirect_wins, "ignored_canonicals")$canonical_to,
      "D"
    )

    canonical_wins <- resolve_folded_urls(
      "A",
      redirects,
      canonicals,
      canonical_conflict_policy = "canonical_wins"
    )
    expect_identical(canonical_wins$resolved, "D")
    expect_identical(canonical_wins$signal, "canonical")

    expect_error(
      resolve_folded_urls(
        "A",
        redirects,
        canonicals,
        canonical_conflict_policy = "error"
      ),
      "conflict"
    )
  })

  it("supports custom redirect and canonical columns", {
    redirects <- data.frame(
      old = "B",
      new = "C",
      stringsAsFactors = FALSE
    )
    canonicals <- data.frame(
      declared = "A",
      canonical = "B",
      stringsAsFactors = FALSE
    )

    result <- resolve_folded_urls(
      "A",
      redirects,
      canonicals,
      redirect_from_col = "old",
      redirect_to_col = "new",
      canonical_from_col = "declared",
      canonical_to_col = "canonical"
    )

    expect_identical(result$resolved, "C")
  })

  it("returns unchanged URLs when no signals are supplied", {
    result <- resolve_folded_urls(c("A", NA), NULL, NULL)

    expect_identical(result$resolved, c("A", NA))
    expect_identical(result$changed, c(FALSE, FALSE))
    expect_identical(result$signal, c(NA_character_, NA_character_))
    expect_equal(nrow(attr(result, "fold_map")), 0L)
  })
})
