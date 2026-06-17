context("build_fold_map / composed redirect + canonical folding")

describe("build_fold_map basic signals", {
  it("folds a cross-canonical (A canonical C) with signal 'canonical'", {
    fm <- build_fold_map(canonicals_df = data.frame(from = "A", to = "C"))
    expect_equal(fm$from, "A")
    expect_equal(fm$to, "C")
    expect_equal(fm$signal, "canonical")
  })

  it("drops self-canonicals as no-ops", {
    fm <- build_fold_map(canonicals_df = data.frame(from = "A", to = "A"))
    expect_equal(nrow(fm), 0)
  })

  it("folds a plain redirect with signal 'redirect'", {
    fm <- build_fold_map(redirects_df = data.frame(from = "A", to = "B"))
    expect_equal(fm$to, "B")
    expect_equal(fm$signal, "redirect")
  })

  it("returns an empty map when neither signal is supplied", {
    fm <- build_fold_map()
    expect_equal(nrow(fm), 0)
    expect_equal(names(fm), c("from", "to", "signal"))
  })

  it("resolves a canonical target that itself redirects", {
    # canonical A -> B, redirect B -> C  =>  A folds to C
    fm <- build_fold_map(
      redirects_df = data.frame(from = "B", to = "C"),
      canonicals_df = data.frame(from = "A", to = "B")
    )
    map <- stats::setNames(fm$to, fm$from)
    expect_equal(unname(map["A"]), "C")
    expect_equal(unname(map["B"]), "C")
    expect_equal(fm$signal[fm$from == "A"], "canonical")
    expect_equal(fm$signal[fm$from == "B"], "redirect")
  })
})

describe("canonical_conflict_policy on a redirecting source", {
  # A redirects to B, but A also declares canonical D (disagreement).
  redirects <- data.frame(from = "A", to = "B", stringsAsFactors = FALSE)
  canonicals <- data.frame(from = "A", to = "D", stringsAsFactors = FALSE)

  it("redirect_wins (default): redirect used, canonical ignored+flagged", {
    fm <- build_fold_map(
      redirects_df = redirects, canonicals_df = canonicals
    )
    map <- stats::setNames(fm$to, fm$from)
    expect_equal(unname(map["A"]), "B")
    expect_equal(fm$signal[fm$from == "A"], "redirect")

    conflicts <- attr(fm, "conflicts")
    expect_equal(conflicts$source, "A")
    expect_true(conflicts$disagrees)
    expect_equal(conflicts$resolution, "redirect")

    ignored <- attr(fm, "ignored_canonicals")
    expect_equal(ignored$source, "A")
    expect_equal(ignored$canonical_to, "D")
  })

  it("canonical_wins: canonical target used, still flagged", {
    fm <- build_fold_map(
      redirects_df = redirects, canonicals_df = canonicals,
      canonical_conflict_policy = "canonical_wins"
    )
    map <- stats::setNames(fm$to, fm$from)
    expect_equal(unname(map["A"]), "D")
    expect_equal(fm$signal[fm$from == "A"], "canonical")
    expect_equal(attr(fm, "conflicts")$resolution, "canonical")
  })

  it("error: aborts on genuine disagreement", {
    expect_error(
      build_fold_map(
        redirects_df = redirects, canonicals_df = canonicals,
        canonical_conflict_policy = "error"
      ),
      "conflict"
    )
  })

  it("error: does NOT abort when redirect and canonical agree", {
    # A redirects to B and also declares canonical B (resolves to same place).
    fm <- build_fold_map(
      redirects_df = data.frame(from = "A", to = "B"),
      canonicals_df = data.frame(from = "A", to = "B"),
      canonical_conflict_policy = "error"
    )
    expect_equal(stats::setNames(fm$to, fm$from)[["A"]], "B")
    expect_false(attr(fm, "conflicts")$disagrees)
  })

  it("flags a redirecting source whose canonical resolves to the same target", {
    # redirect A->B; redirect E->B; canonical A->E. canonical resolves to B,
    # which agrees with A's redirect target, so disagrees = FALSE but the
    # canonical on a redirecting source is still recorded.
    fm <- build_fold_map(
      redirects_df = data.frame(from = c("A", "E"), to = c("B", "B")),
      canonicals_df = data.frame(from = "A", to = "E")
    )
    conflicts <- attr(fm, "conflicts")
    expect_true("A" %in% conflicts$source)
    expect_false(conflicts$disagrees[conflicts$source == "A"])
    expect_true("A" %in% attr(fm, "ignored_canonicals")$source)
  })
})

describe("canonical_duplicate_from_policy (many canonicals, one source)", {
  dup <- data.frame(from = c("A", "A"), to = c("C1", "C2"))

  it("strict (default) errors on duplicate canonical targets", {
    expect_error(build_fold_map(canonicals_df = dup), "[Aa]mbiguous")
  })

  it("first_wins keeps the first canonical", {
    fm <- build_fold_map(
      canonicals_df = dup, canonical_duplicate_from_policy = "first_wins"
    )
    expect_equal(stats::setNames(fm$to, fm$from)[["A"]], "C1")
  })

  it("last_wins keeps the last canonical", {
    fm <- build_fold_map(
      canonicals_df = dup, canonical_duplicate_from_policy = "last_wins"
    )
    expect_equal(stats::setNames(fm$to, fm$from)[["A"]], "C2")
  })
})

describe("canonical_loop_handling (cycles among canonicals)", {
  loop <- data.frame(from = c("A", "B"), to = c("B", "A"))

  it("error (default) errors on a canonical loop", {
    expect_error(build_fold_map(canonicals_df = loop), "cycle")
  })

  it("prune_loop resolves despite the loop (URLs unfolded)", {
    fm <- build_fold_map(
      canonicals_df = loop, canonical_loop_handling = "prune_loop"
    )
    # Loop edges pruned => A and B map to themselves => no folds.
    expect_equal(nrow(fm), 0)
  })

  it("break_arrow keeps the highest in-degree node as sink", {
    fm <- build_fold_map(
      canonicals_df = loop, canonical_loop_handling = "break_arrow"
    )
    expect_lte(nrow(fm), 1)
  })

  it("uses canonical loop policy independently of redirect loop policy", {
    # Redirect loop would error under loop_handling='error', but there is no
    # redirect loop here; the canonical loop is broken by its own policy.
    fm <- build_fold_map(
      redirects_df = data.frame(from = "R1", to = "R2"),
      canonicals_df = loop,
      loop_handling = "error",
      canonical_loop_handling = "prune_loop"
    )
    expect_equal(stats::setNames(fm$to, fm$from)[["R1"]], "R2")
  })
})

describe("fold map idempotency", {
  it("applying the map to its own targets is a no-op", {
    fm <- build_fold_map(
      redirects_df = data.frame(from = c("A", "B"), to = c("B", "C")),
      canonicals_df = data.frame(from = "X", to = "A")
    )
    map <- stats::setNames(fm$to, fm$from)
    # Every target must not itself be a re-mappable key (terminal/idempotent).
    second_hop <- ifelse(fm$to %in% names(map), map[fm$to], fm$to)
    expect_equal(unname(second_hop), unname(fm$to))
  })
})
