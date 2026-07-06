context("audit_canonicals / audit_fold")

describe("audit_canonicals mirrors audit_redirects structure", {
  canonicals <- data.frame(
    from = c("A", "B", "C", "D", "D", "E"),
    to   = c("B", "C", "final", "X", "Y", "E")
  )

  it("returns a canonical_audit object with the expected fields", {
    audit <- audit_canonicals(canonicals)
    expect_s3_class(audit, "canonical_audit")
    expect_true(all(c(
      "n_rules", "n_self_refs", "self_refs", "n_conflicts", "conflicts",
      "n_loops", "loops", "chains", "max_chain_length"
    ) %in% names(audit)))
  })

  it("detects self-referencing canonicals", {
    audit <- audit_canonicals(canonicals)
    expect_equal(audit$n_self_refs, 1L) # the self-referencing row E
  })

  it("detects conflicting canonical sources", {
    audit <- audit_canonicals(canonicals)
    expect_equal(audit$n_conflicts, 1L) # source D has two distinct targets
  })

  it("reports chain lengths", {
    audit <- audit_canonicals(canonicals)
    expect_gte(audit$max_chain_length, 2L) # the multi-hop A chain
  })

  it("identifies orphaned canonicals against an edge list", {
    edges <- data.frame(from = "Z", to = "A")
    audit <- audit_canonicals(canonicals, edge_list_df = edges)
    expect_false(is.null(audit$orphaned_redirects))
    expect_gt(nrow(audit$orphaned_redirects), 0)
  })

  it("prints a canonical-specific report", {
    audit <- audit_canonicals(canonicals)
    expect_output(print(audit), "Canonical Audit Report")
  })

  it("validates the data frame argument", {
    expect_error(audit_canonicals("not a df"), "canonicals_df")
  })

  it("prints loops when canonical loops are present", {
    loop_canonicals <- data.frame(
      from = c("P", "Q", "R"),
      to   = c("Q", "R", "P")
    )
    audit <- audit_canonicals(loop_canonicals)
    expect_equal(audit$n_loops, 1)
    expect_output(print(audit), "Loops")
  })

  it("prints the orphaned-canonicals count when present", {
    edges <- data.frame(from = "Z", to = "A")
    audit <- audit_canonicals(canonicals, edge_list_df = edges)
    expect_output(print(audit), "Orphaned canonicals")
  })
})

describe("audit_redirects still works after the refactor", {
  redirects <- data.frame(
    from = c("A", "B", "C"), to = c("B", "C", "final")
  )
  it("keeps its class and report header", {
    audit <- audit_redirects(redirects)
    expect_s3_class(audit, "redirect_audit")
    expect_output(print(audit), "Redirect Audit Report")
  })
})

describe("audit_fold combined cross-signal view", {
  redirects <- data.frame(from = "A", to = "B")
  canonicals <- data.frame(from = "A", to = "D")

  it("exposes per-signal audits and the conflict tables", {
    af <- audit_fold(redirects, canonicals)
    expect_s3_class(af, "fold_audit")
    expect_s3_class(af$redirects, "redirect_audit")
    expect_s3_class(af$canonicals, "canonical_audit")
    expect_equal(af$conflicts$source, "A")
    expect_true(af$conflicts$disagrees)
    expect_equal(af$ignored_canonicals$source, "A")
  })

  it("reports disagreements under policy 'error' without aborting", {
    af <- audit_fold(redirects, canonicals, canonical_conflict_policy = "error")
    expect_true(af$conflicts$disagrees)
    expect_equal(af$conflict_policy, "error")
  })

  it("prints the combined report", {
    af <- audit_fold(redirects, canonicals)
    expect_output(print(af), "Combined Fold Audit")
  })

  it("works with only one signal supplied", {
    af <- audit_fold(redirects_df = redirects)
    expect_s3_class(af$redirects, "redirect_audit")
    expect_null(af$canonicals)
  })

  it("prints zero ignored canonicals when fold composition fails", {
    # A duplicate redirect source under the default "strict" policy makes
    # `.build_terminal_map()` error inside `.compose_fold_map()`; audit_fold()
    # catches this and leaves `conflicts`/`ignored_canonicals` at their
    # initial NULL values instead of aborting.
    dup_redirects <- data.frame(from = c("A", "A"), to = c("B", "C"))
    af <- audit_fold(redirects_df = dup_redirects)
    expect_null(af$ignored_canonicals)
    expect_output(print(af), "Ignored canonicals (source also redirects): 0",
      fixed = TRUE
    )
  })
})
