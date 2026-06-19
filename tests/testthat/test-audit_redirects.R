# Tests for audit_redirects()

describe("audit_redirects basic functionality", {
  it("reports chain lengths correctly", {
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "Final")
    )
    audit <- audit_redirects(redirects)
    expect_equal(audit$n_rules, 3)
    expect_equal(audit$max_chain_length, 3)
    # A has the longest chain (A->B->C->Final = 3 hops)
    a_row <- audit$chains[audit$chains$from == "A", ]
    expect_equal(a_row$chain_length, 3)
    expect_equal(a_row$to_final, "Final")
  })

  it("detects self-referencing redirects", {
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("A", "X", "Y")
    )
    audit <- audit_redirects(redirects)
    expect_equal(audit$n_self_refs, 1)
    expect_equal(audit$self_refs$from, "A")
  })

  it("detects conflicting sources", {
    redirects <- data.frame(
      from = c("A", "A", "B"),
      to = c("X", "Y", "Z")
    )
    audit <- audit_redirects(redirects)
    expect_equal(audit$n_conflicts, 1)
    expect_equal(audit$conflicts$source, "A")
    expect_equal(audit$conflicts$n_targets, 2)
  })

  it("detects loops", {
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    audit <- audit_redirects(redirects)
    expect_equal(audit$n_loops, 1)
    expect_true(length(audit$loops) == 1)
    # The loop path should mention A, B, C
    expect_true(grepl("A", fixed = TRUE, audit$loops[[1]]))
    expect_true(grepl("B", fixed = TRUE, audit$loops[[1]]))
    expect_true(grepl("C", fixed = TRUE, audit$loops[[1]]))
  })

  it("detects orphaned redirects", {
    redirects <- data.frame(
      from = c("A", "B"),
      to = c("X", "Y")
    )
    edges <- data.frame(
      from = "Z", to = "A"
    )
    audit <- audit_redirects(redirects, edge_list_df = edges)
    # B is not in the edge list
    expect_gt(nrow(audit$orphaned_redirects), 0)
    expect_true("B" %in% audit$orphaned_redirects$from)
  })

  it("marks loop members in chains", {
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A")
    )
    audit <- audit_redirects(redirects)
    expect_true(all(audit$chains$in_loop))
  })
})


describe("audit_redirects edge cases", {
  it("handles empty redirects", {
    redirects <- data.frame(
      from = character(0), to = character(0)
    )
    audit <- audit_redirects(redirects)
    expect_equal(audit$n_rules, 0)
    expect_equal(audit$n_self_refs, 0)
    expect_equal(audit$n_conflicts, 0)
    expect_equal(audit$n_loops, 0)
  })

  it("handles all-NA redirects", {
    redirects <- data.frame(
      from = c(NA, NA), to = c(NA, NA)
    )
    audit <- audit_redirects(redirects)
    expect_equal(audit$n_rules, 0)
  })

  it("handles single redirect", {
    redirects <- data.frame(from = "A", to = "B")
    audit <- audit_redirects(redirects)
    expect_equal(audit$n_rules, 1)
    expect_equal(audit$max_chain_length, 1)
  })

  it("no orphans when edge_list_df is NULL", {
    redirects <- data.frame(from = "A", to = "B")
    audit <- audit_redirects(redirects)
    expect_null(audit$orphaned_redirects)
  })

  it("returns redirect_audit class", {
    redirects <- data.frame(from = "A", to = "B")
    audit <- audit_redirects(redirects)
    expect_s3_class(audit, "redirect_audit")
  })

  it("print method works without error", {
    redirects <- data.frame(
      from = c("A", "B", "C", "D", "D", "E"),
      to = c("B", "C", "A", "X", "Y", "E")
    )
    audit <- audit_redirects(redirects)
    expect_output(print(audit), "Redirect Audit Report")
  })
})


describe("audit_redirects print method coverage", {
  it("prints orphaned redirects info when present", {
    redirects <- data.frame(
      from = c("A", "B"),
      to = c("X", "Y")
    )
    edges <- data.frame(
      from = "Z", to = "A"
    )
    audit <- audit_redirects(redirects, edge_list_df = edges)
    expect_output(print(audit), "Orphaned redirects")
  })

  it("prints long chains when present", {
    redirects <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "Final")
    )
    audit <- audit_redirects(redirects)
    expect_output(print(audit), "Long chains")
  })

  it("prints clean report with no issues", {
    redirects <- data.frame(
      from = "A", to = "B"
    )
    audit <- audit_redirects(redirects)
    output <- capture.output(print(audit))
    expect_true(any(grepl("Redirect Audit Report", fixed = TRUE, output)))
    # The detail sections (with ---) should not appear
    expect_false(any(grepl("--- Self-referencing", fixed = TRUE, output)))
    expect_false(any(grepl("--- Conflicting", fixed = TRUE, output)))
    expect_false(any(grepl("--- Loops", fixed = TRUE, output)))
    expect_false(any(grepl("--- Long chains", fixed = TRUE, output)))
  })

  it("prints all sections for complex redirects", {
    redirects <- data.frame(
      from = c("A", "B", "C", "D", "D", "E"),
      to = c("B", "C", "A", "X", "Y", "E")
    )
    audit <- audit_redirects(redirects)
    output <- capture.output(print(audit))
    expect_true(any(grepl("Redirect Audit Report", fixed = TRUE, output)))
    expect_true(any(grepl("Self-referencing", fixed = TRUE, output)))
    expect_true(any(grepl("Conflicting", fixed = TRUE, output)))
    expect_true(any(grepl("Loops", fixed = TRUE, output)))
  })

  it("prints empty report for zero-rule audit", {
    redirects <- data.frame(
      from = character(0), to = character(0)
    )
    audit <- audit_redirects(redirects)
    expect_output(print(audit), "Total rules.*0")
  })
})


describe("audit_redirects only self-referencing redirects", {
  it("handles case where all redirects are self-refs", {
    redirects <- data.frame(
      from = c("A", "B"),
      to = c("A", "B")
    )
    audit <- audit_redirects(redirects)
    expect_equal(audit$n_self_refs, 2)
    expect_equal(audit$n_loops, 0)
    expect_equal(audit$max_chain_length, 0)
  })
})


describe("audit_redirects input validation", {
  it("errors on non-data-frame", {
    expect_error(audit_redirects("not a df"), "must be a data frame")
  })

  it("errors on missing columns", {
    redirects <- data.frame(x = "A", y = "B")
    expect_error(audit_redirects(redirects), "must have")
  })

  it("errors on non-data-frame edge_list_df", {
    redirects <- data.frame(from = "A", to = "B")
    expect_error(
      audit_redirects(redirects, edge_list_df = "bad"),
      "must be a data frame or NULL"
    )
  })
})
