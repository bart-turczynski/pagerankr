sim_sf_internal_fixture <- function() {
  data.frame(
    Address = c(
      "https://example.com/",
      "https://example.com/a",
      "https://example.com/b"
    ),
    `Status Code` = c("200", "200", "200"),
    Indexability = c("Indexable", "Indexable", "Indexable"),
    `Indexability Status` = c("", "", ""),
    `Redirect URL` = c("", "", ""),
    `Canonical Link Element` = c("", "", ""),
    check.names = FALSE
  )
}

sim_sf_links_fixture <- function() {
  data.frame(
    Type = c("Hyperlink", "Hyperlink", "Hyperlink"),
    Source = c(
      "https://example.com/",
      "https://example.com/",
      "https://example.com/a"
    ),
    Destination = c(
      "https://example.com/a",
      "https://example.com/b",
      "https://example.com/b"
    ),
    Follow = c("TRUE", "TRUE", "TRUE"),
    Rel = c("", "", ""),
    `Link Position` = c("Navigation", "Content", "Content"),
    `Link Origin` = c("HTML", "HTML", "HTML"),
    check.names = FALSE
  )
}

sim_sf_bundle <- function() {
  screaming_frog_bundle(
    sim_sf_internal_fixture(),
    sim_sf_links_fixture(),
    "all_outlinks"
  )
}

describe("simulate_changes_screaming_frog", {
  it("returns a comparison table with node_status and attributes", {
    bundle <- sim_sf_bundle()
    result <- simulate_changes_screaming_frog(bundle)
    expect_s3_class(result, "data.frame")
    expect_true("node_status" %in% names(result))
    expect_false(is.null(attr(result, "summary")))
    expect_s3_class(attr(result, "proposed"), "data.frame")
    expect_type(attr(result, "manifest"), "list")
  })

  it("with no changes, all deltas are zero", {
    bundle <- sim_sf_bundle()
    result <- simulate_changes_screaming_frog(bundle)
    expect_true(all(result$delta == 0))
  })

  it("retires a URL via redirect_urls_df", {
    bundle <- sim_sf_bundle()
    retire <- data.frame(
      from = "https://example.com/a",
      to = "https://example.com/b"
    )
    result <- simulate_changes_screaming_frog(
      bundle,
      redirect_urls_df = retire
    )
    a_row <- result[result$node_name == "https://example.com/a", ]
    # /a is folded into /b: present in baseline, absent in proposed.
    expect_true(is.na(a_row$pagerank_proposed))
    b_row <- result[result$node_name == "https://example.com/b", ]
    expect_gt(b_row$delta, 0)
  })

  it("models a redirect on top of the bundle's crawled graph", {
    # /a currently links to /b; retiring /a strips that outlink and folds /'s
    # inbound A-link onto /b. The added-link path also survives the bundle
    # schema (nofollow/placement/origin columns padded).
    bundle <- sim_sf_bundle()
    add <- data.frame(
      from = "https://example.com/b",
      to = "https://example.com/"
    )
    result <- simulate_changes_screaming_frog(
      bundle,
      add_links_df = add
    )
    home_row <- result[result$node_name == "https://example.com/", ]
    expect_gt(home_row$delta, 0)
  })

  it("errors on an unknown target under on_unknown_target = 'error'", {
    bundle <- sim_sf_bundle()
    retire <- data.frame(
      from = "https://example.com/a",
      to = "https://example.com/brand-new"
    )
    expect_error(
      simulate_changes_screaming_frog(
        bundle,
        redirect_urls_df = retire,
        on_unknown_target = "error"
      ),
      "brand-new"
    )
  })

  it("rejects a non-bundle input", {
    expect_error(
      simulate_changes_screaming_frog(data.frame(from = "A", to = "B")),
      "screaming_frog_bundle"
    )
  })
})

describe("simulate_changes_screaming_frog remove_urls", {
  it("flags a removed URL removed-dead on the bundle path", {
    bundle <- sim_sf_bundle()
    result <- simulate_changes_screaming_frog(
      bundle,
      remove_urls = "https://example.com/a"
    )
    a_row <- result[result$node_name == "https://example.com/a", ]
    expect_equal(a_row$node_status, "removed-dead")
    expect_equal(
      attr(result, "manifest")$urls_removed,
      "https://example.com/a"
    )
    # The forced 404 overrides the bundle's real 200 crawled status.
    audit <- attr(attr(result, "proposed"), "transition_audit")
    expect_gte(audit$dropped$n_status_dead, 1L)
  })
})
