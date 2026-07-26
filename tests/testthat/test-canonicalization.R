# The single list of record for get_clean_url arguments that canonical_profile()
# deliberately does NOT pin. Kept at file scope so the surface guard and its own
# regression tests share one definition rather than two parallel lists.
triaged_unpinned_args <- c(
  "url", # the input, not a knob
  "source", # PSL source; unreachable at the pinned www/subdomain values
  # The key drops the query, so query_handling and its sub-options are inert.
  "query_handling", "params_keep", "params_drop", "params_case_sensitive",
  "sort_params", "empty_param_handling", "decode_plus",
  "port_handling", # the key drops the port
  "url_standard", # standard selector (rurl 2.2.0)
  # rurl 2.7.0: parse-route selectors, not key components. `profile` is an
  # unrelated rurl concept that merely shares a name with canonical_profile.
  "scheme_policy", "scheme_acceptance", "engine", "profile"
)

# Compare rurl's actual argument surface against the surface pagerankr has
# accounted for (pinned in canonical_profile() + triaged as unpinned above).
# Takes the rurl names as an argument so removals can be exercised with a stub
# instead of an actual rurl downgrade.
canonicalization_surface_diff <- function(
    rurl_arg_names,
    profile_names = names(canonical_profile()),
    triaged = triaged_unpinned_args) {
  accounted <- c(profile_names, triaged)
  list(
    # rurl grew an argument pagerankr has never considered.
    added = setdiff(rurl_arg_names, accounted),
    # rurl dropped an argument pagerankr still accounts for.
    removed = setdiff(accounted, rurl_arg_names)
  )
}

describe("canonical_profile", {
  it("pins every node-identity rurl::get_clean_url knob explicitly", {
    profile <- canonical_profile()
    expect_named(
      profile,
      c(
        "protocol_handling", "case_handling", "www_handling",
        "trailing_slash_handling", "index_page_handling", "path_normalization",
        "scheme_relative_handling", "subdomain_levels_to_keep",
        "host_encoding", "path_encoding"
      )
    )
    # The contract anchors: keep scheme, lower the host.
    expect_equal(profile$protocol_handling, "keep")
    expect_equal(profile$case_handling, "lower_host")
  })

  it("overrides only the redefined path knobs; mirrors defaults otherwise", {
    profile <- canonical_profile()
    # Intentional overrides: rurl 2.1.0 redefined "none"/"keep" to keep the path
    # verbatim, so we pin the values that reproduce the committed canonical key
    # (decode + dot-segment removal). See canonical_profile() @details.
    expect_identical(profile$path_normalization, "dot_segments")
    expect_identical(profile$path_encoding, "decode")

    # Every other knob still equals rurl's current default (so those stay
    # drift-guarded; a future default change surfaces here).
    defaults <- formals(rurl::get_clean_url)
    overridden <- c("path_normalization", "path_encoding")
    for (k in setdiff(names(profile), overridden)) {
      expect_identical(
        profile[[k]], eval(defaults[[k]]),
        info = paste("profile diverges from rurl default for", k)
      )
    }
  })

  it("flags any rurl knob the profile has neither pinned nor triaged", {
    # Surface guard against ADDITIONS. The two tests above both iterate over
    # `names(profile)`, so an argument rurl grows but pagerankr has never
    # considered is invisible to them (rurl 2.7.0 added four). Read rurl's own
    # formals instead: a new argument fails here until it is deliberately
    # pinned in canonical_profile() or listed in triaged_unpinned_args.
    diff <- canonicalization_surface_diff(names(formals(rurl::get_clean_url)))
    expect_identical(
      diff$added, character(0),
      info = paste(
        "rurl grew get_clean_url argument(s) pagerankr has not triaged:",
        toString(diff$added),
        "- pin them in canonical_profile() or add them to",
        "triaged_unpinned_args."
      )
    )
  })

  it("flags any argument rurl has dropped since pagerankr triaged it", {
    # Surface guard against REMOVALS -- the direction the additions-only
    # `setdiff` above is blind to. A shrinking surface is the realistic case:
    # the `Imports: rurl` floor is satisfied by releases that predate the
    # 2.7.0 arguments, so any pin, vendor, or CRAN-resolved install can
    # resolve to a rurl that lacks them.
    diff <- canonicalization_surface_diff(names(formals(rurl::get_clean_url)))

    # Severe: canonical_profile() passes these by name, so their removal makes
    # every do.call(rurl::get_clean_url, canonical_profile()) an error.
    pinned_removed <- intersect(diff$removed, names(canonical_profile()))
    expect_identical(
      pinned_removed, character(0),
      info = paste(
        "rurl dropped get_clean_url argument(s) canonical_profile() PINS:",
        toString(pinned_removed),
        "- the installed rurl cannot reproduce the canonical key."
      )
    )

    # Benign-but-report: pagerankr never set these, so their removal cannot
    # change the key. It does mean the triage list has gone stale, and it is
    # a reliable signal that rurl was downgraded.
    triaged_removed <- setdiff(diff$removed, names(canonical_profile()))
    expect_identical(
      triaged_removed, character(0),
      info = paste(
        "rurl dropped get_clean_url argument(s) pagerankr triaged as unpinned:",
        toString(triaged_removed),
        "- the key is unaffected, but confirm this is an intended rurl",
        "version change and prune triaged_unpinned_args."
      )
    )
  })

  it("surface diff detects removals as well as additions", {
    # Regression test for the guard itself: the original one-directional
    # `setdiff` returned character(0) for both "nothing changed" and
    # "arguments disappeared". Stub the formal names rather than downgrading
    # rurl for real.
    actual <- names(formals(rurl::get_clean_url))

    # Baseline: the real surface is fully accounted for in both directions.
    clean <- canonicalization_surface_diff(actual)
    expect_identical(clean$added, character(0))
    expect_identical(clean$removed, character(0))

    # A rurl that LOST the 2.7.0 parse-route selectors (i.e. any 2.2.x).
    route_args <- c("scheme_policy", "scheme_acceptance", "engine", "profile")
    downgraded <- canonicalization_surface_diff(setdiff(actual, route_args))
    expect_identical(downgraded$added, character(0))
    expect_setequal(downgraded$removed, route_args)

    # A rurl that LOST a knob canonical_profile() actually pins.
    pinned_loss <- canonicalization_surface_diff(
      setdiff(actual, "case_handling")
    )
    expect_identical(pinned_loss$removed, "case_handling")

    # The addition case still fires.
    grown <- canonicalization_surface_diff(c(actual, "brand_new_knob"))
    expect_identical(grown$added, "brand_new_knob")
    expect_identical(grown$removed, character(0))
  })

  it("drops port, query, and fragment from the canonical key", {
    # Behavioral guard against a default FLIP on the knobs the profile
    # deliberately leaves unpinned (see canonical_profile() @details for the
    # full list). The node key is scheme+host+path; assert those components
    # really are dropped so a future rurl default flip on an unpinned knob is
    # caught here rather than silently changing node identity.
    key <- do.call(
      rurl::get_clean_url,
      c(
        list(url = "http://Example.COM:8080/a/../b?utm_source=x#frag"),
        canonical_profile()
      )
    )
    expect_false(grepl("8080", key, fixed = TRUE))
    expect_false(grepl("utm_source", key, fixed = TRUE))
    expect_false(grepl("frag", key, fixed = TRUE))
    # Positive control: host lowered, dot-segment removed, scheme kept.
    expect_identical(unname(key), "http://example.com/b")
  })
})

describe("resolve_rurl_params (via clean_url_columns)", {
  it("applies the canonical profile by default (no overrides)", {
    urls <- c("HTTP://EXAMPLE.COM/Path/", "https://Sub.Example.CO.UK/a")
    df <- data.frame(url = urls)
    cleaned <- clean_url_columns(df, columns = "url")
    expect_equal(
      cleaned$url,
      unname(rurl::get_clean_url(urls))
    )
  })

  it("applies a case_handling override when supplied", {
    df <- data.frame(url = "HTTP://EXAMPLE.COM/path")
    default_result <- clean_url_columns(df, columns = "url")
    expect_true(grepl("example.com", default_result$url, fixed = TRUE))

    override_result <- clean_url_columns(
      df, columns = "url", case_handling = "keep"
    )
    expect_true(grepl("EXAMPLE.COM", override_result$url, fixed = TRUE))
  })
})

describe("resolve_rurl_params validation", {
  it("errors when rurl_params is not a list (via filter_links_by_domain)", {
    edges <- data.frame(
      from = "http://example.com/a", to = "http://example.com/b"
    )
    expect_error(
      filter_links_by_domain(edges, rurl_params = "not-a-list"),
      "must be a list"
    )
  })
})

describe("canonical node key across the parse-determinism risk surface", {
  # rurl parses via libcurl, whose edge-case output has varied by platform and
  # libcurl version; rurl is making that deterministic, which is a behavior
  # change for exactly these constructs. Pin the node keys they produce so a
  # rurl upgrade that moves any of them fails `devtools::test()` here, instead
  # of needing an out-of-band probe script to notice. Rows marked (raw) are
  # ones rurl returns NA for, kept as their raw token by clean_url_columns().
  it("pins the key for every risk-surface construct", {
    inputs <- c(
      "http://Example.COM/a",
      "https://example.com:8080/a/../b?q=1#f",
      "/a", # scheme-less rooted (raw)
      "B", # dotless bare token (raw)
      "//example.com/a", # protocol-relative
      "example.org/index.html",
      "http://B\u{fc}cher.example/\u{fc}ber", # IDN host + non-ASCII path
      "http://xn--bcher-kva.example/a", # the punycode form of the same host
      "ftp://example.com/a",
      "mailto:a@example.com", # (raw)
      "tel:+123456", # (raw)
      "file:///tmp/x", # the construct at the center of the rurl epic (raw)
      "javascript:void(0)", # (raw)
      "http://example.com/%7Euser/a",
      "http://example.com/a/./b/../c",
      NA,
      "http://WWW.Example.com/",
      "http://example.com",
      "https://user:pw@example.com/a",
      "http://example.com/a%20b",
      "  http://example.com/a  ", # surrounding whitespace (raw)
      "http://sub.example.co.uk/a"
    )
    expected <- c(
      "http://example.com/a",
      "https://example.com/b",
      "/a",
      "B",
      "http://example.com/a",
      "http://example.org/index.html",
      "http://b\u{fc}cher.example/\u{fc}ber",
      "http://xn--bcher-kva.example/a",
      "ftp://example.com/a",
      "mailto:a@example.com",
      "tel:+123456",
      "file:///tmp/x",
      "javascript:void(0)",
      "http://example.com/~user/a",
      "http://example.com/a/c",
      NA_character_,
      "http://www.example.com/",
      "http://example.com/",
      "https://example.com/a",
      "http://example.com/a b",
      "  http://example.com/a  ",
      "http://sub.example.co.uk/a"
    )

    keys <- clean_url_columns(
      data.frame(url = inputs),
      columns = "url"
    )$url

    expect_equal(unname(keys), expected)
  })

  it("keeps the IDN host and its punycode form as distinct nodes", {
    # host_encoding = "keep" means neither form is folded into the other, so
    # they are two nodes. Asserted explicitly because a rurl change to IDN
    # rendering would merge or split nodes without changing any key above.
    idn <- "http://B\u{fc}cher.example/a"
    puny <- "http://xn--bcher-kva.example/a"
    keys <- clean_url_columns(
      data.frame(url = c(idn, puny)),
      columns = "url"
    )$url
    expect_false(identical(keys[[1]], keys[[2]]))
  })
})

describe("profile is behavior-preserving end to end", {
  it("clean_url_columns matches rurl defaults when no overrides are given", {
    urls <- c(
      "HTTP://WWW.Example.COM/Path/", "https://sub.example.co.uk/a?b=1#f",
      "example.org/index.html"
    )
    df <- data.frame(from = urls, to = urls)
    cleaned <- clean_url_columns(df, columns = "from")
    expect_equal(unname(cleaned$from), unname(rurl::get_clean_url(urls)))
  })
})
