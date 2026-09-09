# The single list of record for get_clean_url arguments that canonical_profile()
# deliberately does NOT pin. Kept at file scope so the surface guard and its own
# regression tests share one definition rather than two parallel lists.
triaged_unpinned_args <- c(
  "url", # the input, not a knob
  "source", # PSL source; unreachable at the pinned www/subdomain values
  # rurl 2.7.0: parse-route selectors, not key components. `profile` is an
  # unrelated rurl concept that merely shares a name with canonical_profile.
  # Note `profile = "seo"` is deliberately NOT used -- see canonical_profile().
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
        "host_encoding", "path_encoding", "url_standard", "port_handling",
        "query_handling", "params_keep", "params_drop",
        "params_case_sensitive", "sort_params", "empty_param_handling",
        "decode_plus", "credential_handling"
      )
    )
    # The contract anchors: keep scheme, lower the host.
    expect_equal(profile$protocol_handling, "keep")
    expect_equal(profile$case_handling, "lower_host")
  })

  it("overrides only the identity knobs; mirrors defaults otherwise", {
    profile <- canonical_profile()
    # Intentional overrides. `url_standard` is where identity semantics live;
    # `path_encoding = "keep"` holds the presentation dial at its only
    # identity-preserving value. See canonical_profile() @details.
    expect_identical(profile$path_normalization, "dot_segments")
    expect_identical(profile$path_encoding, "keep")
    expect_identical(profile$url_standard, "whatwg")
    # A non-default port is a different origin; an IDN host and its punycode
    # form are the same request; a contentful param is part of the resource.
    expect_identical(profile$port_handling, "strip_default")
    expect_identical(profile$host_encoding, "idna")
    expect_identical(profile$query_handling, "filter")

    # Every other knob still equals rurl's current default (so those stay
    # drift-guarded; a future default change surfaces here).
    defaults <- formals(rurl::get_clean_url)
    overridden <- c(
      "path_normalization", "path_encoding", "url_standard",
      "port_handling", "host_encoding", "query_handling"
    )
    for (k in setdiff(names(profile), overridden)) {
      # For a match.arg formal the effective default is the first element of
      # the choice vector, not the whole vector.
      effective <- eval(defaults[[k]])
      if (length(effective) > 1L) effective <- effective[[1L]]
      expect_identical(
        profile[[k]], effective,
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

  it("keeps what identifies the resource and drops what does not", {
    # Behavioral guard on the shape of the key itself. Under "parse, do not
    # fold" the key is scheme + host + non-default port + path + contentful
    # query; the fragment and userinfo identify no resource and go.
    key <- do.call(
      rurl::get_clean_url,
      c(
        list(url = "http://Example.COM:8080/a/../b?utm_source=x&c=1#frag"),
        canonical_profile()
      )
    )
    # Kept: a non-default port is a different origin; `c=1` is contentful.
    expect_true(grepl(":8080", key, fixed = TRUE))
    expect_true(grepl("c=1", key, fixed = TRUE))
    # Dropped: a tracking param names no distinct page, a fragment no resource.
    expect_false(grepl("utm_source", key, fixed = TRUE))
    expect_false(grepl("frag", key, fixed = TRUE))
    # Host lowered, dot-segment removed, scheme kept.
    expect_identical(unname(key), "http://example.com:8080/b?c=1")
  })

  it("strips only the ports the standard makes redundant", {
    keys <- do.call(
      rurl::get_clean_url,
      c(
        list(url = c(
          "http://example.com:80/a", "https://example.com:443/a",
          "http://example.com/a", "https://example.com/a",
          "http://example.com:8080/a"
        )),
        canonical_profile()
      )
    )
    # :80 on http and :443 on https are redundant, so they fold into the
    # port-less form. :8080 is a different origin and must not.
    expect_identical(unname(keys[[1]]), unname(keys[[3]]))
    expect_identical(unname(keys[[2]]), unname(keys[[4]]))
    expect_identical(unname(keys[[5]]), "http://example.com:8080/a")
  })

  it("separates contentful params from tracking noise", {
    keys <- do.call(
      rurl::get_clean_url,
      c(
        list(url = c(
          "https://example.com/a?color=red", "https://example.com/a?color=blue",
          "https://example.com/a?utm_source=x", "https://example.com/a"
        )),
        canonical_profile()
      )
    )
    # Faceted values are different pages; no redirect or canonical need exist
    # between them, so the graph has to keep them apart.
    expect_false(identical(keys[[1]], keys[[2]]))
    # A tracking param names no distinct page.
    expect_identical(unname(keys[[3]]), unname(keys[[4]]))
  })
})

describe("resolve_rurl_params (via clean_url_columns)", {
  it("applies the canonical profile by default (no overrides)", {
    # Compared against the PROFILE, not against rurl's bare defaults: the
    # profile deliberately diverges from six of them (see canonical_profile()).
    # What this pins is that clean_url_columns() applies that profile and adds
    # nothing of its own.
    urls <- c(
      "HTTP://EXAMPLE.COM/Path/", "https://Sub.Example.CO.UK/a",
      "http://example.com:8080/a?utm_source=x&c=1"
    )
    df <- data.frame(url = urls)
    cleaned <- clean_url_columns(df, columns = "url")
    expect_equal(
      cleaned$url,
      unname(do.call(
        rurl::get_clean_url, c(list(url = urls), canonical_profile())
      ))
    )
  })

  it("applies a user override when supplied", {
    df <- data.frame(url = "http://www.example.com/path")
    default_result <- clean_url_columns(df, columns = "url")
    expect_true(grepl("www.example.com", default_result$url, fixed = TRUE))

    override_result <- clean_url_columns(
      df, columns = "url", www_handling = "strip"
    )
    expect_false(grepl("www.", override_result$url, fixed = TRUE))
  })

  it("rejects an override of a knob url_standard governs", {
    # Since the profile pins url_standard = "whatwg", rurl refuses a
    # conflicting value for the axes that selector governs (case_handling,
    # path_normalization). This is a deliberate narrowing of `rurl_params`:
    # those two axes ARE node identity, and a caller who could move them could
    # silently re-key the graph. Pinned so the restriction is documented
    # behavior rather than a surprise from a dependency.
    df <- data.frame(url = "HTTP://EXAMPLE.COM/path")
    expect_error(
      clean_url_columns(df, columns = "url", case_handling = "keep"),
      "governs `case_handling`"
    )
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
      "  http://example.com/a  ", # surrounding whitespace
      "http://sub.example.co.uk/a"
    )
    expected <- c(
      "http://example.com/a",
      # Non-default port kept (different origin); `q=1` is contentful.
      "https://example.com:8080/b?q=1",
      "/a",
      "B",
      "http://example.com/a",
      "http://example.org/index.html",
      # host_encoding = "idna" renders the punycode form for both spellings.
      "http://xn--bcher-kva.example/\u{fc}ber",
      "http://xn--bcher-kva.example/a",
      "ftp://example.com/a",
      "mailto:a@example.com",
      "tel:+123456",
      "file:///tmp/x",
      "javascript:void(0)",
      # whatwg preserves percent spellings, so %7E is NOT folded to `~`.
      "http://example.com/%7Euser/a",
      "http://example.com/a/c",
      NA_character_,
      "http://www.example.com/",
      "http://example.com/",
      "https://example.com/a",
      "http://example.com/a%20b",
      # whatwg strips leading/trailing C0-or-space, so this now parses rather
      # than falling through to clean_url_columns()'s raw-token fallback.
      "http://example.com/a",
      "http://sub.example.co.uk/a"
    )

    keys <- clean_url_columns(
      data.frame(url = inputs),
      columns = "url"
    )$url

    expect_equal(unname(keys), expected)
  })

  it("pins the key for percent-encoded dot segments", {
    # Every row here is pinned to the answer the URL standard requires --
    # `%2e` is a dot segment wherever a literal `.` would be one -- so a
    # release that stops removing them fails here rather than silently
    # re-keying the graph. rurl 3.0.0 DID stop, under the old
    # path_encoding = "decode" pin, by running decode after dot-segment
    # removal; url_standard = "whatwg" is what restores conformance, because
    # the standard recognizes the encoded form directly rather than relying on
    # a presentation dial to decode it first.
    inputs <- c(
      "http://example.com/%2e%2e/a",
      "http://example.com/%2E/a",
      "http://example.com/a/.%2e/b", # partly encoded
      "http://example.com/a/%2e./b", # partly encoded, other half
      "http://example.com/a%2e%2eb", # NOT a segment: dots are interior
      "http://example.com/%252e%252e/a", # double-encoded: decodes once only
      "http://example.com/a/%2e%2e", # trailing, pops to root
      "http://example.com/a/b/%2e%2e/" # trailing with slash
    )
    expected <- c(
      "http://example.com/a",
      "http://example.com/a",
      "http://example.com/b",
      "http://example.com/b",
      # Interior dots, not a segment. whatwg preserves the spelling.
      "http://example.com/a%2e%2eb",
      # Double-encoded: whatwg decodes nothing, so it stays fully encoded and
      # never becomes a dot segment. (Under the old decode pin this decoded
      # once, to /%2e%2e/a.)
      "http://example.com/%252e%252e/a",
      "http://example.com/",
      "http://example.com/a/"
    )

    keys <- clean_url_columns(
      data.frame(url = inputs),
      columns = "url"
    )$url

    expect_equal(unname(keys), expected)
  })

  it("pins the key for the classes the profile used to get wrong", {
    # This block used to record DEVIATIONS from the URL standard, pinned to
    # what rurl returned rather than to what the standard prescribes. Under
    # url_standard = "whatwg" the first four are now conformant, and the rows
    # stay here as the regression guard for exactly that.
    inputs <- c(
      # `%2F` is not a path separator: `..%2fa` is one segment, so nothing may
      # be popped, and `a%2Findex.html` is one segment, not two. The old
      # path_encoding = "decode" pin merged both pairs.
      "http://example.com/..%2fa",
      "http://example.com/a%2Findex.html",
      # The standard strips tab and newline before parsing.
      "http://example.com/a\tb",
      "http://example.com/a\nb",
      # NOT a deviation: rurl keeps a root-dot host distinct from its bare
      # form deliberately (rurl RURL-eikgtrqf), and get_url_key() holds them
      # apart too. Two nodes, on purpose.
      "http://example.com./a"
    )
    expected <- c(
      "http://example.com/..%2fa",
      "http://example.com/a%2Findex.html",
      "http://example.com/ab",
      "http://example.com/ab",
      "http://example.com./a"
    )

    keys <- clean_url_columns(
      data.frame(url = inputs),
      columns = "url"
    )$url

    expect_equal(unname(keys), expected)
  })

  it("keeps an encoded slash distinct from a path separator", {
    # The inverse of what this test used to assert. `%2F` is data inside one
    # segment; `/` is structure. Merging them made two resources one node.
    # Stated as an explicit split assertion rather than as string equality so
    # a regression names the defect instead of just moving a fixture row.
    keys <- clean_url_columns(
      data.frame(url = c("http://example.com/a%2Fb", "http://example.com/a/b")),
      columns = "url"
    )$url
    expect_false(identical(keys[[1]], keys[[2]]))
  })

  it("folds an IDN host into its punycode form", {
    # The inverse of what this test used to assert. The two spellings resolve
    # to the same bytes on the wire, so no redirect and no canonical can ever
    # exist between them -- if the graph does not fold them, nothing will.
    # host_encoding = "idna" normalizes both to the punycode form.
    idn <- "http://B\u{fc}cher.example/a"
    puny <- "http://xn--bcher-kva.example/a"
    keys <- clean_url_columns(
      data.frame(url = c(idn, puny)),
      columns = "url"
    )$url
    expect_identical(keys[[1]], keys[[2]])
    expect_identical(unname(keys[[1]]), "http://xn--bcher-kva.example/a")
  })
})

describe("profile is applied end to end", {
  it("clean_url_columns adds nothing beyond the profile", {
    # Not a comparison against rurl's bare defaults -- the profile diverges
    # from six of those on purpose. This pins that the wrapper is a faithful
    # pass-through of canonical_profile() and introduces no behavior of its
    # own, on a column that exercises host case, port, query and fragment.
    urls <- c(
      "HTTP://WWW.Example.COM/Path/", "https://sub.example.co.uk/a?b=1#f",
      "example.org/index.html", "http://example.com:8080/a?utm_source=x"
    )
    df <- data.frame(from = urls, to = urls)
    cleaned <- clean_url_columns(df, columns = "from")
    expect_equal(
      unname(cleaned$from),
      unname(do.call(
        rurl::get_clean_url, c(list(url = urls), canonical_profile())
      ))
    )
  })
})
