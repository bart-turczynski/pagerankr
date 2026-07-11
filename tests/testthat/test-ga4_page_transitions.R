# Tests for ga4_page_transitions()

describe("ga4_page_transitions: basic edge list", {
  it("produces a from/to edge list with counts in pagerank() input shape", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u1", "u1", "u2", "u2"),
      ga_session_id = c(1, 1, 1, 9, 9),
      page_location = c("/home", "/blog", "/contact", "/home", "/blog"),
      event_timestamp = c(100, 200, 300, 100, 200),
      batch_page_id = c(0, 1, 2, 0, 1),
      batch_ordering_id = c(0, 0, 0, 0, 0),
      batch_event_index = c(0, 1, 2, 0, 1)
    )
    res <- ga4_page_transitions(events)

    expect_s3_class(res, "data.frame")
    expect_named(res, c("from", "to", "n"))
    expect_type(res$from, "character")
    expect_type(res$to, "character")
    expect_type(res$n, "integer")

    # /home->/blog occurs in both sessions, so count == 2.
    home_blog <- res[res$from == "/home" & res$to == "/blog", ]
    expect_equal(home_blog$n, 2L)
    blog_contact <- res[res$from == "/blog" & res$to == "/contact", ]
    expect_equal(blog_contact$n, 1L)
  })

  it("feeds straight into pagerank()", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u1", "u1"),
      ga_session_id = c(1, 1, 1),
      page_location = c("/a", "/b", "/c"),
      event_timestamp = c(1, 2, 3)
    )
    res <- ga4_page_transitions(events)
    pr <- pagerank(res, weight_col = "n", clean_edge_urls = FALSE)
    expect_s3_class(pr, "data.frame")
    expect_gte(nrow(pr), 3)
  })
})


describe("ga4_page_transitions: deterministic ordering with tied timestamps", {
  # The acceptance fixture: events sharing the SAME event_timestamp must order
  # deterministically via the batch tie-break fields.
  make_tied_events <- function(row_order) {
    e <- data.frame(
      user_pseudo_id = rep("u1", 3),
      ga_session_id = rep(1, 3),
      page_location = c("/p1", "/p2", "/p3"),
      event_timestamp = c(500, 500, 500), # all tied
      batch_page_id = c(0, 0, 0),
      batch_ordering_id = c(0, 0, 0),
      batch_event_index = c(0, 1, 2) # true client order
    )
    e[row_order, , drop = FALSE]
  }

  it("recovers stable order from tied timestamps via batch_event_index", {
    # Even when the rows arrive shuffled, the batch fields impose order
    # /p1 -> /p2 -> /p3.
    res <- ga4_page_transitions(make_tied_events(c(3, 1, 2)))
    expect_equal(
      res[order(res$from), c("from", "to")],
      data.frame(
        from = c("/p1", "/p2"),
        to = c("/p2", "/p3")
      ),
      ignore_attr = TRUE
    )
  })

  it("is invariant to input row permutation (deterministic)", {
    perms <- list(c(1, 2, 3), c(3, 2, 1), c(2, 3, 1), c(2, 1, 3))
    results <- lapply(perms, function(p) {
      ga4_page_transitions(make_tied_events(p))
    })
    for (r in results[-1]) {
      expect_equal(r, results[[1]])
    }
  })

  it("breaks ties on batch_page_id then batch_ordering_id", {
    # All timestamps tied; ordering must walk batch_page_id, then
    # batch_ordering_id, then batch_event_index.
    e <- data.frame(
      user_pseudo_id = rep("u1", 3),
      ga_session_id = rep(1, 3),
      page_location = c("/third", "/first", "/second"),
      event_timestamp = c(7, 7, 7),
      batch_page_id = c(2, 0, 1),
      batch_ordering_id = c(0, 0, 0),
      batch_event_index = c(0, 0, 0)
    )
    res <- ga4_page_transitions(e)
    # Order is /first -> /second -> /third.
    expect_equal(res$n[res$from == "/first" & res$to == "/second"], 1L)
    expect_equal(res$n[res$from == "/second" & res$to == "/third"], 1L)
    expect_false(any(res$from == "/third"))
  })
})


describe("ga4_page_transitions: session boundaries", {
  it("never joins page views across sessions", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u1", "u1"),
      ga_session_id = c(1, 1, 2), # last view is a new session
      page_location = c("/a", "/b", "/c"),
      event_timestamp = c(1, 2, 3)
    )
    res <- ga4_page_transitions(events)
    # /b -> /c crosses a session boundary and must not appear.
    expect_false(any(res$from == "/b" & res$to == "/c"))
    expect_true(any(res$from == "/a" & res$to == "/b"))
  })

  it("separates sessions by user too", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u2"),
      ga_session_id = c(1, 1), # same session id, different user
      page_location = c("/a", "/b"),
      event_timestamp = c(1, 2)
    )
    res <- ga4_page_transitions(events)
    expect_equal(nrow(res), 0L)
    expect_named(res, c("from", "to", "n"))
  })
})


describe("ga4_page_transitions: self-transitions", {
  it("drops consecutive same-page views by default (reloads)", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u1", "u1"),
      ga_session_id = c(1, 1, 1),
      page_location = c("/a", "/a", "/b"),
      event_timestamp = c(1, 2, 3)
    )
    res <- ga4_page_transitions(events)
    expect_false(any(res$from == "/a" & res$to == "/a"))
    expect_true(any(res$from == "/a" & res$to == "/b"))
  })

  it("keeps self-transitions when drop_self_transitions = FALSE", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u1"),
      ga_session_id = c(1, 1),
      page_location = c("/a", "/a"),
      event_timestamp = c(1, 2)
    )
    res <- ga4_page_transitions(events, drop_self_transitions = FALSE)
    expect_equal(res$n[res$from == "/a" & res$to == "/a"], 1L)
  })
})


describe("ga4_page_transitions: custom column names", {
  it("honors custom session/page/ordering and output column names", {
    events <- data.frame(
      uid = c("u1", "u1"),
      sess = c(1, 1),
      page = c("/x", "/y"),
      ts = c(1, 2)
    )
    res <- ga4_page_transitions(
      events,
      user_id_col = "uid",
      session_id_col = "sess",
      page_col = "page",
      timestamp_col = "ts",
      from_col = "source",
      to_col = "target",
      count_col = "weight"
    )
    expect_named(res, c("source", "target", "weight"))
    expect_equal(res$weight, 1L)
  })

  it("works without any batch tie-break columns present", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u1"),
      ga_session_id = c(1, 1),
      page_location = c("/a", "/b"),
      event_timestamp = c(1, 2)
    )
    expect_silent(ga4_page_transitions(events))
  })
})


describe("ga4_page_transitions: edge cases", {
  it("returns an empty edge list for empty input", {
    events <- data.frame(
      user_pseudo_id = character(0),
      ga_session_id = numeric(0),
      page_location = character(0),
      event_timestamp = numeric(0)
    )
    res <- ga4_page_transitions(events)
    expect_equal(nrow(res), 0L)
    expect_named(res, c("from", "to", "n"))
  })

  it("returns empty when every session has a single page view", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u2"),
      ga_session_id = c(1, 1),
      page_location = c("/a", "/b"),
      event_timestamp = c(1, 1)
    )
    res <- ga4_page_transitions(events)
    expect_equal(nrow(res), 0L)
  })

  it("drops transitions adjacent to an NA page", {
    events <- data.frame(
      user_pseudo_id = c("u1", "u1", "u1"),
      ga_session_id = c(1, 1, 1),
      page_location = c("/a", NA, "/b"),
      event_timestamp = c(1, 2, 3)
    )
    res <- ga4_page_transitions(events)
    # /a->NA and NA->/b both dropped; no usable transition remains.
    expect_equal(nrow(res), 0L)
  })

  it("returns empty when events_df has only a single row", {
    events <- data.frame(
      user_pseudo_id = "u1",
      ga_session_id = 1,
      page_location = "/a",
      event_timestamp = 1
    )
    res <- ga4_page_transitions(events)
    expect_equal(nrow(res), 0L)
    expect_named(res, c("from", "to", "n"))
  })
})


describe("ga4_page_transitions: validation", {
  it("errors when events_df is not a data frame", {
    expect_error(ga4_page_transitions(list(a = 1)), "must be a data frame")
  })

  it("errors on a missing required column", {
    events <- data.frame(
      user_pseudo_id = "u1",
      ga_session_id = 1,
      page_location = "/a"
    )
    expect_error(
      ga4_page_transitions(events),
      "missing required column"
    )
  })

  it("errors on a bad drop_self_transitions flag", {
    events <- data.frame(
      user_pseudo_id = "u1", ga_session_id = 1,
      page_location = "/a", event_timestamp = 1
    )
    expect_error(
      ga4_page_transitions(events, drop_self_transitions = "yes"),
      "must be TRUE or FALSE"
    )
  })

  it("errors when a scalar-string argument is not a character", {
    events <- data.frame(
      user_pseudo_id = "u1", ga_session_id = 1,
      page_location = "/a", event_timestamp = 1
    )
    expect_error(
      ga4_page_transitions(events, user_id_col = 123),
      "must be a single non-NA character string"
    )
  })

  it("errors when a scalar-string argument has length != 1", {
    events <- data.frame(
      user_pseudo_id = "u1", ga_session_id = 1,
      page_location = "/a", event_timestamp = 1
    )
    expect_error(
      ga4_page_transitions(events, user_id_col = c("a", "b")),
      "must be a single non-NA character string"
    )
  })

  it("errors when a scalar-string argument is NA", {
    events <- data.frame(
      user_pseudo_id = "u1", ga_session_id = 1,
      page_location = "/a", event_timestamp = 1
    )
    expect_error(
      ga4_page_transitions(events, user_id_col = NA_character_),
      "must be a single non-NA character string"
    )
  })

  it("errors when drop_self_transitions has length != 1", {
    events <- data.frame(
      user_pseudo_id = "u1", ga_session_id = 1,
      page_location = "/a", event_timestamp = 1
    )
    expect_error(
      ga4_page_transitions(events, drop_self_transitions = c(TRUE, FALSE)),
      "must be TRUE or FALSE"
    )
  })

  it("errors when drop_self_transitions is NA", {
    events <- data.frame(
      user_pseudo_id = "u1", ga_session_id = 1,
      page_location = "/a", event_timestamp = 1
    )
    expect_error(
      ga4_page_transitions(events, drop_self_transitions = NA),
      "must be TRUE or FALSE"
    )
  })
})
