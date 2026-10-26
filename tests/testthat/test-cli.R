# The command-line entry point scores an edge list with pagerank() and
# compares two score files with compare_pagerank(). These tests first pin what
# those two functions return on the shipped case-study crawls, then require the
# CLI to reproduce it.

# The graph-eligible edges of one shipped Screaming Frog crawl, reduced to the
# columns the CLI reads: from, to and placement. The fixture is a crawl export,
# not an edge list, so the edge list is derived through the package's own
# Screaming Frog importer.
cli_fixture_edges <- function(phase) {
  dir <- system.file(
    "extdata", paste0("reviews-microsite-", phase),
    package = "pagerankr"
  )
  bundle <- screaming_frog_bundle(
    internal = file.path(dir, "internal_all.csv"),
    links = file.path(dir, "all_inlinks.csv")
  )
  bundle$edges[, c("from", "to", "placement")]
}

cli_top_node <- function(scores) {
  scores[which.max(scores$pagerank), , drop = FALSE]
}

describe("pagerank() on the case-study fixture (pinned for the CLI)", {
  edges <- cli_fixture_edges("before")

  it("scores the before crawl with the documented defaults", {
    pr <- pagerank(edges, placement_col = "placement")

    expect_named(pr, c("node_name", "pagerank"))
    expect_identical(nrow(pr), 78L)
    expect_equal(sum(pr$pagerank), 1)
    top <- cli_top_node(pr)
    expect_identical(
      top$node_name, "https://external-12.example.org/s8/p02?q=1"
    )
    expect_equal(top$pagerank, 0.0213480653, tolerance = 1e-8)
    # The CLI forwards --damping; the default it inherits is 0.85.
    expect_identical(
      pagerank(edges, placement_col = "placement", damping = 0.85)$pagerank,
      pr$pagerank
    )
  })

  it("computes CheiRank as PageRank on the transposed graph", {
    fwd <- pagerank(edges, placement_col = "placement")
    chei <- pagerank(edges, placement_col = "placement", reverse = TRUE)
    swapped <- edges
    swapped$from <- edges$to
    swapped$to <- edges$from
    expected <- pagerank(swapped, placement_col = "placement")

    expect_setequal(chei$node_name, fwd$node_name)
    expect_equal(
      chei$pagerank,
      expected$pagerank[match(chei$node_name, expected$node_name)]
    )
    expect_false(isTRUE(all.equal(
      chei$pagerank, fwd$pagerank[match(chei$node_name, fwd$node_name)]
    )))
    top <- cli_top_node(chei)
    expect_identical(top$node_name, "https://reviews-microsite.example.net/s2/")
    expect_equal(top$pagerank, 0.1352078442, tolerance = 1e-8)
  })

  it("moves the scores with damping and placement weights", {
    weights <- c(
      content = 1, nav = 0.1, header = 0.1, footer = 0.1, aside = 0.1
    )
    pr <- pagerank(
      edges,
      placement_col = "placement", placement_weights = weights, damping = 0.7
    )
    top <- cli_top_node(pr)
    expect_equal(top$pagerank, 0.01823862222, tolerance = 1e-8)

    content_only <- pagerank(
      edges,
      placement_col = "placement", accepted_placements = "content"
    )
    expect_identical(nrow(content_only), 77L)
  })
})

describe("compare_pagerank() on the case-study fixture (pinned for the CLI)", {
  it("compares the before and after crawls", {
    before <- pagerank(cli_fixture_edges("before"), placement_col = "placement")
    after <- pagerank(cli_fixture_edges("after"), placement_col = "placement")
    cmp <- compare_pagerank(before, after)

    expect_identical(nrow(cmp), 96L)
    common <- cmp[!is.na(cmp$pagerank_a) & !is.na(cmp$pagerank_b), ]
    expect_equal(
      common$pagerank_a,
      before$pagerank[match(common$node_name, before$node_name)]
    )
    expect_equal(
      common$delta,
      after$pagerank[match(common$node_name, after$node_name)] -
        before$pagerank[match(common$node_name, before$node_name)]
    )
    summary <- attr(cmp, "summary")
    expect_identical(summary$nodes_gained, 18L)
    expect_identical(summary$nodes_lost, 6L)
    expect_equal(summary$spearman_rho, 0.8322140496, tolerance = 1e-8)
    expect_equal(summary$mean_abs_delta, 0.0006117597391, tolerance = 1e-8)
  })
})

# --- The CLI --------------------------------------------------------------

cli_csv <- function(df, na = "NA") {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(df, path, row.names = FALSE, na = na)
  path
}

cli_read <- function(path) {
  utils::read.csv(path, na.strings = "", stringsAsFactors = FALSE)
}

# Run cli() with --out pointing at a temp file and read the CSV back.
cli_run <- function(args) {
  out <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  cli(c(args, "--out", out))
  out
}

# Direct scores the CLI's `score` must reproduce.
cli_expected_scores <- function(edges, ...) {
  fwd <- pagerank(edges, placement_col = "placement", ...)
  chei <- pagerank(edges, placement_col = "placement", reverse = TRUE, ...)
  list(forward = fwd, reverse = chei)
}

cli_expect_scores <- function(scores, expected) {
  expect_named(scores, c("node_name", "pagerank", "cheirank"))
  expect_setequal(scores$node_name, expected$forward$node_name)
  fwd <- expected$forward
  chei <- expected$reverse
  expect_equal(
    scores$pagerank, fwd$pagerank[match(scores$node_name, fwd$node_name)]
  )
  expect_equal(
    scores$cheirank, chei$pagerank[match(scores$node_name, chei$node_name)]
  )
}

cli_small_edges <- function() {
  data.frame(
    from = c(
      "https://example.com/", "https://example.com/a", "https://example.com/b"
    ),
    to = c(
      "https://example.com/a", "https://example.com/b", "https://example.com/"
    ),
    placement = c("content", "nav", "content")
  )
}

describe("cli score: fixture round trip", {
  edges <- cli_fixture_edges("before")
  edges_csv <- cli_csv(edges)

  it("writes PageRank and CheiRank as pagerank() computes them", {
    scores <- cli_read(cli_run(c("score", edges_csv)))
    cli_expect_scores(scores, cli_expected_scores(edges))
    expect_identical(nrow(scores), 78L)
    expect_false(is.unsorted(rev(scores$pagerank)))
  })

  it("forwards --damping and --placement-weights", {
    weights <- c(
      content = 1, nav = 0.1, header = 0.1, footer = 0.1, aside = 0.1
    )
    scores <- cli_read(cli_run(c(
      "score", edges_csv, "--damping=0.7",
      "--placement-weights", "content=1,nav=0.1,header=0.1,footer=0.1,aside=0.1"
    )))
    cli_expect_scores(
      scores,
      cli_expected_scores(edges, placement_weights = weights, damping = 0.7)
    )
  })

  it("forwards --accepted-placements", {
    scores <- cli_read(cli_run(
      c("score", edges_csv, "--accepted-placements", "Content")
    ))
    cli_expect_scores(
      scores, cli_expected_scores(edges, accepted_placements = "content")
    )
    expect_identical(nrow(scores), 77L)
  })
})

describe("cli compare: fixture round trip", {
  before <- cli_fixture_edges("before")
  after <- cli_fixture_edges("after")
  before_scores <- cli_run(c("score", cli_csv(before)))
  after_scores <- cli_run(c("score", cli_csv(after)))
  expected_before <- cli_expected_scores(before)
  expected_after <- cli_expected_scores(after)

  # The CLI writes scores with 15 significant digits (?cli, "Precision"), so
  # the direct comparison runs on the in-memory scores rounded the same way:
  # nodes whose scores differ only by floating-point noise then tie in both.
  cli_direct_comparison <- function(direction, metric) {
    as_scores <- function(result) {
      out <- data.frame(node_name = result$node_name)
      out[[metric]] <- as.numeric(sprintf("%.15g", result$pagerank))
      out
    }
    compare_pagerank(
      as_scores(expected_before[[direction]]),
      as_scores(expected_after[[direction]]),
      pr_col = metric
    )
  }

  cli_expect_comparison <- function(cmp, expected) {
    expect_named(cmp, names(expected))
    expect_setequal(cmp$node_name, expected$node_name)
    rows <- match(cmp$node_name, expected$node_name)
    for (column in setdiff(names(expected), "node_name")) {
      expect_equal(cmp[[column]], expected[[column]][rows], label = column)
    }
  }

  it("writes compare_pagerank() output for PageRank as CSV", {
    cmp <- cli_read(cli_run(c("compare", before_scores, after_scores)))
    cli_expect_comparison(cmp, cli_direct_comparison("forward", "pagerank"))
    expect_identical(nrow(cmp), 96L)

    # The scores and deltas also match the unrounded comparison.
    unrounded <- compare_pagerank(
      expected_before$forward, expected_after$forward
    )
    rows <- match(cmp$node_name, unrounded$node_name)
    expect_equal(cmp$pagerank_b, unrounded$pagerank_b[rows])
    expect_equal(cmp$delta, unrounded$delta[rows])
  })

  it("compares CheiRank with --metric cheirank", {
    cmp <- cli_read(cli_run(
      c("compare", before_scores, after_scores, "--metric", "cheirank")
    ))
    cli_expect_comparison(cmp, cli_direct_comparison("reverse", "cheirank"))
  })

  it("writes the comparison and its summary as JSON", {
    expected <- cli_direct_comparison("forward", "pagerank")
    out <- cli_run(
      c("compare", before_scores, after_scores, "--format", "json")
    )
    json <- readLines(out)
    expect_length(json, 1L)
    expect_true(startsWith(json, "{\"metric\":\"pagerank\",\"summary\":{"))
    summary <- attr(expected, "summary")
    rho <- regmatches(json, regexpr("\"spearman_rho\":[^,]+", json))
    expect_equal(
      as.numeric(sub("^\"spearman_rho\":", "", rho)), summary$spearman_rho
    )
    expect_true(grepl(
      paste0(
        "\"nodes_gained\":", summary$nodes_gained,
        ",\"nodes_lost\":", summary$nodes_lost, "}"
      ),
      json,
      fixed = TRUE
    ))
    rows <- regmatches(json, gregexpr("\\{\"node_name\":[^}]*\\}", json))[[1L]]
    expect_length(rows, nrow(expected))
    first <- expected[1L, ]
    num <- function(x) sprintf("%.15g", x)
    expect_identical(
      rows[[1L]],
      paste0(
        "{\"node_name\":\"", first$node_name, "\"",
        ",\"pagerank_a\":", num(first$pagerank_a),
        ",\"pagerank_b\":", num(first$pagerank_b),
        ",\"delta\":", num(first$delta),
        ",\"pct_change\":", num(first$pct_change),
        ",\"rank_a\":", first$rank_a, ",\"rank_b\":", first$rank_b,
        ",\"rank_delta\":", first$rank_delta, "}"
      )
    )
  })
})

describe("cli output", {
  edges_csv <- cli_csv(cli_small_edges())

  it("writes to standard output without --out", {
    lines <- capture.output(res <- cli(c("score", edges_csv)))
    expect_identical(lines[[1L]], "\"node_name\",\"pagerank\",\"cheirank\"")
    scores <- utils::read.csv(text = lines, stringsAsFactors = FALSE)
    expect_equal(scores, res)
  })

  it("uses a weight column as edge weights", {
    edges <- cli_small_edges()[c("from", "to")]
    edges$weight <- c(1, 2, 3)
    scores <- cli_read(cli_run(c("score", cli_csv(edges))))
    expected <- pagerank(edges, weight_col = "weight")
    expect_equal(
      scores$pagerank,
      expected$pagerank[match(scores$node_name, expected$node_name)]
    )
  })

  it("writes missing comparison values as empty CSV cells and JSON null", {
    a <- cli_csv(data.frame(node_name = c("x", "y"), pagerank = c(0.6, 0.4)))
    b <- cli_csv(data.frame(node_name = c("x", "z"), pagerank = c(0.5, 0.5)))
    csv <- readLines(cli_run(c("compare", a, b)))
    expect_true(any(grepl("^\"y\",0.4,,", csv)))
    json <- readLines(cli_run(c("compare", a, b, "--format", "json")))
    expect_true(grepl(
      "\"node_name\":\"y\",\"pagerank_a\":0.4,\"pagerank_b\":null",
      json,
      fixed = TRUE
    ))
    expect_true(grepl("\"spearman_rho\":null", json, fixed = TRUE))
  })

  it("orders rows that print the same PageRank by node_name", {
    # 0.1 + 0.2 is one unit in the last place above 0.3; both print as 0.3.
    forward <- data.frame(
      node_name = c("b", "a", "c"), pagerank = c(0.1 + 0.2, 0.3, 0.4)
    )
    reverse <- data.frame(node_name = c("a", "b", "c"), pagerank = 1 / 3)
    scores <- .cli_join_scores(forward, reverse)
    expect_identical(scores$node_name, c("c", "a", "b"))
  })

  it("writes UTF-8 whatever the locale", {
    edges_utf8 <- withr::local_tempfile(fileext = ".csv")
    cafe <- "https://example.com/café"
    sep <- "https://example.com/a b"
    writeLines(
      enc2utf8(c("from,to", paste0(cafe, ",", sep), paste0(sep, ",", cafe))),
      edges_utf8,
      useBytes = TRUE
    )
    withr::local_locale(c(LC_CTYPE = "C"))
    skip_if_not(
      identical(Sys.getlocale("LC_CTYPE"), "C"), "cannot switch to the C locale"
    )
    utf8_bytes <- function(x) charToRaw(enc2utf8(x))
    has_bytes <- function(haystack, needle) {
      grepl(
        rawToChar(needle), rawToChar(haystack),
        fixed = TRUE, useBytes = TRUE
      )
    }

    scores_out <- cli_run(c("score", edges_utf8))
    bytes <- readBin(scores_out, "raw", file.size(scores_out))
    expect_true(has_bytes(bytes, utf8_bytes(paste0("\"", cafe, "\""))))
    expect_true(has_bytes(bytes, utf8_bytes(paste0("\"", sep, "\""))))
    expect_false(has_bytes(bytes, charToRaw("<U+")))

    stdout_lines <- capture.output(cli(c("score", edges_utf8)))
    stdout_bytes <- charToRaw(paste(stdout_lines, collapse = "\n"))
    expect_true(has_bytes(stdout_bytes, utf8_bytes(cafe)))
    expect_false(has_bytes(stdout_bytes, charToRaw("<U+")))

    json_out <- cli_run(
      c("compare", scores_out, scores_out, "--format", "json")
    )
    bytes <- readBin(json_out, "raw", file.size(json_out))
    expect_true(has_bytes(bytes, utf8_bytes(paste0("\"", cafe, "\""))))
    expect_true(has_bytes(bytes, utf8_bytes(paste0("\"", sep, "\""))))
    expect_false(has_bytes(bytes, charToRaw("<U+")))
  })

  it("writes a large CSV in linear time", {
    skip_on_cran()
    # `compare` cleans no URLs, so its run time is reading, joining and
    # writing; rendering CSV through a textConnection() took minutes here.
    n <- 150000L
    nodes <- sprintf("https://example.com/p%06d", seq_len(n))
    a <- cli_csv(data.frame(node_name = nodes, pagerank = seq_len(n) / n))
    b <- cli_csv(data.frame(node_name = nodes, pagerank = rev(seq_len(n)) / n))
    elapsed <- system.time(cmp_out <- cli_run(c("compare", a, b)))[["elapsed"]]
    expect_lt(elapsed, 10)
    expect_length(readLines(cmp_out), n + 1L)
  })

  it("prints usage for --help", {
    expect_output(res <- cli("--help"), "pagerankr score EDGES")
    expect_null(res)
    expect_output(cli(c("score", "--help")), "--damping")
    expect_output(cli(c("compare", "-h")), "--metric")
  })
})

describe("cli number and JSON encoding", {
  it("formats doubles with 15 significant digits", {
    expect_identical(
      .cli_format_num(c(0.1, 1 / 3, 1e-20, 0, 272.214825628921)),
      c("0.1", "0.333333333333333", "1e-20", "0", "272.214825628921")
    )
    # Noise past the 15th digit is gone, so near-equal scores tie.
    expect_identical(
      .cli_format_num(0.1 + 1e-17), .cli_format_num(0.1)
    )
    expect_identical(.cli_format_num(c(NA, Inf, NaN)), rep(NA_character_, 3L))
  })

  it("escapes JSON strings and encodes missing values as null", {
    expect_identical(
      .cli_json_values(c("a\"b", "x\\y", "l\nm\tn\r", "c\001d", NA)),
      c(
        "\"a\\\"b\"", "\"x\\\\y\"", "\"l\\nm\\tn\\r\"", "\"c\\u0001d\"",
        "null"
      )
    )
    expect_identical(
      .cli_json_values(c(TRUE, FALSE, NA)), c("true", "false", "null")
    )
    expect_identical(.cli_json_values(c(1L, NA)), c("1", "null"))
    expect_identical(.cli_json_rows(data.frame(a = numeric())), "[]")
  })
})

describe("cli argument errors", {
  edges_csv <- cli_csv(cli_small_edges())
  scores_csv <- cli_run(c("score", edges_csv))

  expect_usage_error <- function(args, pattern) {
    expect_error(cli(args), pattern, class = "pagerankr_cli_usage")
  }

  it("rejects a missing or unknown command", {
    expect_usage_error(character(), "no command")
    expect_usage_error("rank", "unknown command 'rank'")
    expect_usage_error(NA_character_, "character vector")
    expect_usage_error(1, "character vector")
  })

  it("rejects unknown, repeated and valueless options", {
    expect_usage_error(c("score", edges_csv, "--dampnig", "0.8"), "--dampnig")
    expect_usage_error(c("score", edges_csv, "--damping"), "needs a value")
    expect_usage_error(
      c("score", edges_csv, "--out", "a.csv", "--out=b.csv"),
      "more than once"
    )
    expect_usage_error(
      c("compare", scores_csv, scores_csv, "--format", "xml"),
      "--format must be one of csv, json, not 'xml'"
    )
    expect_usage_error(
      c("compare", scores_csv, scores_csv, "--metric", "hits"),
      "--metric"
    )
  })

  it("rejects the wrong number of files", {
    expect_usage_error("score", "takes 1 file")
    expect_usage_error(c("score", edges_csv, edges_csv), "got 2")
    expect_usage_error(c("compare", scores_csv), "takes 2 file")
  })

  it("rejects a malformed --damping", {
    expect_usage_error(c("score", edges_csv, "--damping", "abc"), "--damping")
    expect_usage_error(
      c("score", edges_csv, "--damping", "1.5"), "in \\[0, 1\\), not '1.5'"
    )
    expect_usage_error(c("score", edges_csv, "--damping", "-0.1"), "\\[0, 1\\)")
  })

  it("rejects --damping 1, which leaves the scores undefined", {
    expect_usage_error(
      c("score", edges_csv, "--damping", "1"), "in \\[0, 1\\), not '1'"
    )
    # 0 is in range: every node gets the teleport share.
    scores <- cli_read(cli_run(c("score", edges_csv, "--damping", "0")))
    expect_equal(scores$pagerank, rep(1 / 3, 3L))
  })

  it("reads -h as an option's value, not as a request for help", {
    expect_usage_error(
      c("score", edges_csv, "--damping", "-h"), "not '-h'"
    )
    expect_usage_error(
      c("compare", scores_csv, scores_csv, "--format", "--help"),
      "not '--help'"
    )
    # Where an option name is expected, -h and --help still print usage.
    expect_output(res <- cli(c("score", edges_csv, "--help")), "--damping")
    expect_null(res)
    expect_output(cli(c("score", "--damping", "0.5", "-h")), "--damping")
    expect_output(cli(c("score", "--damping=0.5", "-h")), "--damping")
  })

  it("ends the options at --", {
    withr::local_dir(withr::local_tempdir())
    utils::write.csv(cli_small_edges(), "--edges.csv", row.names = FALSE)
    out <- withr::local_tempfile(fileext = ".csv")
    cli(c("score", "--out", out, "--", "--edges.csv"))
    expect_identical(nrow(cli_read(out)), 3L)
    # After --, -h is a file name, not a request for help.
    err <- expect_error(cli(c("score", "--", "-h")), "no such file")
    expect_false(inherits(err, "pagerankr_cli_usage"))
    expect_usage_error(c("score", "--", "a.csv", "b.csv"), "got 2")
  })

  it("rejects placements outside the vocabulary", {
    expect_usage_error(
      c("score", edges_csv, "--accepted-placements", "content,sidebar"),
      "unknown placement\\(s\\) sidebar"
    )
    expect_usage_error(
      c("score", edges_csv, "--accepted-placements", ","),
      "at least one"
    )
    expect_usage_error(
      c("score", edges_csv, "--placement-weights", "body=1"),
      "unknown placement\\(s\\) body"
    )
  })

  it("rejects malformed --placement-weights", {
    expect_usage_error(
      c("score", edges_csv, "--placement-weights", "nav"),
      "placement=weight pairs, not 'nav'"
    )
    expect_usage_error(
      c("score", edges_csv, "--placement-weights", "nav=0"),
      "positive numbers, not '0'"
    )
    expect_usage_error(
      c("score", edges_csv, "--placement-weights", "nav=x"),
      "positive numbers"
    )
    expect_usage_error(
      c("score", edges_csv, "--placement-weights", "nav=1,NAV=2"),
      "more than once"
    )
  })
})

describe("cli input errors", {
  edges_csv <- cli_csv(cli_small_edges())
  no_placement_csv <- cli_csv(cli_small_edges()[c("from", "to")])

  expect_input_error <- function(args, pattern) {
    err <- expect_error(cli(args), pattern)
    expect_false(inherits(err, "pagerankr_cli_usage"))
  }

  it("reports a missing or unreadable file", {
    missing_file <- file.path(tempdir(), "no-such-edges.csv")
    expect_input_error(c("score", missing_file), "no such file")
    empty <- withr::local_tempfile(fileext = ".csv")
    file.create(empty)
    expect_input_error(c("score", empty), "cannot read edges file")
    expect_input_error(c("compare", missing_file, missing_file), "no such")
  })

  it("reports missing and non-numeric columns", {
    expect_input_error(
      c("score", cli_csv(data.frame(source = "a", to = "b"))),
      "no from column"
    )
    expect_input_error(
      c("score", cli_csv(data.frame(from = "a", to = "b", weight = "heavy"))),
      "non-numeric weight value: 'heavy'"
    )
    expect_input_error(
      c("compare", edges_csv, edges_csv),
      "no node_name, pagerank column"
    )
    bad_scores <- cli_csv(data.frame(node_name = "a", pagerank = "high"))
    expect_input_error(
      c("compare", bad_scores, bad_scores),
      "non-numeric pagerank value"
    )
  })

  it("reads only the columns it uses", {
    path <- cli_csv(data.frame(
      extra = "x", to = "b", from = "a", weight = 2, notes = "y"
    ))
    edges <- .cli_read_csv(
      path, "edges file",
      required = c("from", "to"), optional = c("placement", "weight")
    )
    expect_named(edges, c("to", "from", "weight"))
    expect_identical(edges$weight, "2")
  })

  it("reports blank cells and repeated nodes in a scores file", {
    good <- cli_csv(data.frame(node_name = c("x", "y"), pagerank = c(0.6, 0.4)))
    blank_score <- cli_csv(
      data.frame(node_name = c("x", "y"), pagerank = c(0.6, NA)),
      na = ""
    )
    expect_input_error(
      c("compare", good, blank_score),
      paste0(
        "scores file '", blank_score,
        "' has a blank pagerank cell in data row 2"
      )
    )
    blank_node <- cli_csv(
      data.frame(node_name = c(NA, "y"), pagerank = c(0.6, 0.4)),
      na = ""
    )
    expect_input_error(
      c("compare", blank_node, good), "has a blank node_name cell in data row 1"
    )
    repeated <- cli_csv(
      data.frame(node_name = c("x", "y", "x"), pagerank = c(0.5, 0.3, 0.2))
    )
    expect_input_error(
      c("compare", good, repeated),
      paste0("scores file '", repeated, "' lists node_name 'x' more than once")
    )
    # Only the compared metric's column has to be complete.
    blank_other <- cli_csv(
      data.frame(
        node_name = c("x", "y"), pagerank = c(0.6, 0.4), cheirank = c(0.5, NA)
      ),
      na = ""
    )
    expect_identical(
      nrow(cli_read(cli_run(c("compare", good, blank_other)))), 2L
    )
  })

  it("reports scores that are not finite numbers", {
    edges <- cli_small_edges()
    solver <- pagerank
    local_mocked_bindings(pagerank = function(..., reverse = FALSE) {
      out <- solver(..., reverse = reverse)
      if (reverse) out$pagerank[[2L]] <- NaN
      out
    })
    expect_input_error(
      c("score", cli_csv(edges)),
      "the cheirank of 1 of 3 nodes is not a finite number"
    )
  })

  it("needs a placement column for the placement options", {
    expect_input_error(
      c("score", no_placement_csv, "--accepted-placements", "content"),
      "--accepted-placements needs a 'placement' column"
    )
    weighted <- cli_small_edges()
    weighted$weight <- 1
    expect_input_error(
      c("score", cli_csv(weighted), "--placement-weights", "nav=0.1"),
      "cannot be combined with --placement-weights"
    )
  })

  it("reports an unwritable output file", {
    out <- file.path(tempdir(), "no-such-dir", "scores.csv")
    expect_input_error(
      c("score", edges_csv, "--out", out), "cannot write output file"
    )
  })
})

describe("the installed script", {
  it("runs both commands from a shell and exits by outcome", {
    skip_on_cran()
    script <- system.file("exec", "pagerankr", package = "pagerankr")
    skip_if_not(nzchar(script), "the pagerankr script is not installed")
    rscript <- file.path(R.home("bin"), "Rscript")
    # The script calls the installed package; the child process searches the
    # same libraries as this one. R CMD check's R_TESTS names a startup file
    # by a relative path every child R would try, and fail, to source.
    withr::local_envvar(
      R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
      R_TESTS = NA
    )
    probe <- system2(
      rscript,
      c("-e", shQuote("cat(exists('cli', asNamespace('pagerankr')))")),
      stdout = TRUE, stderr = FALSE
    )
    skip_if_not(
      identical(probe, "TRUE"), "the installed pagerankr predates cli()"
    )

    edges <- cli_fixture_edges("before")
    edges_csv <- cli_csv(edges)
    out <- withr::local_tempfile(fileext = ".csv")
    status <- system2(
      rscript, c(shQuote(script), "score", shQuote(edges_csv),
                 "--damping", "0.7", "--out", shQuote(out)),
      stdout = FALSE, stderr = FALSE
    )
    expect_equal(status, 0L)
    cli_expect_scores(cli_read(out), cli_expected_scores(edges, damping = 0.7))

    json <- system2(
      rscript, c(shQuote(script), "compare", shQuote(out), shQuote(out),
                 "--format", "json"),
      stdout = TRUE, stderr = FALSE
    )
    expect_true(startsWith(json[[1L]], "{\"metric\":\"pagerank\""))

    usage <- system2(
      rscript, c(shQuote(script), "score", shQuote(edges_csv), "--bogus"),
      stdout = FALSE, stderr = FALSE
    )
    expect_equal(usage, 2L)
    damping_one <- system2(
      rscript,
      c(shQuote(script), "score", shQuote(edges_csv), "--damping", "1"),
      stdout = FALSE, stderr = FALSE
    )
    expect_equal(damping_one, 2L)
    missing_input <- system2(
      rscript, c(shQuote(script), "score", shQuote(tempfile())),
      stdout = FALSE, stderr = FALSE
    )
    expect_equal(missing_input, 1L)
  })
})
