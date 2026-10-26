#' Command-line entry point: score an edge list or compare two score files
#'
#' @description
#' `cli()` is what the `pagerankr` script installed with the package runs, so
#' an edge list can be scored, and two score files compared, from a shell with
#' no interactive R session. It has two commands:
#'
#' * `score` reads an edge-list CSV and writes a scores CSV with the PageRank
#'   and the CheiRank of every node. CheiRank is [pagerank()] with
#'   `reverse = TRUE`: PageRank on the transposed graph, so a page that links
#'   out to much of the site scores high.
#' * `compare` reads two scores CSVs written by `score` and writes the
#'   [compare_pagerank()] result for one of the two scores, as CSV or JSON.
#'
#' Output is machine-readable and goes to standard output unless `--out FILE`
#' names a file. Input files are read as UTF-8 and output is written as UTF-8,
#' whatever the locale. The command never judges the scores: a run that
#' produces finite scores succeeds whatever the numbers are, and thresholds are
#' left to the caller.
#'
#' Every option takes a value, given as `--name value` or `--name=value`.
#' `-h` or `--help` where an option name is expected prints the usage; as an
#' option's value it is just that value. `--` ends the options: every argument
#' after it is a file, even one that starts with `-`.
#'
#' @param args A character vector of command-line arguments, the command
#'   first. Defaults to `commandArgs(trailingOnly = TRUE)`, the arguments the
#'   script was run with.
#'
#' @section Locating the script:
#' The script is installed in the package's `exec` directory; find it with
#' `system.file("exec", "pagerankr", package = "pagerankr")`. Run it with
#' `Rscript`, or directly where it is executable, since it starts with
#' `#!/usr/bin/env Rscript`:
#'
#' ```sh
#' PAGERANKR=$(Rscript -e 'cat(system.file("exec", "pagerankr",
#'   package = "pagerankr"))')
#' Rscript "$PAGERANKR" score edges.csv --out scores.csv
#' ```
#'
#' @section `score`:
#' `pagerankr score EDGES [--damping X] [--accepted-placements LIST]
#' [--placement-weights LIST] [--out FILE]`
#'
#' `EDGES` is a CSV file with a header row. It needs `from` and `to` columns
#' and may have a `placement` column (the region each link sits in) and a
#' numeric `weight` column; other columns are ignored. Scoring uses the
#' [pagerank()] defaults: URLs are cleaned, self-loops and isolates are
#' dropped and duplicate edges collapse.
#'
#' * `--damping X`: the damping factor, a number in `[0, 1)`. Default `0.85`.
#'   [pagerank()] itself accepts `1`, but with no teleport the scores are not
#'   well defined on most graphs (the solver can return `NaN`), so `score`
#'   refuses it.
#' * `--accepted-placements LIST`: comma-separated placements to keep, from
#'   `content`, `nav`, `header`, `footer` and `aside`. Edges placed elsewhere
#'   are dropped. Needs a `placement` column.
#' * `--placement-weights LIST`: comma-separated `placement=weight` pairs, for
#'   example `content=1,nav=0.1,header=0.1,footer=0.1,aside=0.1`. Placements
#'   not named keep weight 1. Needs a `placement` column and cannot be combined
#'   with a `weight` column.
#' * `--out FILE`: write the CSV to `FILE` instead of standard output.
#'
#' The output has one row per scored node, highest written PageRank first and
#' rows that print the same PageRank in `node_name` order:
#' \describe{
#'   \item{`node_name`}{The node, after URL cleaning.}
#'   \item{`pagerank`}{Its PageRank. The column sums to 1.}
#'   \item{`cheirank`}{Its CheiRank, PageRank with `reverse = TRUE`. The column
#'     sums to 1.}
#' }
#'
#' @section `compare`:
#' `pagerankr compare SCORES_A SCORES_B [--metric pagerank|cheirank]
#' [--format csv|json] [--out FILE]`
#'
#' `SCORES_A` (the baseline) and `SCORES_B` are CSV files with a `node_name`
#' column and the metric's column, as `score` writes them. Each `node_name`
#' appears once and no cell in either column is blank.
#'
#' * `--metric`: which score to compare, `pagerank` (default) or `cheirank`.
#' * `--format`: `csv` (default) or `json`.
#' * `--out FILE`: write to `FILE` instead of standard output.
#'
#' The CSV has the [compare_pagerank()] columns, with the metric's name in
#' place of `pagerank`: `node_name`, `<metric>_a`, `<metric>_b`, `delta`,
#' `pct_change`, `rank_a`, `rank_b` and `rank_delta`, largest absolute change
#' first. A node missing from one file has empty cells for that side.
#'
#' The JSON is one object: `metric` (the metric compared), `summary` (the
#' [compare_pagerank()] summary: `spearman_rho`, `mean_abs_delta`,
#' `nodes_gained` and `nodes_lost`) and `nodes`, an array with one object per
#' row of the CSV. A missing value is `null`.
#'
#' @section Precision:
#' Both commands write numbers with 15 significant digits, R's own CSV
#' precision. Nodes in equivalent positions in the graph get scores that differ
#' only by floating-point noise past that digit; written out, they are usually
#' equal, so `compare` ranks them as ties instead of ordering them by the
#' noise. Rounding makes a difference between platforms unlikely, not
#' impossible: two values either side of a rounding boundary still print
#' differently. A comparison of the written scores matches [compare_pagerank()]
#' run on the scores rounded to 15 significant digits. Its ranks and
#' `spearman_rho` are computed on the written, rounded values, so they can
#' differ from [compare_pagerank()] run on the full-precision scores wherever
#' scores differ only by floating-point noise.
#'
#' @section Exit status:
#' The script exits 0 on success, 2 on a usage error (an unknown command or
#' option, a missing or malformed value, a placement outside the vocabulary)
#' and 1 on any other error, such as an unreadable or malformed input file, a
#' blank or repeated cell in a scores file, or a score that is not a finite
#' number. It prints the error to standard error. Called from R, `cli()`
#' signals the error instead; a usage error has class `pagerankr_cli_usage`.
#'
#' @return Invisibly, the data frame written: the scores for `score`, the
#'   [compare_pagerank()] result for `compare`, or `NULL` for `--help`.
#'
#' @seealso [pagerank()], [compare_pagerank()]
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("https://example.com/", "https://example.com/a",
#'            "https://example.com/b"),
#'   to = c("https://example.com/a", "https://example.com/b",
#'          "https://example.com/")
#' )
#' edges_csv <- tempfile(fileext = ".csv")
#' utils::write.csv(edges, edges_csv, row.names = FALSE)
#' cli(c("score", edges_csv))
#'
#' scores_csv <- tempfile(fileext = ".csv")
#' cli(c("score", edges_csv, "--damping", "0.7", "--out", scores_csv))
#' cli(c("compare", scores_csv, scores_csv, "--format", "json"))
cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (!is.character(args) || anyNA(args)) {
    .cli_usage_error("`args` must be a character vector without NA.")
  }
  if (length(args) == 0L) {
    .cli_usage_error("no command given; use 'score' or 'compare'.")
  }
  command <- args[[1L]]
  rest <- args[-1L]
  if (command %in% c("-h", "--help", "help")) {
    return(.cli_help("main"))
  }
  switch(command,
    score = .cli_score(rest),
    compare = .cli_compare(rest),
    .cli_usage_error(
      "unknown command '", command, "'; use 'score' or 'compare'."
    )
  )
}

#' Signal a CLI usage error (the script maps its class to exit status 2)
#' @keywords internal
#' @noRd
.cli_usage_error <- function(...) {
  msg <- paste0(...)
  stop(errorCondition(msg, class = "pagerankr_cli_usage", call = NULL))
}

#' Usage text, per topic
#' @keywords internal
#' @noRd
.cli_usage <- function(topic = c("main", "score", "compare")) {
  topic <- match.arg(topic)
  score <- c(
    "pagerankr score EDGES [--damping X] [--accepted-placements LIST]",
    "                      [--placement-weights LIST] [--out FILE]",
    "  EDGES: CSV with from,to and optional placement and weight columns.",
    "  Writes node_name,pagerank,cheirank (CheiRank = reverse PageRank).",
    "  --damping X                damping factor in [0, 1) (default 0.85)",
    "  --accepted-placements LIST placements to keep, e.g. content,nav",
    "  --placement-weights LIST   e.g. content=1,nav=0.1,footer=0.1",
    "  Placements: content, nav, header, footer, aside."
  )
  compare <- c(
    "pagerankr compare SCORES_A SCORES_B [--metric pagerank|cheirank]",
    "                                    [--format csv|json] [--out FILE]",
    "  SCORES_A, SCORES_B: CSVs written by 'score'; A is the baseline.",
    "  Writes compare_pagerank() output for the metric (default pagerank)."
  )
  common <- c(
    "",
    "  --out FILE writes to FILE instead of standard output.",
    "  -- ends the options; every later argument is a file.",
    "  Help: ?pagerankr::cli"
  )
  body <- switch(topic,
    main = c(score, "", compare),
    score = score,
    compare = compare
  )
  c("Usage:", body, common)
}

#' Print usage to standard output
#' @keywords internal
#' @noRd
.cli_help <- function(topic) {
  writeLines(.cli_usage(topic))
  invisible(NULL)
}

#' Split one `--name` or `--name=value` argument
#' @keywords internal
#' @noRd
.cli_split_flag <- function(arg) {
  body <- substring(arg, 3L)
  eq <- regexpr("=", body, fixed = TRUE)
  if (eq < 0L) {
    return(list(name = body, value = NA_character_))
  }
  list(
    name = substring(body, 1L, eq - 1L),
    value = substring(body, eq + 1L)
  )
}

#' Does `args` ask for help where an option name is expected?
#'
#' `--name` without `=value` takes the next argument as its value, so a `-h`
#' there is a value, not a request for help; scanning stops at `--`.
#'
#' @keywords internal
#' @noRd
.cli_wants_help <- function(args) {
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg == "--") {
      return(FALSE)
    }
    if (arg %in% c("-h", "--help")) {
      return(TRUE)
    }
    takes_next <- startsWith(arg, "--") && !grepl("=", arg, fixed = TRUE)
    i <- i + if (takes_next) 2L else 1L
  }
  FALSE
}

#' Parse a command's arguments into positionals and option values
#'
#' Every option takes a value, given as `--name value` or `--name=value`.
#' `-h` / `--help` where an option name is expected returns `help = TRUE`;
#' `--` ends the options.
#'
#' @keywords internal
#' @noRd
.cli_parse <- function(args, options, n_positional, command) {
  if (.cli_wants_help(args)) {
    return(list(help = TRUE))
  }
  values <- list()
  positional <- character()
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    i <- i + 1L
    if (arg == "--") {
      positional <- c(positional, args[-seq_len(i - 1L)])
      break
    }
    if (!startsWith(arg, "--")) {
      positional <- c(positional, arg)
      next
    }
    flag <- .cli_split_flag(arg)
    .cli_check_option(flag$name, options, values, command)
    if (is.na(flag$value)) {
      if (i > length(args)) {
        .cli_usage_error("option --", flag$name, " needs a value.")
      }
      flag$value <- args[[i]]
      i <- i + 1L
    }
    values[[flag$name]] <- flag$value
  }
  if (length(positional) != n_positional) {
    .cli_usage_error(
      "'", command, "' takes ", n_positional, " file argument(s), got ",
      length(positional), "."
    )
  }
  list(help = FALSE, positional = positional, values = values)
}

#' @keywords internal
#' @noRd
.cli_check_option <- function(name, options, values, command) {
  if (!(name %in% options)) {
    .cli_usage_error(
      "unknown option --", name, " for '", command, "'; options: ",
      toString(paste0("--", options)), "."
    )
  }
  if (name %in% names(values)) {
    .cli_usage_error("option --", name, " is given more than once.")
  }
  invisible(NULL)
}

#' Parse `--damping`; `NULL` leaves pagerank()'s default in force
#' @keywords internal
#' @noRd
.cli_damping <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  damping <- suppressWarnings(as.numeric(value))
  if (is.na(damping) || damping < 0 || damping >= 1) {
    .cli_usage_error(
      "--damping must be a number in [0, 1), not '", value, "'."
    )
  }
  damping
}

#' Split a comma-separated option value into trimmed, non-empty items
#' @keywords internal
#' @noRd
.cli_split_list <- function(value, option) {
  items <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  items <- items[nzchar(items)]
  if (length(items) == 0L) {
    .cli_usage_error("--", option, " needs at least one item.")
  }
  items
}

#' Check placements against pagerank()'s vocabulary
#' @keywords internal
#' @noRd
.cli_check_placements <- function(placements, option) {
  allowed <- .pr_placement_vocabulary()
  unknown <- setdiff(placements, allowed)
  if (length(unknown) > 0L) {
    .cli_usage_error(
      "--", option, ": unknown placement(s) ", toString(unknown),
      "; use ", toString(allowed), "."
    )
  }
  invisible(NULL)
}

#' Parse `--accepted-placements`
#' @keywords internal
#' @noRd
.cli_accepted_placements <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  option <- "accepted-placements"
  placements <- unique(.pr_ascii_lower(.cli_split_list(value, option)))
  .cli_check_placements(placements, option)
  placements
}

#' Parse `--placement-weights` into a named numeric vector
#' @keywords internal
#' @noRd
.cli_placement_weights <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  option <- "placement-weights"
  items <- .cli_split_list(value, option)
  pairs <- strsplit(items, "=", fixed = TRUE)
  malformed <- lengths(pairs) != 2L
  if (any(malformed)) {
    .cli_usage_error(
      "--", option, " takes placement=weight pairs, not '",
      items[malformed][[1L]], "'."
    )
  }
  placements <- .pr_ascii_lower(trimws(vapply(pairs, `[[`, "", 1L)))
  raw_weights <- trimws(vapply(pairs, `[[`, "", 2L))
  .cli_check_placements(placements, option)
  if (anyDuplicated(placements) > 0L) {
    .cli_usage_error("--", option, " names a placement more than once.")
  }
  weights <- suppressWarnings(as.numeric(raw_weights))
  bad <- is.na(weights) | !is.finite(weights) | weights <= 0
  if (any(bad)) {
    .cli_usage_error(
      "--", option, ": weights must be positive numbers, not '",
      raw_weights[bad][[1L]], "'."
    )
  }
  names(weights) <- placements
  weights
}

#' Parse an option restricted to a fixed set; the first choice is the default
#' @keywords internal
#' @noRd
.cli_choice <- function(value, option, choices) {
  if (is.null(value)) {
    return(choices[[1L]])
  }
  if (!(value %in% choices)) {
    .cli_usage_error(
      "--", option, " must be one of ", toString(choices), ", not '", value,
      "'."
    )
  }
  value
}

#' Read the `required` and `optional` columns of a CSV input as character
#'
#' The header is read first, so the columns the command does not use are
#' skipped (`colClasses = "NULL"`) instead of read and dropped. Strings are
#' marked UTF-8 whatever the locale.
#'
#' @keywords internal
#' @noRd
.cli_read_csv <- function(path, what, required, optional = character()) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("cannot read ", what, " '", path, "': no such file.", call. = FALSE)
  }
  read <- function(...) {
    tryCatch(
      utils::read.csv(path, check.names = FALSE, encoding = "UTF-8", ...),
      error = function(e) {
        stop(
          "cannot read ", what, " '", path, "': ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  # nrows = 1, not 0: read.table() takes a non-positive nrows as "all rows".
  header <- names(read(nrows = 1L))
  .cli_require_columns(header, required, what, path)
  col_classes <- rep("NULL", length(header))
  wanted <- intersect(c(required, optional), header)
  col_classes[match(wanted, header)] <- "character"
  read(colClasses = col_classes, na.strings = "")
}

#' Stop unless the `header` names every column in `required`
#' @keywords internal
#' @noRd
.cli_require_columns <- function(header, required, what, path) {
  missing_cols <- setdiff(required, header)
  if (length(missing_cols) > 0L) {
    stop(
      what, " '", path, "' has no ", toString(missing_cols),
      " column; it has: ", toString(header), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Convert a column read as character to numeric, refusing non-numbers
#' @keywords internal
#' @noRd
.cli_numeric_column <- function(x, column, what, path) {
  out <- suppressWarnings(as.numeric(x))
  bad <- is.na(out) & !is.na(x)
  if (any(bad)) {
    stop(
      what, " '", path, "' has a non-numeric ", column, " value: '",
      x[bad][[1L]], "'.",
      call. = FALSE
    )
  }
  out
}

#' Read the `score` command's edge list
#' @keywords internal
#' @noRd
.cli_read_edges <- function(path) {
  what <- "edges file"
  edges <- .cli_read_csv(
    path, what,
    required = c("from", "to"), optional = c("placement", "weight")
  )
  edges <- edges[
    intersect(c("from", "to", "placement", "weight"), names(edges))
  ]
  if ("weight" %in% names(edges)) {
    edges$weight <- .cli_numeric_column(edges$weight, "weight", what, path)
  }
  edges
}

#' Read one of the `compare` command's score files
#' @keywords internal
#' @noRd
.cli_read_scores <- function(path, metric) {
  what <- "scores file"
  scores <- .cli_read_csv(path, what, required = c("node_name", metric))
  out <- data.frame(node_name = scores$node_name)
  out[[metric]] <- .cli_numeric_column(scores[[metric]], metric, what, path)
  for (column in names(out)) {
    if (anyNA(out[[column]])) {
      stop(
        what, " '", path, "' has a blank ", column, " cell in data row ",
        which(is.na(out[[column]]))[[1L]], ".",
        call. = FALSE
      )
    }
  }
  repeated <- anyDuplicated(out$node_name)
  if (repeated > 0L) {
    stop(
      what, " '", path, "' lists node_name '", out$node_name[[repeated]],
      "' more than once.",
      call. = FALSE
    )
  }
  out
}

#' Build the pagerank() arguments `score` shares between both directions
#' @keywords internal
#' @noRd
.cli_score_args <- function(edges, path, damping, accepted, weights) {
  has_placement <- "placement" %in% names(edges)
  has_weight <- "weight" %in% names(edges)
  placement_flags <- c(
    "--accepted-placements" = !is.null(accepted),
    "--placement-weights" = !is.null(weights)
  )
  if (any(placement_flags) && !has_placement) {
    stop(
      toString(names(placement_flags)[placement_flags]), " needs a ",
      "'placement' column in edges file '", path, "'.",
      call. = FALSE
    )
  }
  if (!is.null(weights) && has_weight) {
    stop(
      "edges file '", path, "' has a 'weight' column, which cannot be ",
      "combined with --placement-weights.",
      call. = FALSE
    )
  }
  args <- list(
    edge_list_df = edges,
    placement_col = if (has_placement) "placement",
    weight_col = if (has_weight) "weight",
    accepted_placements = accepted,
    placement_weights = weights
  )
  if (!is.null(damping)) {
    args$damping <- damping
  }
  args
}

#' Stop unless every score `pagerank()` returned is a finite number
#' @keywords internal
#' @noRd
.cli_check_finite <- function(result, metric) {
  bad <- !is.finite(result$pagerank)
  if (any(bad)) {
    stop(
      "the ", metric, " of ", sum(bad), " of ", length(bad), " nodes is not ",
      "a finite number; the solver did not produce usable scores.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Join forward PageRank and CheiRank, highest written PageRank first
#'
#' Rows are ordered on the PageRank as written (15 significant digits), then
#' on `node_name`, so rows that print the same score are not ordered by the
#' noise past the 15th digit.
#'
#' @keywords internal
#' @noRd
.cli_join_scores <- function(forward, reverse) {
  scores <- merge(
    data.frame(node_name = forward$node_name, pagerank = forward$pagerank),
    data.frame(node_name = reverse$node_name, cheirank = reverse$pagerank),
    by = "node_name", all = TRUE
  )
  written <- as.numeric(.cli_format_num(scores$pagerank))
  scores <- scores[
    order(-written, scores$node_name, method = "radix"), ,
    drop = FALSE
  ]
  row.names(scores) <- NULL
  scores
}

#' The `score` command
#' @keywords internal
#' @noRd
.cli_score <- function(args) {
  opts <- .cli_parse(
    args,
    options = c(
      "damping", "accepted-placements", "placement-weights", "out"
    ),
    n_positional = 1L,
    command = "score"
  )
  if (opts$help) {
    return(.cli_help("score"))
  }
  values <- opts$values
  damping <- .cli_damping(values$damping)
  accepted <- .cli_accepted_placements(values[["accepted-placements"]])
  weights <- .cli_placement_weights(values[["placement-weights"]])
  path <- opts$positional[[1L]]
  edges <- .cli_read_edges(path)

  pr_args <- .cli_score_args(edges, path, damping, accepted, weights)
  forward <- do.call(pagerank, pr_args)
  .cli_check_finite(forward, "pagerank")
  reverse <- do.call(pagerank, c(pr_args, list(reverse = TRUE)))
  .cli_check_finite(reverse, "cheirank")
  scores <- .cli_join_scores(forward, reverse)
  .cli_write(.cli_csv_lines(scores), values$out)
  invisible(scores)
}

#' The `compare` command
#' @keywords internal
#' @noRd
.cli_compare <- function(args) {
  opts <- .cli_parse(
    args,
    options = c("metric", "format", "out"),
    n_positional = 2L,
    command = "compare"
  )
  if (opts$help) {
    return(.cli_help("compare"))
  }
  values <- opts$values
  metric <- .cli_choice(values$metric, "metric", c("pagerank", "cheirank"))
  format <- .cli_choice(values$format, "format", c("csv", "json"))
  scores_a <- .cli_read_scores(opts$positional[[1L]], metric)
  scores_b <- .cli_read_scores(opts$positional[[2L]], metric)

  result <- compare_pagerank(scores_a, scores_b, pr_col = metric)
  lines <- if (format == "json") {
    .cli_json_comparison(result, metric)
  } else {
    .cli_csv_lines(result)
  }
  .cli_write(lines, values$out)
  invisible(result)
}

#' Format doubles with 15 significant digits; non-finite values become `NA`
#'
#' Fifteen digits is R's own CSV precision. It is deliberately not the 17 a
#' double needs to round-trip: scores of nodes in equivalent graph positions
#' differ only by floating-point noise past the 15th digit, and on platforms
#' where R reads decimals with double rather than extended precision a 17-digit
#' value can come back one unit in the last place off. Either way the ranks
#' `compare` computes from such near-ties would depend on the noise and the
#' platform; at 15 digits they are ties.
#'
#' @keywords internal
#' @noRd
.cli_format_num <- function(x) {
  out <- sprintf("%.15g", x)
  out[!is.finite(x)] <- NA_character_
  out
}

#' Quote strings for CSV, doubling embedded quotes as `write.csv()` does
#' @keywords internal
#' @noRd
.cli_csv_quote <- function(x) {
  paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")
}

#' Encode one column as CSV cells; a missing value is an empty cell
#' @keywords internal
#' @noRd
.cli_csv_cells <- function(x) {
  out <- if (is.character(x)) {
    .cli_csv_quote(x)
  } else if (is.double(x)) {
    .cli_format_num(x)
  } else {
    as.character(x)
  }
  out[is.na(x) | is.na(out)] <- ""
  out
}

#' Render a data frame as CSV lines, doubles at 15 significant digits
#'
#' The layout is `write.csv(row.names = FALSE, na = "")` with only character
#' columns quoted, built with vectorized `paste()`: writing through a
#' `textConnection()` grows its result one line at a time, which is quadratic
#' in the row count.
#'
#' @keywords internal
#' @noRd
.cli_csv_lines <- function(df) {
  header <- paste(.cli_csv_quote(names(df)), collapse = ",")
  if (nrow(df) == 0L) {
    return(header)
  }
  cells <- lapply(unname(df), .cli_csv_cells)
  c(header, do.call(paste, c(cells, sep = ",")))
}

#' Escape strings for a JSON string literal
#' @keywords internal
#' @noRd
.cli_json_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  controls <- gregexpr("[\001-\037]", x)
  regmatches(x, controls) <- lapply(
    regmatches(x, controls),
    function(chars) sprintf("\\u%04x", vapply(chars, utf8ToInt, 1L))
  )
  x
}

#' Encode an atomic vector as JSON values; `NA` becomes `null`
#' @keywords internal
#' @noRd
.cli_json_values <- function(x) {
  out <- if (is.character(x)) {
    paste0("\"", .cli_json_escape(x), "\"")
  } else if (is.logical(x)) {
    ifelse(x, "true", "false")
  } else if (is.integer(x)) {
    as.character(x)
  } else {
    .cli_format_num(x)
  }
  out[is.na(x) | is.na(out)] <- "null"
  out
}

#' Encode a named list of scalars as a JSON object
#' @keywords internal
#' @noRd
.cli_json_object <- function(x) {
  fields <- vapply(x, .cli_json_values, "")
  paste0(
    "{", paste0(.cli_json_values(names(x)), ":", fields, collapse = ","), "}"
  )
}

#' Encode a data frame as a JSON array of row objects
#' @keywords internal
#' @noRd
.cli_json_rows <- function(df) {
  if (nrow(df) == 0L) {
    return("[]")
  }
  keys <- .cli_json_values(names(df))
  cells <- Map(
    function(key, column) paste0(key, ":", .cli_json_values(column)),
    keys, df
  )
  rows <- paste0("{", do.call(paste, c(unname(cells), sep = ",")), "}")
  paste0("[", paste(rows, collapse = ","), "]")
}

#' Encode a compare_pagerank() result, with its summary, as JSON
#' @keywords internal
#' @noRd
.cli_json_comparison <- function(result, metric) {
  paste0(
    "{\"metric\":", .cli_json_values(metric),
    ",\"summary\":", .cli_json_object(attr(result, "summary")),
    ",\"nodes\":", .cli_json_rows(result), "}"
  )
}

#' Write lines as UTF-8 to `out`, or to standard output when `out` is `NULL`
#'
#' The lines are converted to UTF-8 and written as bytes, with no conversion
#' by the connection: translating to the native encoding first would turn
#' every character a C or POSIX locale cannot represent into `<U+00E9>`.
#'
#' @keywords internal
#' @noRd
.cli_write <- function(lines, out) {
  lines <- enc2utf8(lines)
  if (is.null(out)) {
    writeLines(lines, stdout(), useBytes = TRUE)
    return(invisible(NULL))
  }
  con <- tryCatch(
    file(out, open = "w", encoding = "native.enc"),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(con)) {
    stop("cannot write output file '", out, "'.", call. = FALSE)
  }
  on.exit(close(con))
  writeLines(lines, con, useBytes = TRUE)
  invisible(NULL)
}
