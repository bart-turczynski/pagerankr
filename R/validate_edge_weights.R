#' Validate Edge Weights and Per-Source Totals
#'
#' Inspect a weighted edge list before it reaches the PageRank solver. The
#' report identifies negative and non-finite weights, degenerate sources whose
#' outgoing weights are all zero, and (optionally) source totals that do not
#' match an expected probability total.
#'
#' @param edge_list_df A data frame containing source and weight columns.
#' @param weight_col Name of the numeric edge-weight column.
#' @param from_col Name of the source-node column used to define outgoing
#'   choice sets.
#' @param expected_total Optional finite, non-negative total expected for each
#'   source. Use `1` to validate an already-normalized transition-probability
#'   column. `NULL` (default) reports totals without enforcing a target.
#' @param tolerance Non-negative numeric tolerance for `expected_total`.
#' @param action How validation failures are handled: `"error"` (default),
#'   `"warning"`, or `"none"`. The report is returned in every mode.
#'
#' @return A data frame with one row per source and columns describing edge
#'   count, weight total, invalid-value counts, all-zero status, optional total
#'   agreement, and overall validity.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "A", "B"),
#'   to = c("B", "C", "C"),
#'   probability = c(0.25, 0.75, 1)
#' )
#' validate_edge_weights(
#'   edges,
#'   weight_col = "probability",
#'   expected_total = 1
#' )
validate_edge_weights <- function(edge_list_df,
                                  weight_col = "weight",
                                  from_col = "from",
                                  expected_total = NULL,
                                  tolerance = sqrt(.Machine$double.eps),
                                  action = c("error", "warning", "none")) {
  action <- match.arg(action)

  vew_validate_args(
    edge_list_df, weight_col, from_col, expected_total, tolerance
  )

  if (nrow(edge_list_df) == 0) {
    return(vew_empty_report())
  }

  sources <- as.character(edge_list_df[[from_col]])
  weights <- edge_list_df[[weight_col]]
  unique_sources <- unique(sources)

  report <- vew_build_report(
    unique_sources, sources, weights, expected_total, tolerance
  )

  if (all(report$valid)) {
    return(report)
  }
  if (action == "none") {
    return(report)
  }

  message <- vew_problem_message(report, weight_col, expected_total, tolerance)
  if (action == "error") {
    stop(message, call. = FALSE)
  }
  warning(message, call. = FALSE)

  report
}

#' Validate scalar arguments to [validate_edge_weights()]
#'
#' Runs every input guard in the original short-circuit order, raising the
#' same error text as the monolithic validator.
#' @noRd
vew_validate_args <- function(edge_list_df, weight_col, from_col,
                              expected_total, tolerance) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  vew_check_col_name(weight_col, "weight_col")
  vew_check_col_name(from_col, "from_col")

  missing_cols <- setdiff(c(from_col, weight_col), names(edge_list_df))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in `edge_list_df`: ",
      toString(missing_cols),
      call. = FALSE
    )
  }
  if (!is.numeric(edge_list_df[[weight_col]])) {
    stop("`weight_col` '", weight_col, "' must be numeric.", call. = FALSE)
  }
  vew_check_expected_total(expected_total)
  vew_check_tolerance(tolerance)
  invisible(NULL)
}

#' Validate a single-column-name argument
#' @noRd
vew_check_col_name <- function(value, arg_name) {
  msg <- sprintf(
    "`%s` must be a single non-empty column name.", arg_name
  )
  if (!is.character(value)) {
    stop(msg, call. = FALSE)
  }
  if (length(value) != 1) {
    stop(msg, call. = FALSE)
  }
  if (is.na(value)) {
    stop(msg, call. = FALSE)
  }
  if (!nzchar(value)) {
    stop(msg, call. = FALSE)
  }
  invisible(NULL)
}

#' Validate the optional `expected_total` argument
#' @noRd
vew_check_expected_total <- function(expected_total) {
  if (is.null(expected_total)) {
    return(invisible(NULL))
  }
  msg <- "`expected_total` must be NULL or one finite non-negative number."
  if (!is.numeric(expected_total)) {
    stop(msg, call. = FALSE)
  }
  if (length(expected_total) != 1) {
    stop(msg, call. = FALSE)
  }
  if (is.na(expected_total)) {
    stop(msg, call. = FALSE)
  }
  if (!is.finite(expected_total)) {
    stop(msg, call. = FALSE)
  }
  if (expected_total < 0) {
    stop(msg, call. = FALSE)
  }
  invisible(NULL)
}

#' Validate the `tolerance` argument
#' @noRd
vew_check_tolerance <- function(tolerance) {
  msg <- "`tolerance` must be one finite non-negative number."
  if (!is.numeric(tolerance)) {
    stop(msg, call. = FALSE)
  }
  if (length(tolerance) != 1) {
    stop(msg, call. = FALSE)
  }
  if (is.na(tolerance)) {
    stop(msg, call. = FALSE)
  }
  if (!is.finite(tolerance)) {
    stop(msg, call. = FALSE)
  }
  if (tolerance < 0) {
    stop(msg, call. = FALSE)
  }
  invisible(NULL)
}

#' Empty per-source report skeleton
#' @noRd
vew_empty_report <- function() {
  data.frame(
    source = character(0),
    n_edges = integer(0),
    total = numeric(0),
    n_negative = integer(0),
    n_na = integer(0),
    n_nan = integer(0),
    n_infinite = integer(0),
    all_zero = logical(0),
    total_ok = logical(0),
    valid = logical(0)
  )
}

#' Summarise one source's outgoing weights into a report row
#' @noRd
vew_summarize_source <- function(source, sources, weights,
                                 expected_total, tolerance) {
  idx <- if (is.na(source)) {
    is.na(sources)
  } else {
    !is.na(sources) & sources == source
  }
  values <- weights[idx]
  finite_values <- values[is.finite(values)]
  has_invalid <- !all(is.finite(values))
  total <- if (has_invalid) NA_real_ else sum(values)
  all_zero <- length(values) > 0 && !has_invalid && all(values == 0)
  total_ok <- if (is.null(expected_total)) {
    NA
  } else {
    !has_invalid && abs(total - expected_total) <= tolerance
  }

  data.frame(
    source = source,
    n_edges = length(values),
    total = total,
    n_negative = sum(finite_values < 0),
    n_na = sum(is.na(values) & !is.nan(values)),
    n_nan = sum(is.nan(values)),
    n_infinite = sum(is.infinite(values)),
    all_zero = all_zero,
    total_ok = total_ok
  )
}

#' Assemble the full per-source report with a `valid` flag
#' @noRd
vew_build_report <- function(unique_sources, sources, weights,
                             expected_total, tolerance) {
  report_rows <- lapply(
    unique_sources,
    vew_summarize_source,
    sources = sources,
    weights = weights,
    expected_total = expected_total,
    tolerance = tolerance
  )

  report <- do.call(rbind, report_rows)
  rownames(report) <- NULL
  report$valid <- report$n_negative == 0 &
    report$n_na == 0 &
    report$n_nan == 0 &
    report$n_infinite == 0 &
    !report$all_zero &
    (is.na(report$total_ok) | report$total_ok)
  report
}

#' Build the human-readable failure message from a report
#' @noRd
vew_problem_message <- function(report, weight_col, expected_total, tolerance) {
  problems <- character(0)
  n_negative <- sum(report$n_negative)
  n_non_finite <- sum(report$n_na + report$n_nan + report$n_infinite)
  n_all_zero <- sum(report$all_zero)
  n_bad_total <- if (is.null(expected_total)) 0L else sum(!report$total_ok)

  if (n_negative > 0) {
    problems <- c(problems, sprintf("%d negative weight(s)", n_negative))
  }
  if (n_non_finite > 0) {
    problems <- c(
      problems,
      sprintf(
        "%d non-finite weight(s): %d NA, %d NaN, %d Inf",
        n_non_finite,
        sum(report$n_na),
        sum(report$n_nan),
        sum(report$n_infinite)
      )
    )
  }
  if (n_all_zero > 0) {
    problems <- c(
      problems,
      sprintf("%d source(s) with all-zero outgoing weights", n_all_zero)
    )
  }
  if (n_bad_total > 0) {
    problems <- c(
      problems,
      sprintf(
        "%d source total(s) outside %.6g +/- %.6g",
        n_bad_total,
        expected_total,
        tolerance
      )
    )
  }

  paste0(
    "Invalid edge weights in `", weight_col, "`: ",
    paste(problems, collapse = "; "),
    ". Inspect `validate_edge_weights()` for per-source details."
  )
}
