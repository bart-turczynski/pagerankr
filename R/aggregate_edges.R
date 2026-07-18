#' @title Aggregate Duplicate Edges After Folding
#' @description Collapses duplicate `from -> to` rows in an edge list using
#'   explicit, per-column aggregation semantics. This is the post-fold
#'   aggregation step intended to run *after* redirect / canonical folding has
#'   coalesced URL variants onto their representatives, and it is a
#'   loss-aware alternative to [get_unique_edges()].
#'
#'   Where [get_unique_edges()] dedups a `from/to` pair by **keeping the first
#'   row** (silently discarding everything else on the duplicate rows),
#'   `aggregate_edges()` combines the duplicate rows column-by-column. This
#'   matters the moment edges carry quantities: click counts, predicted click
#'   propensities, repeated links to the same destination, and conflicting
#'   follow / nofollow metadata are all preserved or combined deterministically
#'   instead of dropped.
#'
#' @details
#' ## Default per-column semantics
#'
#' For every column other than `from_col` / `to_col` (and any column named in
#' `preserve_cols`), an aggregation is chosen automatically unless overridden in
#' `agg`:
#'
#' - **numeric / integer columns** (additive counts and click propensities) are
#'   summed. Repeated link instances to the same destination therefore add their
#'   propensities together: multiple slots pointing at one target produce more
#'   total propensity, which is the correct behavioral reading.
#' - **logical columns** (boolean attributes such as `nofollow`) are resolved
#'   with an explicit *conflict policy* (see `nofollow_policy`). They are never
#'   silently first-wins.
#' - **all other columns** (character, factor, ...) fall back to `"first"`,
#'   which reproduces the legacy keep-first behavior for non-additive
#'   identifier-like columns.
#'
#' ## Overriding per column
#'
#' `agg` is a named list mapping a column name to either:
#'
#' - one of the built-in strings `"sum"`, `"mean"`, `"max"`, `"min"`,
#'   `"first"`, `"last"`, `"any"`, `"all"`, `"majority"`, or `"error"`, or
#' - a function taking the vector of grouped values and returning a length-1
#'   value.
#'
#' The boolean conflict policies (`"any"`, `"all"`, `"majority"`, `"error"`)
#' may be applied to any logical column. `"error"` raises if a `from/to` group
#' holds conflicting (mixed `TRUE`/`FALSE`) values; the others reduce to
#' "any TRUE", "all TRUE", and the majority value (ties resolve to `TRUE`)
#' respectively.
#'
#' ## Preserving placement features
#'
#' Columns named in `preserve_cols` are **not** collapsed. Each surviving
#' `from/to` group keeps the individual per-instance values as a list-column
#' (one list element per group, holding that group's vector of values). This
#' lets placement / position features survive aggregation so a later
#' reasonable-surfer model can use each individual link instance.
#'
#' ## Backward compatibility
#'
#' With no weight or extra columns (a plain `from`/`to` edge list), the result
#' is identical to [get_unique_edges()]: NA edges dropped, self-loops handled
#' per `self_loops`, one row per unique `from/to` pair, from/to coerced to
#' character.
#'
#' @param edge_list_df A data frame representing the edge list, with at least
#'   the `from_col` and `to_col` columns.
#' @param agg A named list of per-column aggregation overrides. Names are
#'   column names; values are either a built-in aggregation string or a
#'   function. Columns not listed use the defaults described above. Default
#'   `list()`.
#' @param nofollow_policy The default conflict policy applied to logical
#'   columns that are not explicitly listed in `agg`. One of `"any"` (default),
#'   `"all"`, `"majority"`, or `"error"`. Named `nofollow_policy` because the
#'   nofollow flag is the canonical boolean attribute, but it governs every
#'   un-overridden logical column.
#' @param preserve_cols Character vector of columns to keep un-collapsed as
#'   per-group list-columns (e.g. placement / position features). Default
#'   `character(0)`.
#' @param self_loops How to handle self-loops (`a -> a`). One of `"drop"`
#'   (default) or `"keep"`.
#' @param from_col Name of the source-node column. Default `"from"`.
#' @param to_col Name of the target-node column. Default `"to"`.
#'
#' @return A data frame with one row per unique `from/to` pair (self-loops
#'   handled per `self_loops`). `from_col` / `to_col` are coerced to character;
#'   each remaining column is aggregated per its resolved rule; `preserve_cols`
#'   become list-columns. Row order follows first appearance of each
#'   `from/to` pair in the (NA-filtered, self-loop-handled) input.
#'
#' @seealso [get_unique_edges()] for the lossy keep-first dedup.
#' @export
#' @examples
#' # Click counts to the same destination sum instead of being dropped.
#' edges <- data.frame(
#'   from = c("A", "A", "B"),
#'   to = c("B", "B", "C"),
#'   clicks = c(3, 5, 2),
#'   nofollow = c(FALSE, TRUE, FALSE)
#' )
#' aggregate_edges(edges)
#'
#' # Require agreement on nofollow, erroring on a conflict.
#' try(aggregate_edges(edges, nofollow_policy = "error"))
#'
#' # Preserve placement features as a list-column for later modeling.
#' edges_pos <- data.frame(
#'   from = c("A", "A"),
#'   to = c("B", "B"),
#'   position = c(1, 7)
#' )
#' aggregate_edges(edges_pos, preserve_cols = "position")

aggregate_edges <- function(edge_list_df,
                            agg = list(),
                            nofollow_policy = c(
                              "any", "all", "majority", "error"
                            ),
                            preserve_cols = character(0),
                            self_loops = c("drop", "keep"),
                            from_col = "from",
                            to_col = "to") {
  # --- Argument matching and basic validation ---
  self_loops <- match.arg(self_loops)
  nofollow_policy <- match.arg(nofollow_policy)

  .validate_aggregate_inputs(edge_list_df, agg, preserve_cols, from_col, to_col)

  # Empty input: return as-is (mirrors get_unique_edges()), reconstructing
  # the from/to columns when the input is a bare data.frame().
  if (nrow(edge_list_df) == 0) {
    return(.aggregate_empty_input(edge_list_df, from_col, to_col))
  }

  edge_list_df <- .prepare_edge_rows(edge_list_df, self_loops, from_col, to_col)

  # If nothing remains, return an empty frame preserving column structure.
  if (nrow(edge_list_df) == 0) {
    empty_result_df <- edge_list_df[FALSE, , drop = FALSE]
    row.names(empty_result_df) <- NULL
    return(empty_result_df)
  }

  key_cols <- c(from_col, to_col)
  value_cols <- setdiff(names(edge_list_df), key_cols)

  # Validate that requested columns actually exist.
  .validate_agg_cols(agg, preserve_cols, value_cols)

  collapse_cols <- setdiff(value_cols, preserve_cols)

  # Resolve grouping. Group key preserves first-appearance order.
  row_groups <- .build_row_groups(edge_list_df, from_col, to_col)

  # from/to take the first value of each group (constant within a group).
  result <- .init_result_frame(edge_list_df, row_groups, from_col, to_col)

  # Aggregate the collapsible columns.
  result <- .aggregate_collapse_cols(
    result, edge_list_df, collapse_cols, agg, nofollow_policy, row_groups
  )

  # Preserve columns as per-group list-columns.
  result <- .preserve_group_cols(
    result, edge_list_df, preserve_cols, row_groups
  )

  # Restore original column order (from/to/value columns).
  result <- result[, names(edge_list_df), drop = FALSE]
  row.names(result) <- NULL
  result
}

#' Validate the top-level arguments to [aggregate_edges()].
#' @keywords internal
#' @noRd
.validate_aggregate_inputs <- function(edge_list_df, agg, preserve_cols,
                                       from_col, to_col) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!is.list(agg)) {
    stop("`agg` must be a named list.", call. = FALSE)
  }
  if (length(agg) > 0 && is.null(names(agg))) {
    stop("`agg` must be a named list.", call. = FALSE)
  }
  if (!is.character(preserve_cols)) {
    stop("`preserve_cols` must be a character vector.", call. = FALSE)
  }
  if (nrow(edge_list_df) == 0) {
    return(invisible(NULL))
  }
  if (!all(c(from_col, to_col) %in% names(edge_list_df))) {
    stop(
      "`edge_list_df` must have specified 'from' and 'to' ",
      "columns if not empty.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Handle an empty edge list, reconstructing from/to on a bare data frame.
#' @keywords internal
#' @noRd
.aggregate_empty_input <- function(edge_list_df, from_col, to_col) {
  if (ncol(edge_list_df) == 0) {
    empty_df <- data.frame()
    empty_df[[from_col]] <- character(0)
    empty_df[[to_col]] <- character(0)
    return(empty_df)
  }
  edge_list_df
}

#' Drop NA edges, coerce from/to to character, and handle self-loops.
#' @keywords internal
#' @noRd
.prepare_edge_rows <- function(edge_list_df, self_loops, from_col, to_col) {
  # Drop any edge where from or to is NA.
  edge_list_df <- edge_list_df[
    !is.na(edge_list_df[[from_col]]) & !is.na(edge_list_df[[to_col]]), ,
    drop = FALSE
  ]

  # Coerce from/to to character (handles factors).
  edge_list_df[[from_col]] <- as.character(edge_list_df[[from_col]])
  edge_list_df[[to_col]] <- as.character(edge_list_df[[to_col]])

  # Handle self-loops.
  if (self_loops == "drop") {
    is_self_loop <- edge_list_df[[from_col]] == edge_list_df[[to_col]]
    edge_list_df <- edge_list_df[!is_self_loop, , drop = FALSE]
  }
  edge_list_df
}

#' Validate that `agg` / `preserve_cols` name real, non-overlapping columns.
#' @keywords internal
#' @noRd
.validate_agg_cols <- function(agg, preserve_cols, value_cols) {
  unknown_agg <- setdiff(names(agg), value_cols)
  if (length(unknown_agg) > 0) {
    stop(
      "`agg` names not found among aggregatable columns: ",
      toString(unknown_agg),
      call. = FALSE
    )
  }
  unknown_preserve <- setdiff(preserve_cols, value_cols)
  if (length(unknown_preserve) > 0) {
    stop(
      "`preserve_cols` not found among aggregatable columns: ",
      toString(unknown_preserve),
      call. = FALSE
    )
  }
  both <- intersect(names(agg), preserve_cols)
  if (length(both) > 0) {
    stop(
      "Columns cannot be in both `agg` and `preserve_cols`: ",
      toString(both),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Split row indices into first-appearance-ordered from/to groups.
#' @keywords internal
#' @noRd
.build_row_groups <- function(edge_list_df, from_col, to_col) {
  group_key <- paste(
    edge_list_df[[from_col]], edge_list_df[[to_col]],
    sep = "\r"
  )
  group_factor <- factor(group_key, levels = unique(group_key))
  split(seq_len(nrow(edge_list_df)), group_factor)
}

#' Build the result frame seeded with one from/to row per group.
#' @keywords internal
#' @noRd
.init_result_frame <- function(edge_list_df, row_groups, from_col, to_col) {
  first_idx <- vapply(row_groups, function(idx) idx[[1L]], integer(1))
  result <- data.frame(
    edge_list_df[[from_col]][first_idx],
    edge_list_df[[to_col]][first_idx],
    check.names = FALSE
  )
  names(result) <- c(from_col, to_col)
  result
}

#' Reduce each collapsible column to one value per from/to group.
#' @keywords internal
#' @noRd
.aggregate_collapse_cols <- function(result, edge_list_df, collapse_cols,
                                     agg, nofollow_policy, row_groups) {
  for (col in collapse_cols) {
    rule <- if (col %in% names(agg)) agg[[col]] else NULL
    fun <- resolve_agg_fun(rule, edge_list_df[[col]], nofollow_policy, col)
    values <- edge_list_df[[col]]
    reduced <- lapply(row_groups, function(idx) fun(values[idx]))
    if (!all(vapply(reduced, length, integer(1)) == 1L)) {
      stop(
        "Aggregation for column '", col,
        "' did not return a single value per group.",
        call. = FALSE
      )
    }
    result[[col]] <- unlist(reduced, use.names = FALSE)
  }
  result
}

#' Attach preserved columns as per-group list-columns.
#' @keywords internal
#' @noRd
.preserve_group_cols <- function(result, edge_list_df, preserve_cols,
                                 row_groups) {
  for (col in preserve_cols) {
    values <- edge_list_df[[col]]
    result[[col]] <- unname(lapply(row_groups, function(idx) values[idx]))
  }
  result
}

#' Resolve a per-column aggregation rule into a reducing function.
#' @keywords internal
#' @noRd
resolve_agg_fun <- function(rule, column, nofollow_policy, col_name) {
  if (is.function(rule)) {
    return(rule)
  }

  if (is.null(rule)) {
    # Choose a default based on column type.
    if (is.logical(column)) {
      rule <- nofollow_policy
    } else if (is.numeric(column)) {
      rule <- "sum"
    } else {
      rule <- "first"
    }
  }

  if (!is.character(rule) || length(rule) != 1L) {
    stop(
      "Aggregation rule for column '", col_name,
      "' must be a single string or a function.",
      call. = FALSE
    )
  }

  switch(
    rule,
    sum = function(x) sum(x, na.rm = TRUE),
    mean = function(x) mean(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE),
    min = function(x) min(x, na.rm = TRUE),
    first = function(x) x[[1L]],
    last = function(x) x[[length(x)]],
    any = function(x) bool_policy_any(x),
    all = function(x) bool_policy_all(x),
    majority = function(x) bool_policy_majority(x),
    error = function(x) bool_policy_error(x, col_name),
    # switch() default branch: reached for any unmatched `rule`, not dead code.
    stop(
      "Unknown aggregation rule '", rule, "' for column '", col_name, "'.",
      call. = FALSE
    )
  ) # nolint: unreachable_code_linter.
}

#' @keywords internal
#' @noRd
bool_policy_any <- function(x) {
  any(as.logical(x), na.rm = TRUE)
}

#' @keywords internal
#' @noRd
bool_policy_all <- function(x) {
  all(as.logical(x), na.rm = TRUE)
}

#' @keywords internal
#' @noRd
bool_policy_majority <- function(x) {
  x <- as.logical(x)
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  n_true <- sum(x)
  # Ties resolve to TRUE.
  n_true >= length(x) - n_true
}

#' @keywords internal
#' @noRd
bool_policy_error <- function(x, col_name) {
  vals <- as.logical(x)
  vals <- vals[!is.na(vals)]
  if (length(unique(vals)) > 1L) {
    stop(
      "Conflicting values in column '", col_name,
      "' for a folded from/to group (policy = \"error\").",
      call. = FALSE
    )
  }
  if (length(vals) == 0L) {
    return(NA)
  }
  vals[[1L]]
}
