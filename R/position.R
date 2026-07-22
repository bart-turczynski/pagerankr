#' Validate the positional-decay arguments
#'
#' `position_col` is *data* and is what switches the axis on, mirroring
#' `container_col` / the boilerplate detector and `placement_col` /
#' `placement_weights`. The decay constants therefore carry real defaults and
#' are simply unused when no position column is supplied.
#'
#' @keywords internal
#' @noRd
.pr_validate_position_args <- function(position_col,
                                       position_transform,
                                       position_alpha,
                                       position_floor,
                                       weight_col,
                                       edge_list_df) {
  .pr_check_position_weight_exclusivity(weight_col, position_col)
  if (is.null(position_col)) {
    return(invisible(NULL))
  }
  .assert_col_or_null(position_col, "position_col", edge_list_df)
  if (!position_transform %in% .pr_position_transforms()) {
    stop(
      "`position_transform` must be one of: ",
      toString(.pr_position_transforms()), ".",
      call. = FALSE
    )
  }
  if (!is.numeric(position_alpha) ||
        length(position_alpha) != 1L ||
        !is.finite(position_alpha) ||
        position_alpha <= 0) {
    stop("`position_alpha` must be a single positive number.", call. = FALSE)
  }
  # The floor keeps the compounded edge weight strictly above zero, so an
  # "effectively dropped" edge can never sneak back in through decay (see
  # notes/edge-weighting-model.md section 3). A floor of 0 would defeat that.
  .pr_assert_unit_scalar(position_floor, "position_floor")
  if (!is.numeric(edge_list_df[[position_col]])) {
    stop(
      "`position_col` (", position_col, ") must be a numeric column.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' The reading-order decay shapes the position axis accepts
#'
#' A rank-based subset of [transform_weights()]'s methods -- the only ones
#' meaningful on a per-source position rank. `log`, `minmax` and `percentile`
#' are range-compressors for raw signals (click counts), not rank decays, so
#' they are deliberately not offered here.
#'
#' @keywords internal
#' @noRd
.pr_position_transforms <- function() {
  c("zipf", "rank_linear")
}

#' The position axis builds a weight column, so it is exclusive with a caller's
#' `weight_col`
#'
#' Split out for the same reason as the placement and boilerplate checks:
#' callers that validate their own `weight_col` first can report the clearer
#' error. The synthetic column placement/boilerplate build is ours, not the
#' caller's, so composing on top of it is allowed -- that composition (region
#' and recurrence via `pmin`, then position via `*`) *is* the two-axis model.
#'
#' @keywords internal
#' @noRd
.pr_check_position_weight_exclusivity <- function(weight_col, position_col) {
  if (identical(weight_col, .pr_edge_weight_col())) {
    return(invisible(NULL))
  }
  if (!is.null(weight_col) && !is.null(position_col)) {
    stop(
      "`weight_col` cannot be combined with `position_col`.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Decay edge weights by reading order within the source page
#'
#' The genuinely orthogonal axis of the edge-weighting model. Placement and
#' recurrence feed one graded axis (region / templatedness); this measures
#' something different -- where in the reading order a link sits -- and so it
#' **multiplies** into the weight rather than competing for the minimum. The
#' reasonable-surfer intuition falls out of the arithmetic: an above-the-fold
#' boilerplate CTA (`0.5 * 1.0`) outranks a trailing organic link
#' (`1.0 * 0.2`) with no special-casing. See notes/edge-weighting-model.md
#' section 2.
#'
#' `position_col` holds a per-source **position index** (1 = the first link,
#' materialized from document order at ingest, never inferred from row order
#' downstream where a filter or join may already have destroyed it). The index
#' is converted to a `[position_floor, 1]` multiplier by an existing
#' [transform_weights()] decay applied **within each source page's choice set**
#' -- reusing, not re-implementing, the shipped `zipf` / `rank_linear` methods,
#' with `descending = FALSE` because position 1 is the most valuable rank but
#' numerically the smallest. Edges with no index (`NA` -- e.g. site chrome,
#' whose discount is the placement axis's job) keep position weight `1`.
#'
#' @return A list with `edge_list_df`, the possibly-updated `weight_col`, and a
#'   `provenance` list recorded in the transition audit.
#' @keywords internal
#' @noRd
.pr_apply_position <- function(edge_list_df,
                               position_col,
                               from_col,
                               position_transform,
                               position_alpha,
                               position_floor,
                               weight_col) {
  .pr_validate_position_args(
    position_col = position_col,
    position_transform = position_transform,
    position_alpha = position_alpha,
    position_floor = position_floor,
    weight_col = weight_col,
    edge_list_df = edge_list_df
  )
  if (is.null(position_col)) {
    return(
      list(
        edge_list_df = edge_list_df,
        weight_col = weight_col,
        provenance = NULL
      )
    )
  }

  index <- suppressWarnings(as.numeric(edge_list_df[[position_col]]))
  source_page <- as.character(edge_list_df[[from_col]])
  scorable <- !is.na(index) & !is.na(source_page)

  position_weight <- rep(1, nrow(edge_list_df))
  if (any(scorable)) {
    decayed <- .pr_position_decay(
      index[scorable], source_page[scorable],
      position_transform, position_alpha
    )
    position_weight[scorable] <- pmax(position_floor, decayed)
  }

  # Position MULTIPLIES with the graded boilerplate axis (unlike the two
  # detectors feeding that axis, which take the minimum). If placement or
  # boilerplate already built the synthetic weight column, compose on top of it;
  # otherwise start it at 1.
  if (is.null(weight_col)) {
    weight_col <- .pr_edge_weight_col()
    edge_list_df[[weight_col]] <- rep(1, nrow(edge_list_df))
  }
  edge_list_df[[weight_col]] <- edge_list_df[[weight_col]] * position_weight

  list(
    edge_list_df = edge_list_df,
    weight_col = weight_col,
    provenance = list(
      position_col = position_col,
      position_transform = position_transform,
      position_alpha = position_alpha,
      position_floor = position_floor,
      n_edges_scored = sum(scorable),
      n_sources_scored = length(unique(source_page[scorable])),
      min_position_weight = if (any(scorable)) {
        min(position_weight[scorable])
      } else {
        NA_real_
      }
    )
  )
}

#' Per-source reading-order decay
#'
#' Applies [transform_weights()] independently within each source page's
#' choice set, so a position-1 link on page A and a position-1 link on page B
#' are each top-of-choice-set for their own source rather than being ranked
#' against one another. The same grouped-reuse pattern as
#' [transform_edge_weights()], but returning the un-normalized decay factor (the
#' position multiplier) rather than a within-source transition probability.
#'
#' @return A numeric vector aligned to `index`, the decay factor per edge.
#' @keywords internal
#' @noRd
.pr_position_decay <- function(index, source_page, transform, alpha) {
  out <- rep(NA_real_, length(index))
  row_idx <- split(seq_along(index), source_page)
  for (rows in row_idx) {
    out[rows] <- transform_weights(
      index[rows],
      method = transform,
      alpha = alpha,
      descending = FALSE
    )
  }
  out
}
