#' The placement vocabulary
#'
#' The five region terms `pagerank()` accepts in `accepted_placements` and
#' `placement_weights`. Deliberately crawler-neutral: a per-crawler adapter
#' (e.g. [sf_normalize_position()]) maps vendor labels onto these, so the
#' fuzziness of detection stays in the adapter instead of leaking into the
#' weighting math.
#'
#' `"content"` is an acknowledged *residual* -- "not classified as
#' nav/header/footer/aside" -- and is named `content` precisely because it makes
#' no tag claim.
#'
#' @keywords internal
#' @noRd
.pr_placement_vocabulary <- function() {
  c("content", "nav", "header", "footer", "aside")
}

#' Validate `accepted_placements`
#'
#' @return The normalized (lowercased, trimmed, de-duplicated) vector, or NULL.
#' @keywords internal
#' @noRd
.pr_validate_accepted_placements <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || anyNA(x)) {
    stop("`accepted_placements` must be a character vector or NULL.",
      call. = FALSE
    )
  }
  x <- unique(tolower(trimws(x)))
  allowed <- .pr_placement_vocabulary()
  if (!all(x %in% allowed)) {
    stop(
      "`accepted_placements` must contain only: ",
      toString(allowed), ".",
      call. = FALSE
    )
  }
  x
}

#' Validate `placement_weights`
#'
#' @return The normalized named numeric vector, or NULL.
#' @keywords internal
#' @noRd
.pr_validate_placement_weights <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || is.null(names(x)) || !all(nzchar(names(x)))) {
    stop(
      "`placement_weights` must be a named positive numeric vector.",
      call. = FALSE
    )
  }
  if (anyNA(x) || !all(is.finite(x)) || any(x <= 0)) {
    stop(
      "`placement_weights` must contain finite positive values.",
      call. = FALSE
    )
  }
  names(x) <- tolower(trimws(names(x)))
  allowed <- .pr_placement_vocabulary()
  if (!all(names(x) %in% allowed)) {
    stop(
      "`placement_weights` names must contain only: ",
      toString(allowed), ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(names(x)) > 0L) {
    stop("`placement_weights` names must be unique.", call. = FALSE)
  }
  x
}

#' Validate the placement argument trio against each other
#'
#' `accepted_placements` / `placement_weights` are meaningless without a column
#' to read placements from, and `placement_weights` builds a weight column of
#' its own, so it cannot be combined with a caller-supplied `weight_col`.
#'
#' @keywords internal
#' @noRd
.pr_validate_placement_args <- function(placement_col,
                                        accepted_placements,
                                        placement_weights,
                                        weight_col,
                                        edge_list_df,
                                        preset_source = NULL) {
  .pr_check_weight_col_exclusivity(weight_col, placement_weights)
  accepted_placements <- .pr_validate_accepted_placements(accepted_placements)
  placement_weights <- .pr_validate_placement_weights(placement_weights)
  if (is.null(placement_col)) {
    dependent <- c(
      accepted_placements = !is.null(accepted_placements),
      placement_weights = !is.null(placement_weights)
    )
    if (any(dependent)) {
      stop(
        "`", toString(names(dependent)[dependent]),
        "` requires `placement_col`.",
        .pr_placement_preset_hint(preset_source),
        call. = FALSE
      )
    }
  } else {
    .assert_col_or_null(placement_col, "placement_col", edge_list_df)
  }
  list(
    accepted_placements = accepted_placements,
    placement_weights = placement_weights
  )
}

#' Which preset, if any, supplied the placement arguments
#'
#' A preset sets *policy*; `placement_col` is *data* the caller must supply. So
#' `preset = "content"` on an edge list with no placement column errors on an
#' argument the caller never typed -- this reports the preset it came from.
#' Returns `NULL` unless the preset is what set a placement argument.
#'
#' @keywords internal
#' @noRd
.pr_placement_preset_source <- function(preset, applied) {
  placement_args <- c("accepted_placements", "placement_weights")
  if (!any(placement_args %in% applied)) {
    return(NULL)
  }
  .pr_preset_label(preset)
}

#' @keywords internal
#' @noRd
.pr_placement_preset_hint <- function(preset_source) {
  if (is.null(preset_source)) {
    return("")
  }
  paste0(
    " It was set by `preset = \"", preset_source,
    "\"`, which sets policy but not data: name the column holding each",
    " edge's page region."
  )
}

#' `placement_weights` supersedes `weight_col`, so the two are exclusive
#'
#' Checked on its own so callers that validate `weight_col` against their own
#' edge table first (e.g. [pagerank_screaming_frog()]) can still report the
#' clearer of the two errors.
#'
#' @keywords internal
#' @noRd
.pr_check_weight_col_exclusivity <- function(weight_col, placement_weights) {
  if (!is.null(weight_col) && !is.null(placement_weights)) {
    stop(
      "`weight_col` cannot be combined with `placement_weights`.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Name of the synthetic weight column `placement_weights` builds
#' @keywords internal
#' @noRd
.pr_edge_weight_col <- function() {
  ".__pr_edge_weight__"
}

#' Filter and weight edges by placement region
#'
#' The crawler-neutral half of placement-aware PageRank: `accepted_placements`
#' subsets the edge list, and `placement_weights` maps the categorical
#' `placement_col` onto a synthetic numeric weight column that becomes the
#' effective `weight_col`.
#'
#' Placements not named in `placement_weights` keep weight `1`, so a partial
#' recipe such as `c(nav = 0.1)` leaves everything else untouched. Naming all
#' five terms is the way to express a complete recipe -- see the `"content"`
#' preset.
#'
#' @return A list with `edge_list_df`, the possibly-updated `weight_col`, and a
#'   `provenance` list recorded in the transition audit.
#' @keywords internal
#' @noRd
.pr_apply_placement <- function(edge_list_df,
                                placement_col,
                                accepted_placements,
                                placement_weights,
                                weight_col,
                                preset_source = NULL) {
  validated <- .pr_validate_placement_args(
    placement_col = placement_col,
    accepted_placements = accepted_placements,
    placement_weights = placement_weights,
    weight_col = weight_col,
    edge_list_df = edge_list_df,
    preset_source = preset_source
  )
  accepted_placements <- validated$accepted_placements
  placement_weights <- validated$placement_weights
  if (is.null(placement_col)) {
    return(
      list(
        edge_list_df = edge_list_df,
        weight_col = weight_col,
        provenance = NULL
      )
    )
  }
  provenance <- list(
    placement_col = placement_col,
    accepted_placements = accepted_placements,
    placement_weights = placement_weights,
    n_rows_dropped = 0L
  )

  placement <- tolower(trimws(as.character(edge_list_df[[placement_col]])))
  if (!is.null(accepted_placements)) {
    keep <- !is.na(placement) & placement %in% accepted_placements
    provenance$n_rows_dropped <- sum(!keep)
    edge_list_df <- edge_list_df[keep, , drop = FALSE]
    placement <- placement[keep]
    .pr_check_placement_survivors(edge_list_df, accepted_placements)
  }

  if (!is.null(placement_weights)) {
    weight_col <- .pr_edge_weight_col()
    weights <- rep(1, length(placement))
    named <- !is.na(placement) & placement %in% names(placement_weights)
    weights[named] <- unname(placement_weights[placement[named]])
    edge_list_df[[weight_col]] <- weights
  }

  list(
    edge_list_df = edge_list_df,
    weight_col = weight_col,
    provenance = provenance
  )
}

.pr_check_placement_survivors <- function(edge_list_df, accepted_placements) {
  if (nrow(edge_list_df) > 0L) {
    return(invisible(NULL))
  }
  stop(
    "No edges remain after filtering to `accepted_placements`: ",
    toString(accepted_placements), ".",
    call. = FALSE
  )
}
