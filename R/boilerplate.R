#' Validate the boilerplate detector arguments
#'
#' `container_col` is *data* and is what switches the detector on, mirroring
#' `nofollow_col` / `nofollow_action` and `placement_col` / `placement_weights`.
#' The three constants therefore carry real defaults and are simply unused when
#' no container column is supplied.
#'
#' @keywords internal
#' @noRd
.pr_validate_boilerplate_args <- function(container_col,
                                          boilerplate_threshold,
                                          min_container_pages,
                                          boilerplate_weight,
                                          weight_col,
                                          edge_list_df) {
  .pr_check_container_weight_exclusivity(weight_col, container_col)
  if (is.null(container_col)) {
    return(invisible(NULL))
  }
  .assert_col_or_null(container_col, "container_col", edge_list_df)
  .pr_assert_unit_scalar(boilerplate_threshold, "boilerplate_threshold")
  .pr_assert_unit_scalar(boilerplate_weight, "boilerplate_weight")
  if (!is.numeric(min_container_pages) ||
        length(min_container_pages) != 1L ||
        !is.finite(min_container_pages) ||
        min_container_pages < 1) {
    stop(
      "`min_container_pages` must be a single finite number >= 1.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' A single number in (0, 1]
#'
#' `boilerplate_threshold` is a *ratio of pages* and `boilerplate_weight` is a
#' *multiplier*; both are bounded above by 1 and must stay above 0. A weight of
#' 0 would be a silent deletion, which is exactly what the model rejects.
#'
#' @keywords internal
#' @noRd
.pr_assert_unit_scalar <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 || x > 1) {
    stop("`", arg, "` must be a single number in (0, 1].", call. = FALSE)
  }
  invisible(NULL)
}

#' The detector builds a weight column, so it is exclusive with `weight_col`
#'
#' Split out for the same reason as the placement check: callers that validate
#' their own `weight_col` first can report the clearer error.
#'
#' @keywords internal
#' @noRd
.pr_check_container_weight_exclusivity <- function(weight_col, container_col) {
  # The synthetic column placement builds is ours, not the caller's: the two
  # axes are designed to compose multiplicatively, so only a caller-supplied
  # `weight_col` is a contradiction here.
  if (identical(weight_col, .pr_edge_weight_col())) {
    return(invisible(NULL))
  }
  if (!is.null(weight_col) && !is.null(container_col)) {
    stop(
      "`weight_col` cannot be combined with `container_col`.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Discount repetitive links by container-conditioned recurrence
#'
#' The second detector feeding the boilerplate axis. Placement catches site
#' chrome; this catches template links that sit *in the content region* --
#' recycled CTAs, compliance links, author bylines -- which placement can never
#' catch because they are, structurally, content.
#'
#' The metric is **container-conditioned and target-scored**:
#'
#' - denominator: the number of pages the container appears on at all;
#' - numerator: the number of those pages where it points at *this* target;
#' - `ratio = numerator / denominator`, a boilerplate score in `[0, 1]` where
#'   **higher means more boilerplate**.
#'
#' Conditioning on the container rather than the site is what separates a
#' recycled CTA that always links the same place (ratio ~1, boilerplate) from a
#' related-posts module that links somewhere different on every page (low ratio,
#' not boilerplate) even though both recur identically.
#'
#' Containers seen on fewer than `min_container_pages` pages are never
#' classified: "3 out of 3" is thin evidence, and a 3-page container can only
#' produce ratios of 0.33, 0.67 or 1, so membership near the threshold is
#' partly quantization rather than genuine signal.
#'
#' @return A list with `edge_list_df`, the possibly-updated `weight_col`, and a
#'   `provenance` list recorded in the transition audit.
#' @keywords internal
#' @noRd
.pr_apply_boilerplate <- function(edge_list_df,
                                  container_col,
                                  from_col,
                                  to_col,
                                  boilerplate_threshold,
                                  min_container_pages,
                                  boilerplate_weight,
                                  weight_col) {
  .pr_validate_boilerplate_args(
    container_col = container_col,
    boilerplate_threshold = boilerplate_threshold,
    min_container_pages = min_container_pages,
    boilerplate_weight = boilerplate_weight,
    weight_col = weight_col,
    edge_list_df = edge_list_df
  )
  if (is.null(container_col)) {
    return(
      list(
        edge_list_df = edge_list_df,
        weight_col = weight_col,
        provenance = NULL
      )
    )
  }

  container <- as.character(edge_list_df[[container_col]])
  source_page <- as.character(edge_list_df[[from_col]])
  target <- as.character(edge_list_df[[to_col]])
  scorable <- !is.na(container) & nzchar(container) &
    !is.na(source_page) & !is.na(target)

  ratio <- rep(NA_real_, length(container))
  container_pages <- rep(NA_integer_, length(container))
  if (any(scorable)) {
    scored <- .pr_boilerplate_ratio(
      container[scorable], source_page[scorable], target[scorable]
    )
    ratio[scorable] <- scored$ratio
    container_pages[scorable] <- scored$container_pages
  }

  # An edge is boilerplate only with enough evidence behind it.
  judged <- !is.na(ratio) & container_pages >= min_container_pages
  is_boilerplate <- judged & ratio >= boilerplate_threshold

  # Multiply into the existing weights rather than replacing them: placement
  # may already have written a factor here, and the two axes compose (see
  # notes/edge-weighting-model.md section 2). Both factors are recorded
  # separately in the audit, because storing only the product is unauditable.
  if (is.null(weight_col)) {
    weight_col <- .pr_edge_weight_col()
    edge_list_df[[weight_col]] <- rep(1, nrow(edge_list_df))
  }
  factor <- ifelse(is_boilerplate, boilerplate_weight, 1)
  edge_list_df[[weight_col]] <- edge_list_df[[weight_col]] * factor

  list(
    edge_list_df = edge_list_df,
    weight_col = weight_col,
    provenance = list(
      container_col = container_col,
      boilerplate_threshold = boilerplate_threshold,
      min_container_pages = min_container_pages,
      boilerplate_weight = boilerplate_weight,
      n_containers = length(unique(container[scorable])),
      n_edges_scored = sum(scorable),
      n_edges_judged = sum(judged),
      n_edges_discounted = sum(is_boilerplate)
    )
  )
}

#' Container-conditioned recurrence ratio, per edge
#'
#' Counts *distinct pages*, not edge rows, on both sides: a container linking
#' the same target twice on one page is one page's worth of evidence, not two.
#'
#' @return A list of two per-edge vectors: `ratio` and `container_pages` (the
#'   denominator, carried out so the evidence floor can be applied against it).
#' @keywords internal
#' @noRd
.pr_boilerplate_ratio <- function(container, source_page, target) {
  sep <- "\r"
  container_page <- paste(container, source_page, sep = sep)
  pair <- paste(container, target, sep = sep)
  pair_page <- paste(pair, source_page, sep = sep)

  # Denominator: distinct pages the container appears on.
  f <- factor(container)
  first_page <- !duplicated(container_page)
  denominator <- as.integer(table(factor(
    container[first_page],
    levels = levels(f)
  )))
  names(denominator) <- levels(f)

  # Numerator: distinct pages on which the container points at this target.
  pair_f <- factor(pair)
  first_pair_page <- !duplicated(pair_page)
  numerator <- as.integer(table(factor(
    pair[first_pair_page],
    levels = levels(pair_f)
  )))
  names(numerator) <- levels(pair_f)

  container_pages <- denominator[as.character(container)]
  hits <- numerator[as.character(pair)]
  list(
    ratio = unname(hits / container_pages),
    container_pages = unname(container_pages)
  )
}
