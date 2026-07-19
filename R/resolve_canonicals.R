#' Resolve edge endpoints through rel=canonical declarations
#'
#' Applies declared `rel=canonical` folds to the source and target columns of an
#' edge list. This is URL **folding** from canonical-link signals, not URL
#' syntax canonicalization such as lower-casing hosts or removing tracking
#' parameters.
#'
#' @param edge_list_df A data frame representing the edge list.
#' @param canonicals_df A data frame of declared canonical links.
#' @param edge_from_col,edge_to_col Source/target columns in `edge_list_df`.
#' @param canonical_from_col,canonical_to_col From/to columns in
#'   `canonicals_df`.
#' @param canonical_duplicate_from_policy How to handle a canonical source with
#'   multiple distinct targets. See [build_fold_map()].
#' @param canonical_loop_handling How to handle canonical cycles. See
#'   [build_fold_map()].
#'
#' @return The edge list with `edge_from_col` and `edge_to_col` folded through
#'   canonical declarations. The applied fold map is attached as attribute
#'   `"fold_map"`.
#' @family edge-list resolvers
#' @export
#' @examples
#' edges <- data.frame(from = "A", to = "B")
#' canonicals <- data.frame(from = "B", to = "C")
#' resolve_canonicals(edges, canonicals)
resolve_canonicals <- function(edge_list_df,
                               canonicals_df,
                               edge_from_col = "from",
                               edge_to_col = "to",
                               canonical_from_col = "from",
                               canonical_to_col = "to",
                               canonical_duplicate_from_policy = c(
                                 "strict",
                                 "first_wins",
                                 "last_wins",
                                 "most_frequent",
                                 "prune_source",
                                 "resolve_if_consistent"
                               ),
                               canonical_loop_handling = c(
                                 "error",
                                 "prune_loop",
                                 "break_arrow"
                               )) {
  canonical_duplicate_from_policy <- match.arg(
    canonical_duplicate_from_policy
  )
  canonical_loop_handling <- match.arg(canonical_loop_handling)
  .validate_edge_resolution_input(edge_list_df, edge_from_col, edge_to_col)
  .validate_signal_df(
    canonicals_df,
    canonical_from_col,
    canonical_to_col,
    "canonicals_df"
  )

  fold <- .compose_fold_map(
    canonicals_df = canonicals_df,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    canonical_duplicate_from_policy = canonical_duplicate_from_policy,
    canonical_loop_handling = canonical_loop_handling
  )

  resolved <- .apply_map_to_edge_list(
    edge_list_df,
    fold$map,
    edge_from_col,
    edge_to_col
  )
  attr(resolved, "fold_map") <- .fold_map_data_frame(fold)
  resolved
}

#' Resolve URLs through rel=canonical declarations
#'
#' Resolves a character vector through declared `rel=canonical` folds. This is
#' distinct from URL syntax canonicalization; inputs are expected to already be
#' in the same URL namespace as the canonical table.
#'
#' @param urls Character vector of URLs to resolve.
#' @inheritParams resolve_canonicals
#'
#' @return A data frame with `original`, `resolved`, `changed`, and `signal`
#'   columns. The applied fold map is attached as attribute `"fold_map"`.
#' @family URL-vector resolvers
#' @export
#' @examples
#' canonicals <- data.frame(
#'   from = c("A", "B"),
#'   to = c("B", "C")
#' )
#' resolve_canonical_urls(c("A", "B", "X"), canonicals)
resolve_canonical_urls <- function(urls,
                                   canonicals_df,
                                   canonical_from_col = "from",
                                   canonical_to_col = "to",
                                   canonical_duplicate_from_policy = c(
                                     "strict",
                                     "first_wins",
                                     "last_wins",
                                     "most_frequent",
                                     "prune_source",
                                     "resolve_if_consistent"
                                   ),
                                   canonical_loop_handling = c(
                                     "error",
                                     "prune_loop",
                                     "break_arrow"
                                   )) {
  canonical_duplicate_from_policy <- match.arg(
    canonical_duplicate_from_policy
  )
  canonical_loop_handling <- match.arg(canonical_loop_handling)
  .validate_urls(urls)
  .validate_signal_df(
    canonicals_df,
    canonical_from_col,
    canonical_to_col,
    "canonicals_df"
  )

  fold <- .compose_fold_map(
    canonicals_df = canonicals_df,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    canonical_duplicate_from_policy = canonical_duplicate_from_policy,
    canonical_loop_handling = canonical_loop_handling
  )

  .resolve_urls_with_fold(urls, fold)
}

#' Resolve URLs through composed redirects and canonicals
#'
#' Resolves URL vectors with the same composed 3xx redirect plus declared
#' `rel=canonical` fold-map engine used by [pagerank()] and [build_fold_map()].
#' This helper performs signal folding only; it does not perform URL syntax
#' canonicalization.
#'
#' @param urls Character vector of URLs to resolve.
#' @param redirects_df Optional data frame of redirect rules, or `NULL`.
#' @param canonicals_df Optional data frame of declared canonical links, or
#'   `NULL`.
#' @inheritParams build_fold_map
#'
#' @return A data frame with `original`, `resolved`, `changed`, and `signal`
#'   columns. The exported fold map is attached as attribute `"fold_map"` and
#'   cross-signal audit tables are attached as `"conflicts"` and
#'   `"ignored_canonicals"`.
#' @family URL-vector resolvers
#' @export
#' @examples
#' redirects <- data.frame(from = "B", to = "C")
#' canonicals <- data.frame(from = "A", to = "B")
#' resolve_folded_urls(c("A", "B", "X"), redirects, canonicals)
resolve_folded_urls <- function(urls,
                                redirects_df = NULL,
                                canonicals_df = NULL,
                                redirect_from_col = "from",
                                redirect_to_col = "to",
                                canonical_from_col = "from",
                                canonical_to_col = "to",
                                duplicate_from_policy = c(
                                  "strict",
                                  "first_wins",
                                  "last_wins",
                                  "most_frequent",
                                  "prune_source",
                                  "resolve_if_consistent"
                                ),
                                loop_handling = c(
                                  "error",
                                  "prune_loop",
                                  "break_arrow"
                                ),
                                canonical_duplicate_from_policy = c(
                                  "strict",
                                  "first_wins",
                                  "last_wins",
                                  "most_frequent",
                                  "prune_source",
                                  "resolve_if_consistent"
                                ),
                                canonical_loop_handling = c(
                                  "error",
                                  "prune_loop",
                                  "break_arrow"
                                ),
                                canonical_conflict_policy = c(
                                  "redirect_wins",
                                  "error",
                                  "canonical_wins"
                                )) {
  duplicate_from_policy <- match.arg(duplicate_from_policy)
  loop_handling <- match.arg(loop_handling)
  canonical_duplicate_from_policy <- match.arg(
    canonical_duplicate_from_policy
  )
  canonical_loop_handling <- match.arg(canonical_loop_handling)
  canonical_conflict_policy <- match.arg(canonical_conflict_policy)

  .validate_urls(urls)
  .validate_optional_signal_df(
    redirects_df,
    redirect_from_col,
    redirect_to_col,
    "redirects_df"
  )
  .validate_optional_signal_df(
    canonicals_df,
    canonical_from_col,
    canonical_to_col,
    "canonicals_df"
  )

  fold <- .compose_fold_map(
    redirects_df = redirects_df,
    canonicals_df = canonicals_df,
    redirect_from_col = redirect_from_col,
    redirect_to_col = redirect_to_col,
    canonical_from_col = canonical_from_col,
    canonical_to_col = canonical_to_col,
    duplicate_from_policy = duplicate_from_policy,
    loop_handling = loop_handling,
    canonical_duplicate_from_policy = canonical_duplicate_from_policy,
    canonical_loop_handling = canonical_loop_handling,
    canonical_conflict_policy = canonical_conflict_policy
  )

  .resolve_urls_with_fold(urls, fold)
}

.resolve_urls_with_fold <- function(urls, fold) {
  resolved <- .apply_fold_map(urls, fold$map)
  signal <- rep(NA_character_, length(urls))
  idx <- match(urls, names(fold$signal))
  signal[!is.na(idx)] <- unname(fold$signal[idx[!is.na(idx)]])
  out <- data.frame(
    original = urls,
    resolved = resolved,
    changed = !is.na(urls) & urls != resolved,
    signal = signal
  )
  attr(out, "fold_map") <- .fold_map_data_frame(fold)
  attr(out, "conflicts") <- fold$conflicts
  attr(out, "ignored_canonicals") <- fold$ignored_canonicals
  out
}

.apply_map_to_edge_list <- function(edge_list_df,
                                    map,
                                    edge_from_col,
                                    edge_to_col) {
  resolved <- edge_list_df
  if (length(map) == 0L) {
    return(resolved)
  }
  for (col_name in c(edge_from_col, edge_to_col)) {
    resolved[[col_name]] <- .apply_fold_map(resolved[[col_name]], map)
  }
  resolved
}

.fold_map_data_frame <- function(fold) {
  out <- data.frame(
    from = names(fold$map),
    to = unname(fold$map),
    signal = unname(fold$signal[names(fold$map)])
  )
  attr(out, "conflicts") <- fold$conflicts
  attr(out, "ignored_canonicals") <- fold$ignored_canonicals
  out
}

.validate_urls <- function(urls) {
  if (!is.character(urls)) {
    stop("`urls` must be a character vector.", call. = FALSE)
  }
}

.validate_edge_resolution_input <- function(edge_list_df,
                                            edge_from_col,
                                            edge_to_col) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0L &&
        !all(c(edge_from_col, edge_to_col) %in% names(edge_list_df))) {
    stop(
      "`edge_list_df` must have '", edge_from_col, "' and '",
      edge_to_col, "' columns if not empty.",
      call. = FALSE
    )
  }
}

.validate_signal_df <- function(x, from_col, to_col, what) {
  if (!is.data.frame(x)) {
    stop("`", what, "` must be a data frame.", call. = FALSE)
  }
  if (nrow(x) > 0L && !all(c(from_col, to_col) %in% names(x))) {
    stop(
      "`", what, "` must have '", from_col, "' and '", to_col, "' columns.",
      call. = FALSE
    )
  }
}

.validate_optional_signal_df <- function(x, from_col, to_col, what) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  .validate_signal_df(x, from_col, to_col, what)
}
