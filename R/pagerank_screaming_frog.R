#' Score a Screaming Frog bundle with PageRank
#'
#' Thin convenience wrapper around [pagerank()] for the stable
#' [screaming_frog_bundle()] handoff object. Only `bundle$edges` enter the
#' graph. Raw observations and dedicated redirect, canonical, indexability, and
#' resource rows stay out of the edge list and are mapped to the existing
#' [pagerank()] arguments.
#'
#' @param bundle A `screaming_frog_bundle` object.
#' @param accepted_placements,placement_weights Placement controls forwarded to
#'   [pagerank()], which owns them: placement is a crawler-neutral concept, and
#'   this wrapper only supplies the bundle's normalized `placement` column as
#'   [pagerank()]'s `placement_col`. See [pagerank()] for the vocabulary
#'   (`"content"`, `"nav"`, `"header"`, `"footer"`, `"aside"`) and semantics.
#' @param link_origins Optional character vector of normalized link origins to
#'   retain. Values must be among `"html"`, `"rendered"`, and
#'   `"html_rendered"`. `NULL` keeps all origins present in `bundle$edges`.
#'   Unlike placement, link origin *is* a Screaming Frog concept and stays
#'   wrapper-owned.
#' @param weight_col Optional existing edge weight column, forwarded to
#'   [pagerank()]. Cannot be combined with `placement_weights`.
#' @param apply_canonicals Logical flag (default `TRUE`). When `TRUE` the
#'   bundle's `rel=canonical` signals are folded into the graph via
#'   [pagerank()]'s `canonicals_df`. Set `FALSE` for an as-crawled run that
#'   preserves the crawled node identities (no canonical folding) — useful when
#'   canonicals point off the crawled domain (e.g. a mirror/staging host) and
#'   would otherwise relabel crawled pages onto uncrawled targets.
#' @param apply_redirects Logical flag (default `TRUE`). When `TRUE` the
#'   bundle's redirect signals are folded into the graph via [pagerank()]'s
#'   `redirects_df`. Set `FALSE` to skip redirect folding and keep the
#'   as-crawled node identities.
#' @param preset Optional named view forwarded to [pagerank()]'s `preset`, one
#'   of `"raw"`, `"declared"`, `"reversed"`, `"content"`, or a [pr_preset()]
#'   result. The `"raw"` view additionally switches off the bundle's declared
#'   canonical, redirect and indexability tables, since "the graph exactly as
#'   crawled" cannot honor declarations the wrapper would otherwise feed in.
#'   An explicit `apply_canonicals` or `apply_redirects` still overrides the
#'   `"raw"` default for that table. See `vignette("presets")`.
#' @param ... Additional scoring controls passed to [pagerank()], such as
#'   `self_loops`, `drop_isolates_flag`, `nofollow_action`,
#'   `robots_blocked_action`, `rurl_params`, prior settings, and `damping`.
#'   Positional decay is opt-in here too: the bundle's `edges` carry a
#'   `position_index` column (each link's reading-order rank among its source
#'   page's content links, materialized only from an **All Outlinks** export),
#'   so pass `position_col = "position_index"` -- optionally with
#'   `position_transform` / `position_alpha` / `position_floor` -- to switch the
#'   axis on. It stays off by default, per the faithful-default rule, because
#'   reading-order decay reshuffles ranking as hard as placement weighting does.
#'
#' @return The [pagerank()] result data frame. It retains the
#'   `"transition_audit"` attribute from [pagerank()] and adds a
#'   `"screaming_frog_import"` attribute containing bundle diagnostics,
#'   provenance, and wrapper filtering/weighting choices.
#' @export
#'
#' @examples
#' internal <- data.frame(
#'   Address = c("https://example.com/", "https://example.com/a"),
#'   `Status Code` = c("200", "200"),
#'   check.names = FALSE
#' )
#' links <- data.frame(
#'   Type = "Hyperlink",
#'   Source = "https://example.com/",
#'   Destination = "https://example.com/a",
#'   Follow = "TRUE",
#'   check.names = FALSE
#' )
#' bundle <- screaming_frog_bundle(internal, links, "all_outlinks")
#' pagerank_screaming_frog(bundle)
pagerank_screaming_frog <- function(bundle,
                                    accepted_placements = NULL,
                                    link_origins = NULL,
                                    placement_weights = NULL,
                                    weight_col = NULL,
                                    apply_canonicals = TRUE,
                                    apply_redirects = TRUE,
                                    preset = NULL,
                                    ...) {
  # Capture before the prep call reassigns these, since a reassigned formal is
  # no longer `missing()`. Needed for the raw-preset flip inside the prep.
  prep <- .sf_prepare_pr_args(
    bundle, accepted_placements, link_origins, placement_weights, weight_col,
    apply_canonicals, apply_redirects, preset, list(...),
    missing(apply_canonicals), missing(apply_redirects)
  )
  result <- do.call(pagerank, prep$pr_args)

  attr(result, "screaming_frog_import") <- .sf_build_import_attr(
    bundle, prep$input_edges, prep$edges, accepted_placements,
    prep$link_origins, placement_weights, weight_col, prep$apply_canonicals,
    prep$apply_redirects, prep$apply_indexability
  )

  result
}

#' Validate a bundle and build the [pagerank()] argument list from it
#'
#' The shared bundle -> `pagerank()` adapter behind both
#' [pagerank_screaming_frog()] and [simulate_changes_screaming_frog()]. Runs the
#' wrapper-owned validation, applies the `"raw"` preset's declaration flip,
#' filters edges by link origin, and assembles the full `pagerank()` argument
#' list. Returns that list plus the pieces the scoring entry point needs to
#' build its import-audit attribute, so no bundle logic is duplicated between
#' the two entry points.
#'
#' @param missing_canonicals,missing_redirects Whether the caller left
#'   `apply_canonicals` / `apply_redirects` at their defaults, captured via
#'   `missing()` in the public function's frame (the `"raw"` flip only overrides
#'   a defaulted flag).
#' @noRd
.sf_prepare_pr_args <- function(bundle, accepted_placements, link_origins,
                                placement_weights, weight_col,
                                apply_canonicals, apply_redirects, preset, dots,
                                missing_canonicals, missing_redirects) {
  if (!inherits(bundle, "screaming_frog_bundle")) {
    stop("`bundle` must be a `screaming_frog_bundle` object.", call. = FALSE)
  }
  .sf_validate_bundle_for_pagerank(bundle)
  apply_canonicals <- .sf_validate_fold_flag(
    apply_canonicals, "apply_canonicals"
  )
  apply_redirects <- .sf_validate_fold_flag(apply_redirects, "apply_redirects")

  # The `raw` preset means "the graph exactly as crawled -- nothing applied."
  # The wrapper feeds pagerank() the bundle's declared canonical, redirect and
  # indexability tables, and folding any of that declared data IS an
  # application, so the raw view has to switch it all off. This cannot be
  # delegated to pagerank(): a preset sets policy, never data, so pagerank()
  # sees the wrapper-fed `canonicals_df` / `redirects_df` / `indexability_df`
  # as caller-named and (correctly) refuses to let the preset unset them. Hence
  # the flip lives here.
  #
  # Canonicals and redirects have public apply_* flags, so an explicitly passed
  # one still wins over the preset. Indexability has no such flag -- it drives
  # `robots_blocked_action`, which `raw` leaves at its "trap" default, so a
  # robots-blocked page would otherwise trap inbound rank even under raw. It is
  # dropped for the raw view with no override, because "raw but still honor
  # robots-blocking" contradicts the view's definition.
  raw_view <- identical(.pr_preset_label(preset), "raw")
  if (raw_view) {
    if (missing_canonicals) apply_canonicals <- FALSE
    if (missing_redirects) apply_redirects <- FALSE
  }
  apply_indexability <- !raw_view

  .sf_check_reserved_dots(dots)
  .sf_validate_weight_col(weight_col)
  # Checked here, not left to pagerank(), so the wrapper's own weight_col
  # presence check below cannot pre-empt the clearer combination error.
  .pr_check_weight_col_exclusivity(weight_col, placement_weights)

  edges <- bundle$edges
  input_edges <- nrow(edges)

  link_origins <- .sf_validate_link_origins(link_origins)
  edges <- .sf_filter_by_origins(edges, link_origins)

  .sf_check_weight_col_present(weight_col, edges)
  .sf_check_nonempty_edges(edges)

  pr_args <- .sf_build_pr_args(
    edges, bundle, apply_redirects, apply_canonicals, apply_indexability,
    weight_col, accepted_placements, placement_weights, preset, dots
  )
  list(
    pr_args = pr_args,
    input_edges = input_edges,
    edges = edges,
    apply_canonicals = apply_canonicals,
    apply_redirects = apply_redirects,
    apply_indexability = apply_indexability,
    link_origins = link_origins
  )
}

.sf_check_reserved_dots <- function(dots) {
  reserved <- intersect(
    names(dots),
    c(
      "edge_list_df", "redirects_df", "canonicals_df", "indexability_df",
      "edge_from_col", "edge_to_col", "redirect_from_col",
      "redirect_to_col", "canonical_from_col", "canonical_to_col",
      "indexability_url_col", "indexability_status_col", "nofollow_col"
    )
  )
  if (length(reserved) > 0L) {
    stop(
      "`pagerank_screaming_frog()` maps bundle component columns itself; ",
      "do not pass reserved pagerank argument(s): ",
      toString(reserved), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.sf_validate_weight_col <- function(weight_col) {
  if (is.null(weight_col)) {
    return(invisible(NULL))
  }
  if (!is.character(weight_col) || length(weight_col) != 1L) {
    stop("`weight_col` must be a single non-empty string or NULL.",
      call. = FALSE
    )
  }
  if (is.na(weight_col) || !nzchar(weight_col)) {
    stop("`weight_col` must be a single non-empty string or NULL.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.sf_filter_by_origins <- function(edges, link_origins) {
  origin_keys <- .sf_link_origin_key(edges$link_origin)
  if (is.null(link_origins)) {
    return(edges)
  }
  edges[
    !is.na(origin_keys) & origin_keys %in% link_origins,
    ,
    drop = FALSE
  ]
}

.sf_check_weight_col_present <- function(weight_col, edges) {
  if (is.null(weight_col)) {
    return(invisible(NULL))
  }
  if (nrow(edges) > 0L && !(weight_col %in% names(edges))) {
    stop(
      "`weight_col` '", weight_col,
      "' is not present in `bundle$edges`.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.sf_check_nonempty_edges <- function(edges) {
  if (nrow(edges) == 0L) {
    stop(
      "`bundle` has no graph-eligible edges after Screaming Frog wrapper ",
      "filters; `pagerank_screaming_frog()` cannot score a node-only graph.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.sf_build_pr_args <- function(edges, bundle, apply_redirects,
                              apply_canonicals, apply_indexability, weight_col,
                              accepted_placements, placement_weights,
                              preset, dots) {
  redirects_df <- if (apply_redirects) {
    .sf_nullable_df(bundle$redirects)
  } else {
    NULL
  }
  canonicals_df <- if (apply_canonicals) {
    .sf_nullable_df(bundle$canonicals)
  } else {
    NULL
  }
  indexability_df <- if (apply_indexability) {
    .sf_nullable_df(bundle$indexability)
  } else {
    NULL
  }
  c(
    list(
      edge_list_df = edges,
      redirects_df = redirects_df,
      canonicals_df = canonicals_df,
      indexability_df = indexability_df,
      edge_from_col = "from",
      edge_to_col = "to",
      redirect_from_col = "from",
      redirect_to_col = "to",
      canonical_from_col = "from",
      canonical_to_col = "to",
      indexability_url_col = "url",
      indexability_status_col = "indexability_status",
      nofollow_col = "nofollow",
      weight_col = weight_col,
      placement_col = "placement",
      accepted_placements = accepted_placements,
      placement_weights = placement_weights,
      preset = preset
    ),
    dots
  )
}

.sf_build_import_attr <- function(bundle, input_edges, edges,
                                  accepted_placements, link_origins,
                                  placement_weights, weight_col,
                                  apply_canonicals, apply_redirects,
                                  apply_indexability) {
  structure(
    list(
      diagnostics = bundle$diagnostics,
      provenance = bundle$provenance,
      scoring = list(
        input_edges = input_edges,
        # Rows handed to pagerank(), i.e. after the wrapper-owned origin
        # filter. The placement filter now runs inside pagerank(), which
        # records what it dropped in the transition audit's config$placement.
        edge_rows_to_pagerank = nrow(edges),
        accepted_placements = accepted_placements,
        link_origins = link_origins,
        placement_weights = placement_weights,
        weight_col = weight_col,
        apply_canonicals = apply_canonicals,
        apply_redirects = apply_redirects,
        apply_indexability = apply_indexability,
        canonicals_off_domain =
          bundle$diagnostics$counts$canonicals_off_domain,
        nofollow_col = "nofollow"
      )
    ),
    class = "screaming_frog_import_audit"
  )
}

.sf_validate_bundle_for_pagerank <- function(bundle) {
  required <- c(
    "edges", "redirects", "canonicals", "indexability",
    "diagnostics", "provenance"
  )
  missing <- setdiff(required, names(bundle))
  if (length(missing) > 0L) {
    stop(
      "`bundle` is missing required field(s): ",
      toString(missing), ".",
      call. = FALSE
    )
  }
  if (!is.data.frame(bundle$edges)) {
    stop("`bundle$edges` must be a data frame.", call. = FALSE)
  }
  edge_cols <- c("from", "to", "nofollow", "placement", "link_origin")
  missing_edge_cols <- setdiff(edge_cols, names(bundle$edges))
  if (length(missing_edge_cols) > 0L) {
    stop(
      "`bundle$edges` is missing required column(s): ",
      toString(missing_edge_cols), ".",
      call. = FALSE
    )
  }
  if (nrow(bundle$edges) == 0L) {
    stop(
      "`bundle` has no graph-eligible edges; ",
      "`pagerank_screaming_frog()` requires a link export with Hyperlink ",
      "edges, not a node-only bundle.",
      call. = FALSE
    )
  }
}

.sf_validate_fold_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be a single `TRUE` or `FALSE`.", call. = FALSE)
  }
  x
}

.sf_validate_link_origins <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || anyNA(x)) {
    stop("`link_origins` must be a character vector or NULL.", call. = FALSE)
  }
  x <- unique(.sf_link_origin_key(x))
  allowed <- c("html", "rendered", "html_rendered")
  if (anyNA(x) || !all(x %in% allowed)) {
    stop(
      "`link_origins` must contain only: ",
      toString(allowed), ".",
      call. = FALSE
    )
  }
  x
}

.sf_link_origin_key <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(value))
  out[value == "html"] <- "html"
  out[value %in% c("rendered", "rendered html")] <- "rendered"
  out[value %in% c("html_rendered", "html & rendered html")] <-
    "html_rendered"
  out[is.na(x) | value == ""] <- NA_character_
  out
}

.sf_nullable_df <- function(x) {
  if (is.data.frame(x) && nrow(x) > 0L) {
    x
  } else {
    NULL
  }
}
