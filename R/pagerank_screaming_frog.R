#' Score a Screaming Frog bundle with PageRank
#'
#' Thin convenience wrapper around [pagerank()] for the stable
#' [screaming_frog_bundle()] handoff object. Only `bundle$edges` enter the
#' graph. Raw observations and dedicated redirect, canonical, indexability, and
#' resource rows stay out of the edge list and are mapped to the existing
#' [pagerank()] arguments.
#'
#' @param bundle A `screaming_frog_bundle` object.
#' @param accepted_placements Optional character vector of normalized
#'   placements to retain. Values must be among `"nav"`, `"header"`,
#'   `"footer"`, `"sidebar"`, and `"content"`. `NULL` keeps all graph-eligible
#'   hyperlink edges.
#' @param link_origins Optional character vector of normalized link origins to
#'   retain. Values must be among `"html"`, `"rendered"`, and
#'   `"html_rendered"`. `NULL` keeps all origins present in `bundle$edges`.
#' @param placement_weights Optional named positive numeric vector assigning
#'   edge weights by normalized placement. Unnamed or unknown placements are
#'   rejected. Edges whose placement is not named receive weight `1`.
#' @param weight_col Optional existing edge weight column to pass to
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
#' @param ... Additional scoring controls passed to [pagerank()], such as
#'   `self_loops`, `drop_isolates_flag`, `nofollow_action`,
#'   `robots_blocked_action`, `rurl_params`, prior settings, and `damping`.
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
                                    ...) {
  if (!inherits(bundle, "screaming_frog_bundle")) {
    stop("`bundle` must be a `screaming_frog_bundle` object.", call. = FALSE)
  }
  .sf_validate_bundle_for_pagerank(bundle)
  apply_canonicals <- .sf_validate_fold_flag(
    apply_canonicals, "apply_canonicals"
  )
  apply_redirects <- .sf_validate_fold_flag(apply_redirects, "apply_redirects")

  dots <- list(...)
  .sf_check_reserved_dots(dots)
  .sf_validate_weight_col(weight_col, placement_weights)

  edges <- bundle$edges
  input_edges <- nrow(edges)

  accepted_placements <- .sf_validate_accepted_placements(
    accepted_placements
  )
  edges <- .sf_filter_by_placements(edges, accepted_placements)

  link_origins <- .sf_validate_link_origins(link_origins)
  edges <- .sf_filter_by_origins(edges, link_origins)

  weighted <- .sf_apply_placement_weights(edges, placement_weights, weight_col)
  edges <- weighted$edges
  placement_weights <- weighted$placement_weights
  effective_weight_col <- weighted$effective_weight_col

  .sf_check_effective_weight_col(effective_weight_col, edges)
  .sf_check_nonempty_edges(edges)

  pr_args <- .sf_build_pr_args(
    edges, bundle, apply_redirects, apply_canonicals,
    effective_weight_col, dots
  )
  result <- do.call(pagerank, pr_args)

  attr(result, "screaming_frog_import") <- .sf_build_import_attr(
    bundle, input_edges, edges, accepted_placements, link_origins,
    placement_weights, weight_col, effective_weight_col,
    apply_canonicals, apply_redirects
  )

  result
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
      paste(reserved, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.sf_validate_weight_col <- function(weight_col, placement_weights) {
  if (!is.null(weight_col) && !is.null(placement_weights)) {
    stop(
      "`weight_col` cannot be combined with `placement_weights`.",
      call. = FALSE
    )
  }
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

.sf_filter_by_placements <- function(edges, accepted_placements) {
  if (is.null(accepted_placements)) {
    return(edges)
  }
  edges[
    !is.na(edges$placement) & edges$placement %in% accepted_placements,
    ,
    drop = FALSE
  ]
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

.sf_apply_placement_weights <- function(edges, placement_weights, weight_col) {
  effective_weight_col <- weight_col
  if (!is.null(placement_weights)) {
    placement_weights <- .sf_validate_placement_weights(placement_weights)
    edges$.__sf_placement_weight__ <- 1
    weighted <- !is.na(edges$placement) &
      edges$placement %in% names(placement_weights)
    edges$.__sf_placement_weight__[weighted] <-
      unname(placement_weights[edges$placement[weighted]])
    effective_weight_col <- ".__sf_placement_weight__"
  }
  list(
    edges = edges,
    placement_weights = placement_weights,
    effective_weight_col = effective_weight_col
  )
}

.sf_check_effective_weight_col <- function(effective_weight_col, edges) {
  if (is.null(effective_weight_col)) {
    return(invisible(NULL))
  }
  if (nrow(edges) > 0L && !(effective_weight_col %in% names(edges))) {
    stop(
      "`weight_col` '", effective_weight_col,
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
                              apply_canonicals, effective_weight_col, dots) {
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
  c(
    list(
      edge_list_df = edges,
      redirects_df = redirects_df,
      canonicals_df = canonicals_df,
      indexability_df = .sf_nullable_df(bundle$indexability),
      edge_from_col = "from",
      edge_to_col = "to",
      redirect_from_col = "from",
      redirect_to_col = "to",
      canonical_from_col = "from",
      canonical_to_col = "to",
      indexability_url_col = "url",
      indexability_status_col = "indexability_status",
      nofollow_col = "nofollow",
      weight_col = effective_weight_col
    ),
    dots
  )
}

.sf_build_import_attr <- function(bundle, input_edges, edges,
                                  accepted_placements, link_origins,
                                  placement_weights, weight_col,
                                  effective_weight_col, apply_canonicals,
                                  apply_redirects) {
  structure(
    list(
      diagnostics = bundle$diagnostics,
      provenance = bundle$provenance,
      scoring = list(
        input_edges = input_edges,
        scored_edge_rows = nrow(edges),
        accepted_placements = accepted_placements,
        link_origins = link_origins,
        placement_weights = placement_weights,
        weight_col = weight_col,
        effective_weight_col = effective_weight_col,
        apply_canonicals = apply_canonicals,
        apply_redirects = apply_redirects,
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
      paste(missing, collapse = ", "), ".",
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
      paste(missing_edge_cols, collapse = ", "), ".",
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

.sf_validate_accepted_placements <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  allowed <- c("nav", "header", "footer", "sidebar", "content")
  if (!is.character(x) || anyNA(x)) {
    stop("`accepted_placements` must be a character vector or NULL.",
      call. = FALSE
    )
  }
  x <- unique(tolower(trimws(x)))
  if (!all(x %in% allowed)) {
    stop(
      "`accepted_placements` must contain only: ",
      paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
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
      paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  x
}

.sf_validate_placement_weights <- function(x) {
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
  allowed <- c("nav", "header", "footer", "sidebar", "content")
  if (!all(names(x) %in% allowed)) {
    stop(
      "`placement_weights` names must contain only: ",
      paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(names(x)) > 0L) {
    stop("`placement_weights` names must be unique.", call. = FALSE)
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
