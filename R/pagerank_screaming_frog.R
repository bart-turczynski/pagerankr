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
                                    ...) {
  if (!inherits(bundle, "screaming_frog_bundle")) {
    stop("`bundle` must be a `screaming_frog_bundle` object.", call. = FALSE)
  }
  .sf_validate_bundle_for_pagerank(bundle)

  dots <- list(...)
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

  if (!is.null(weight_col) && !is.null(placement_weights)) {
    stop(
      "`weight_col` cannot be combined with `placement_weights`.",
      call. = FALSE
    )
  }
  if (!is.null(weight_col) &&
        (!is.character(weight_col) || length(weight_col) != 1L ||
           is.na(weight_col) || !nzchar(weight_col))) {
    stop("`weight_col` must be a single non-empty string or NULL.",
      call. = FALSE
    )
  }

  edges <- bundle$edges
  input_edges <- nrow(edges)

  accepted_placements <- .sf_validate_accepted_placements(
    accepted_placements
  )
  if (!is.null(accepted_placements)) {
    edges <- edges[
      !is.na(edges$placement) & edges$placement %in% accepted_placements,
      ,
      drop = FALSE
    ]
  }

  link_origins <- .sf_validate_link_origins(link_origins)
  origin_keys <- .sf_link_origin_key(edges$link_origin)
  if (!is.null(link_origins)) {
    edges <- edges[
      !is.na(origin_keys) & origin_keys %in% link_origins,
      ,
      drop = FALSE
    ]
  }

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
  if (!is.null(effective_weight_col) &&
        nrow(edges) > 0L && !(effective_weight_col %in% names(edges))) {
    stop(
      "`weight_col` '", effective_weight_col,
      "' is not present in `bundle$edges`.",
      call. = FALSE
    )
  }

  if (nrow(edges) == 0L) {
    stop(
      "`bundle` has no graph-eligible edges after Screaming Frog wrapper ",
      "filters; `pagerank_screaming_frog()` cannot score a node-only graph.",
      call. = FALSE
    )
  }

  pr_args <- c(
    list(
      edge_list_df = edges,
      redirects_df = .sf_nullable_df(bundle$redirects),
      canonicals_df = .sf_nullable_df(bundle$canonicals),
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
  result <- do.call(pagerank, pr_args)

  attr(result, "screaming_frog_import") <- structure(
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
        nofollow_col = "nofollow"
      )
    ),
    class = "screaming_frog_import_audit"
  )

  result
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
  if (any(!x %in% allowed)) {
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
  if (any(is.na(x)) || any(!x %in% allowed)) {
    stop(
      "`link_origins` must contain only: ",
      paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  x
}

.sf_validate_placement_weights <- function(x) {
  if (!is.numeric(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    stop(
      "`placement_weights` must be a named positive numeric vector.",
      call. = FALSE
    )
  }
  if (anyNA(x) || any(!is.finite(x)) || any(x <= 0)) {
    stop(
      "`placement_weights` must contain finite positive values.",
      call. = FALSE
    )
  }
  names(x) <- tolower(trimws(names(x)))
  allowed <- c("nav", "header", "footer", "sidebar", "content")
  if (any(!names(x) %in% allowed)) {
    stop(
      "`placement_weights` names must contain only: ",
      paste(allowed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(duplicated(names(x)))) {
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
