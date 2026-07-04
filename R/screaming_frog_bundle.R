#' Compose Screaming Frog node and link exports
#'
#' Builds the stable `screaming_frog_bundle` handoff object from an
#' **Internal: All** export and one **All Inlinks** or **All Outlinks** export.
#' The component adapters remain the source of truth: raw link observations stay
#' separate from graph-eligible edges, and node, redirect, canonical, and
#' indexability tables are exposed unchanged for downstream scoring.
#'
#' @param internal A path/data frame accepted by [screaming_frog_internal()], or
#'   an existing `screaming_frog_internal` object.
#' @param links A path/data frame accepted by [screaming_frog_links()], or an
#'   existing `screaming_frog_links` object.
#' @param link_export_kind Declared link export kind when `links` is not already
#'   a `screaming_frog_links` object: `"all_inlinks"` or `"all_outlinks"`.
#' @param origin_policy,endpoint_action Passed to [screaming_frog_links()] when
#'   `links` is not already imported.
#'
#' @return An S3 object of class `screaming_frog_bundle` with stable top-level
#'   fields `nodes`, `observations`, `edges`, `redirects`, `canonicals`,
#'   `indexability`, `diagnostics`, and `provenance`.
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
#' bundle$edges
screaming_frog_bundle <- function(internal,
                                  links,
                                  link_export_kind = c(
                                    "all_inlinks", "all_outlinks"
                                  ),
                                  origin_policy = c(
                                    "all", "html", "rendered"
                                  ),
                                  endpoint_action = c("drop", "error")) {
  link_export_kind <- match.arg(link_export_kind)
  origin_policy <- match.arg(origin_policy)
  endpoint_action <- match.arg(endpoint_action)

  internal_import <- if (inherits(internal, "screaming_frog_internal")) {
    internal
  } else {
    screaming_frog_internal(internal)
  }
  links_import <- if (inherits(links, "screaming_frog_links")) {
    links
  } else {
    screaming_frog_links(
      links,
      export_kind = link_export_kind,
      origin_policy = origin_policy,
      endpoint_action = endpoint_action
    )
  }

  diagnostics <- .sf_bundle_diagnostics(internal_import, links_import)
  provenance <- list(
    export_kind = "screaming_frog_bundle",
    contract_version = .sf_contract()$version,
    sources = list(
      internal = internal_import$provenance$source,
      links = links_import$provenance$source
    ),
    declared_export_kinds = list(
      internal = internal_import$provenance$export_kind,
      links = links_import$provenance$export_kind
    ),
    import_options = list(
      links = list(
        origin_policy = links_import$provenance$origin_policy,
        endpoint_action = links_import$provenance$endpoint_action
      )
    ),
    detected_columns = list(
      internal = internal_import$provenance$detected_columns,
      links = links_import$provenance$detected_columns
    ),
    aliases = list(
      internal = internal_import$provenance$aliases,
      links = links_import$provenance$aliases
    ),
    ignored_columns = list(
      internal = internal_import$provenance$ignored_columns,
      links = links_import$provenance$ignored_columns
    ),
    input_rows = list(
      internal = internal_import$diagnostics$input_rows,
      links = links_import$diagnostics$input_rows
    ),
    retained_input_row_ids = list(
      internal = internal_import$provenance$retained_input_row_ids,
      links = links_import$provenance$retained_input_row_ids
    )
  )

  structure(
    list(
      nodes = internal_import$nodes,
      observations = links_import$observations,
      edges = links_import$edges,
      redirects = internal_import$redirects,
      canonicals = internal_import$canonicals,
      indexability = internal_import$indexability,
      diagnostics = diagnostics,
      provenance = provenance
    ),
    class = "screaming_frog_bundle"
  )
}

#' Summarize a Screaming Frog bundle
#'
#' @param object A `screaming_frog_bundle` object.
#' @param ... Unused; for S3 compatibility.
#' @return A compact named list of row counts and reconciliation counts.
#' @export
summary.screaming_frog_bundle <- function(object, ...) {
  structure(
    list(
      nodes = nrow(object$nodes),
      observations = nrow(object$observations),
      edges = nrow(object$edges),
      redirects = nrow(object$redirects),
      canonicals = nrow(object$canonicals),
      canonicals_off_domain =
        object$diagnostics$counts$canonicals_off_domain,
      excluded_type_rows = object$diagnostics$links$excluded_type_rows,
      dropped_invalid_endpoints =
        object$diagnostics$links$dropped_invalid_endpoints,
      absent_internal_edge_endpoints =
        nrow(object$diagnostics$cross_table$edge_endpoints_absent),
      nodes_absent_from_graph =
        nrow(object$diagnostics$cross_table$nodes_absent_from_graph)
    ),
    class = "summary.screaming_frog_bundle"
  )
}

#' Print a Screaming Frog bundle
#'
#' @param x A `screaming_frog_bundle` object.
#' @param ... Unused; for S3 compatibility.
#' @return `x`, invisibly.
#' @export
print.screaming_frog_bundle <- function(x, ...) {
  s <- summary(x)
  cat("=== Screaming Frog Bundle ===\n\n")
  cat("Rows\n")
  cat("  Nodes:        ", s$nodes, "\n")
  cat("  Observations: ", s$observations, "\n")
  cat("  Edges:        ", s$edges, "\n")
  cat("  Redirects:    ", s$redirects, "\n")
  cat("  Canonicals:   ", s$canonicals, "\n")
  cat("\nLosses / reconciliation\n")
  cat("  Canonicals off domain:  ", s$canonicals_off_domain, "\n")
  cat("  Excluded by type:       ", s$excluded_type_rows, "\n")
  cat("  Invalid graph endpoints:", s$dropped_invalid_endpoints, "\n")
  cat("  Edge endpoints absent:  ", s$absent_internal_edge_endpoints, "\n")
  cat("  Nodes absent from graph:", s$nodes_absent_from_graph, "\n")
  invisible(x)
}

.sf_bundle_diagnostics <- function(internal, links) {
  nodes <- internal$nodes
  observations <- links$observations
  edges <- links$edges
  internal_urls <- unique(nodes$url)
  internal_hosts <- sort(unique(.sf_url_host(internal_urls)))
  internal_hosts <- internal_hosts[!is.na(internal_hosts)]
  graph_urls <- unique(c(edges$from, edges$to))

  canonical_targets_absent <- .sf_absent_signal_targets(
    internal$canonicals, internal_urls, internal_hosts
  )
  n_canonicals_off_domain <- sum(
    canonical_targets_absent$classification == "external_endpoint"
  )

  list(
    counts = list(
      nodes = nrow(nodes),
      observations = nrow(observations),
      edges = nrow(edges),
      redirects = nrow(internal$redirects),
      canonicals = nrow(internal$canonicals),
      canonicals_off_domain = n_canonicals_off_domain,
      indexability = nrow(internal$indexability)
    ),
    inputs = list(
      observations_by_type = .sf_count_df(observations, "type"),
      observations_by_origin = .sf_count_df(observations, "link_origin"),
      observations_by_position = .sf_count_df(observations, "link_position"),
      observations_by_follow = .sf_count_df(observations, "follow"),
      graph_edges_by_follow = .sf_count_df(edges, "follow"),
      graph_edges_by_placement = .sf_count_df(edges, "placement")
    ),
    links = list(
      graph_eligible_rows = links$diagnostics$graph_type_rows,
      edge_rows = links$diagnostics$edge_rows,
      excluded_type_rows = links$diagnostics$excluded_type_rows,
      excluded_types = links$diagnostics$excluded_types,
      excluded_origin_rows = links$diagnostics$excluded_origin_rows,
      dropped_missing_source = links$diagnostics$dropped_missing_source,
      dropped_missing_destination =
        links$diagnostics$dropped_missing_destination,
      dropped_invalid_endpoints =
        links$diagnostics$dropped_invalid_endpoints,
      nofollow_edges = sum(edges$nofollow %in% TRUE),
      follow_unknown_observations = sum(is.na(observations$follow)),
      rel_nofollow_observations = sum(observations$rel_nofollow %in% TRUE),
      follow_rel_disagreements =
        links$diagnostics$follow_rel_disagreements,
      placement_mapped_observations = sum(!is.na(observations$placement)),
      placement_unmapped_observations =
        links$diagnostics$unmapped_position_rows,
      duplicate_observation_rows =
        links$diagnostics$duplicate_observation_rows,
      invalid_follow_values = links$diagnostics$invalid_follow_values,
      invalid_status_codes = links$diagnostics$invalid_status_codes,
      issues = links$diagnostics$issues
    ),
    internal = list(
      dropped_missing_address = internal$diagnostics$dropped_missing_address,
      duplicate_address_rows = internal$diagnostics$duplicate_address_rows,
      invalid_status_codes = internal$diagnostics$invalid_status_codes,
      missing_3xx_destinations =
        internal$diagnostics$missing_3xx_destinations,
      ignored_non_3xx_destinations =
        internal$diagnostics$ignored_non_3xx_destinations,
      self_canonical_rows = internal$diagnostics$self_canonical_rows,
      issues = internal$diagnostics$issues
    ),
    cross_table = list(
      edge_endpoints_absent = .sf_absent_edge_endpoints(
        edges, internal_urls, internal_hosts
      ),
      nodes_absent_from_graph = .sf_nodes_absent_from_graph(
        nodes, graph_urls
      ),
      redirect_targets_absent = .sf_absent_signal_targets(
        internal$redirects, internal_urls, internal_hosts
      ),
      canonical_targets_absent = canonical_targets_absent
    ),
    distributions = list(
      hosts = .sf_count_vector(.sf_url_host(nodes$url), "host"),
      status = .sf_count_df(nodes, "http_status"),
      indexability = .sf_count_df(nodes, "indexability"),
      indexability_status = .sf_count_df(nodes, "indexability_status")
    ),
    schema = list(
      missing_optional_columns = list(
        internal = internal$diagnostics$missing_optional_columns,
        links = links$diagnostics$missing_optional_columns
      ),
      ignored_columns = list(
        internal = internal$diagnostics$ignored_columns,
        links = links$diagnostics$ignored_columns
      )
    )
  )
}

.sf_count_df <- function(x, cols) {
  if (nrow(x) == 0L) {
    out <- as.data.frame(
      stats::setNames(rep(list(character(0)), length(cols)), cols)
    )
    out$n <- integer(0)
    return(out)
  }
  values <- x[, cols, drop = FALSE]
  for (col in cols) {
    values[[col]] <- .sf_count_value(values[[col]])
  }
  values$n <- 1L
  out <- stats::aggregate(
    n ~ .,
    values,
    sum,
    drop = FALSE
  )
  out <- out[do.call(order, out[cols]), , drop = FALSE]
  row.names(out) <- NULL
  out
}

.sf_count_vector <- function(x, name) {
  out <- .sf_count_df(
    data.frame(value = x),
    "value"
  )
  names(out) <- c(name, "n")
  out
}

.sf_count_value <- function(x) {
  out <- as.character(x)
  out[is.na(out)] <- "<NA>"
  out
}

.sf_url_host <- function(url) {
  value <- trimws(as.character(url))
  out <- rep(NA_character_, length(value))
  has_scheme <- grepl("^[A-Za-z][A-Za-z0-9+.-]*://", value)
  host <- sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/?#:]+).*$", "\\1", value)
  out[has_scheme] <- tolower(host[has_scheme])
  out
}

.sf_absent_edge_endpoints <- function(edges, internal_urls, internal_hosts) {
  endpoints <- rbind(
    data.frame(
      input_row = edges$input_row,
      side = "from",
      url = edges$from
    ),
    data.frame(
      input_row = edges$input_row,
      side = "to",
      url = edges$to
    )
  )
  absent <- endpoints[!endpoints$url %in% internal_urls, , drop = FALSE]
  .sf_classify_absent_urls(absent, internal_hosts)
}

.sf_nodes_absent_from_graph <- function(nodes, graph_urls) {
  out <- nodes[!nodes$url %in% graph_urls, "url", drop = FALSE]
  names(out) <- "url"
  row.names(out) <- NULL
  out
}

.sf_absent_signal_targets <- function(signal, internal_urls, internal_hosts) {
  if (nrow(signal) == 0L) {
    out <- data.frame(
      from = character(0),
      to = character(0),
      host = character(0),
      classification = character(0)
    )
    return(out)
  }
  absent <- signal[!signal$to %in% internal_urls, c("from", "to"), drop = FALSE]
  names(absent) <- c("from", "url")
  out <- .sf_classify_absent_urls(absent, internal_hosts)
  names(out)[names(out) == "url"] <- "to"
  out
}

.sf_classify_absent_urls <- function(x, internal_hosts) {
  if (nrow(x) == 0L) {
    x$host <- character(0)
    x$classification <- character(0)
    return(x)
  }
  host <- .sf_url_host(x$url)
  classification <- rep("external_endpoint", length(host))
  classification[host %in% internal_hosts] <- "internal_host_absent"
  classification[is.na(host)] <- "malformed_url"
  out <- cbind(
    x,
    data.frame(
      host = host,
      classification = classification
    )
  )
  sort_cols <- intersect(
    c("classification", "host", "url", "input_row"),
    names(out)
  )
  out <- out[do.call(order, out[sort_cols]), , drop = FALSE]
  row.names(out) <- NULL
  out
}
