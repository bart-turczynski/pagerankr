#' Import Screaming Frog All Inlinks or All Outlinks observations
#'
#' Normalizes a Screaming Frog **All Inlinks** or **All Outlinks** CSV or data
#' frame. Both exports retain their native `Source -> Destination` orientation.
#' Raw observations are kept separately from graph-eligible edges, and URLs are
#' preserved for one downstream canonicalization pass.
#'
#' Only `Type = "Hyperlink"` observations are graph-eligible. `Follow` is the
#' authoritative field used to derive `nofollow`; `Rel` is parsed independently
#' for disagreement diagnostics. Duplicate observations are not aggregated.
#'
#' @param x A path to an All Inlinks/All Outlinks CSV file or an equivalent
#'   data frame.
#' @param export_kind Declared export provenance: `"all_inlinks"` or
#'   `"all_outlinks"`. This never changes edge orientation.
#' @param origin_policy Which DOM observations may become graph edges:
#'   `"all"` (default), `"html"`, or `"rendered"`. Combined
#'   `"HTML & Rendered HTML"` observations qualify for either selective policy.
#'   The raw observation table is never filtered.
#' @param endpoint_action How graph-eligible rows with a blank source or
#'   destination are handled: `"drop"` (default) or `"error"`. Such rows remain
#'   in `observations` in either mode.
#'
#' @return An object of class `screaming_frog_links` with components:
#'   \describe{
#'     \item{observations}{Normalized observations in input row order.}
#'     \item{edges}{Unaggregated, graph-eligible `from` / `to` rows with
#'       nofollow, placement, origin, and link provenance.}
#'     \item{diagnostics}{Input, eligibility, endpoint, type, origin,
#'       Follow/Rel, and schema counts plus row-level issues.}
#'     \item{provenance}{Export kind, source, policy, retained input-row IDs,
#'       and detected columns.}
#'   }
#'
#' @export
#' @examples
#' links <- data.frame(
#'   Type = c("Hyperlink", "Image"),
#'   Source = c("https://example.com/", "https://example.com/"),
#'   Destination = c("https://example.com/a", "https://example.com/logo.png"),
#'   Follow = c("TRUE", "TRUE"),
#'   check.names = FALSE
#' )
#' imported <- screaming_frog_links(links, "all_outlinks")
#' imported$edges
screaming_frog_links <- function(x,
                                 export_kind = c(
                                   "all_inlinks", "all_outlinks"
                                 ),
                                 origin_policy = c(
                                   "all", "html", "rendered"
                                 ),
                                 endpoint_action = c("drop", "error")) {
  export_kind <- match.arg(export_kind)
  origin_policy <- match.arg(origin_policy)
  endpoint_action <- match.arg(endpoint_action)

  raw <- sf_read_input(x, export_kind)
  schema <- attr(raw, "sf_schema")
  input_rows <- nrow(raw)
  input_row <- seq_len(input_rows)
  fields <- sf_contract()$links$order
  missing_optional <- setdiff(
    setdiff(fields, sf_contract()$links$required),
    names(raw)
  )
  raw <- .sf_add_missing_columns(raw, fields)

  d <- .sf_links_classify(raw, origin_policy)
  if (endpoint_action == "error" && any(d$invalid_graph_endpoints)) {
    stop(
      "Screaming Frog link input has ",
      sum(d$invalid_graph_endpoints),
      " graph-eligible row(s) with a blank source or destination.",
      call. = FALSE
    )
  }

  observations <- .sf_links_observations(raw, input_row, d)
  edges <- .sf_links_edges(raw, input_row, d)
  issues <- .sf_links_issues(raw, input_row, d)
  diagnostics <- .sf_links_diagnostics(
    raw, d, observations, edges, input_rows, missing_optional, schema, issues
  )
  provenance <- .sf_links_provenance(
    x, export_kind, origin_policy, endpoint_action,
    schema, input_rows, input_row, d
  )

  structure(
    list(
      observations = observations,
      edges = edges,
      diagnostics = diagnostics,
      provenance = provenance
    ),
    class = "screaming_frog_links"
  )
}

#' Derive per-row link classifications, edge selection, and issue flags
#'
#' @keywords internal
#' @noRd
.sf_links_classify <- function(raw, origin_policy) {
  follow <- sf_parse_follow(raw$follow)
  rel_nofollow <- sf_rel_nofollow(raw$rel)
  placement <- sf_normalize_position(raw$link_position)
  status_code <- .sf_parse_status_code(raw$status_code)
  graph_type <- sf_graph_eligible(raw$type)
  valid_source <- !is.na(raw$source)
  valid_destination <- !is.na(raw$destination)
  valid_endpoints <- valid_source & valid_destination
  origin_eligible <- .sf_origin_eligible(raw$link_origin, origin_policy)
  edge_rows <- graph_type & valid_endpoints & origin_eligible
  invalid_graph_endpoints <- graph_type & !valid_endpoints

  invalid_follow <- !is.na(raw$follow) & is.na(follow)
  follow_rel_disagreement <- !is.na(follow) & !is.na(rel_nofollow) &
    ((!follow) != rel_nofollow)
  unmapped_position <- !is.na(raw$link_position) & is.na(placement)
  invalid_status <- !is.na(raw$status_code) & is.na(status_code)

  list(
    follow = follow,
    rel_nofollow = rel_nofollow,
    placement = placement,
    status_code = status_code,
    graph_type = graph_type,
    valid_source = valid_source,
    valid_destination = valid_destination,
    valid_endpoints = valid_endpoints,
    origin_eligible = origin_eligible,
    edge_rows = edge_rows,
    invalid_graph_endpoints = invalid_graph_endpoints,
    invalid_follow = invalid_follow,
    follow_rel_disagreement = follow_rel_disagreement,
    unmapped_position = unmapped_position,
    invalid_status = invalid_status
  )
}

#' Build the lossless normalized link observation table
#'
#' @keywords internal
#' @noRd
.sf_links_observations <- function(raw, input_row, d) {
  data.frame(
    input_row = input_row,
    type = raw$type,
    source = raw$source,
    source_segments = .sf_parse_integer(raw$source_segments),
    destination = raw$destination,
    destination_segments = .sf_parse_integer(raw$destination_segments),
    size_bytes = .sf_parse_number(raw$size_bytes),
    alt_text = raw$alt_text,
    anchor = raw$anchor,
    http_status = d$status_code,
    status = raw$status,
    crawlability = raw$crawlability,
    follow = d$follow,
    target = raw$target,
    rel = raw$rel,
    rel_nofollow = d$rel_nofollow,
    path_type = raw$path_type,
    link_path = raw$link_path,
    link_position = raw$link_position,
    placement = d$placement,
    link_origin = raw$link_origin
  )
}

#' Build the graph-eligible edge table
#'
#' @keywords internal
#' @noRd
.sf_links_edges <- function(raw, input_row, d) {
  edge_rows <- d$edge_rows
  follow <- d$follow
  data.frame(
    input_row = input_row[edge_rows],
    from = raw$source[edge_rows],
    to = raw$destination[edge_rows],
    nofollow = !follow[edge_rows],
    follow = follow[edge_rows],
    rel = raw$rel[edge_rows],
    rel_nofollow = d$rel_nofollow[edge_rows],
    anchor = raw$anchor[edge_rows],
    alt_text = raw$alt_text[edge_rows],
    target = raw$target[edge_rows],
    path_type = raw$path_type[edge_rows],
    link_path = raw$link_path[edge_rows],
    link_position = raw$link_position[edge_rows],
    placement = d$placement[edge_rows],
    link_origin = raw$link_origin[edge_rows],
    destination_status_code = d$status_code[edge_rows],
    destination_status = raw$status[edge_rows],
    destination_crawlability = raw$crawlability[edge_rows]
  )
}

#' Assemble row-level link issue records
#'
#' @keywords internal
#' @noRd
.sf_links_issues <- function(raw, input_row, d) {
  issues <- data.frame(
    input_row = integer(0),
    field = character(0),
    value = character(0),
    issue = character(0)
  )
  issues <- .sf_append_issues(
    issues, input_row[!d$valid_source], "source", raw$source[!d$valid_source],
    "missing_endpoint"
  )
  issues <- .sf_append_issues(
    issues, input_row[!d$valid_destination], "destination",
    raw$destination[!d$valid_destination], "missing_endpoint"
  )
  issues <- .sf_append_issues(
    issues, input_row[d$invalid_follow], "follow", raw$follow[d$invalid_follow],
    "invalid_follow_value"
  )
  issues <- .sf_append_issues(
    issues, input_row[d$follow_rel_disagreement], "rel",
    raw$rel[d$follow_rel_disagreement], "follow_rel_disagreement"
  )
  issues <- .sf_append_issues(
    issues, input_row[d$unmapped_position], "link_position",
    raw$link_position[d$unmapped_position], "unmapped_position"
  )
  issues <- .sf_append_issues(
    issues, input_row[d$invalid_status], "status_code",
    raw$status_code[d$invalid_status], "invalid_status_code"
  )
  issues
}

#' Build the link import diagnostics list
#'
#' @keywords internal
#' @noRd
.sf_links_diagnostics <- function(raw, d, observations, edges, input_rows,
                                  missing_optional, schema, issues) {
  excluded_types <- unique(raw$type[!d$graph_type & !is.na(raw$type)])
  list(
    input_rows = input_rows,
    observation_rows = nrow(observations),
    graph_type_rows = sum(d$graph_type),
    edge_rows = nrow(edges),
    duplicate_observation_rows = .sf_duplicate_link_rows(raw),
    dropped_missing_source = sum(d$graph_type & !d$valid_source),
    dropped_missing_destination = sum(d$graph_type & !d$valid_destination),
    dropped_invalid_endpoints = sum(d$invalid_graph_endpoints),
    excluded_type_rows = sum(!d$graph_type),
    excluded_types = excluded_types,
    excluded_origin_rows = sum(
      d$graph_type & d$valid_endpoints & !d$origin_eligible
    ),
    invalid_follow_values = sum(d$invalid_follow),
    follow_rel_disagreements = sum(d$follow_rel_disagreement),
    unmapped_position_rows = sum(d$unmapped_position),
    invalid_status_codes = sum(d$invalid_status),
    missing_optional_columns = missing_optional,
    ignored_columns = schema$ignored_columns,
    issues = issues
  )
}

#' Build the link import provenance list
#'
#' @keywords internal
#' @noRd
.sf_links_provenance <- function(x, export_kind, origin_policy, endpoint_action,
                                 schema, input_rows, input_row, d) {
  list(
    export_kind = export_kind,
    source = if (is.data.frame(x)) "<data.frame>" else x,
    origin_policy = origin_policy,
    endpoint_action = endpoint_action,
    detected_columns = schema$columns,
    aliases = schema$aliases,
    ignored_columns = schema$ignored_columns,
    input_rows = input_rows,
    retained_input_row_ids = input_row[d$edge_rows]
  )
}

.sf_origin_eligible <- function(x, policy) {
  if (identical(policy, "all")) {
    return(rep(TRUE, length(x)))
  }
  value <- tolower(trimws(as.character(x)))
  if (identical(policy, "html")) {
    value %in% c("html", "html & rendered html")
  } else {
    value %in% c("rendered html", "html & rendered html")
  }
}

.sf_duplicate_link_rows <- function(x) {
  sum(duplicated(x) | duplicated(x, fromLast = TRUE))
}
