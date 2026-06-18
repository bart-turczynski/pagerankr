#' Import Screaming Frog Internal: All node facts
#'
#' Normalizes a Screaming Frog **Internal: All** CSV or data frame into node,
#' redirect, canonical, and indexability tables. This is a node-side adapter:
#' it does not reconstruct links from aggregate Inlinks/Outlinks counts.
#'
#' URLs are preserved as exported. Redirects are emitted only for valid 3xx
#' rows with a non-blank destination. Canonicals are derived independently,
#' including self-canonicals for audit.
#'
#' @param x A path to an Internal: All CSV file or an equivalent data frame.
#'
#' @return An object of class `screaming_frog_internal` with components:
#'   \describe{
#'     \item{nodes}{Normalized node facts in input row order. Columns are
#'       `url`, `segments`, `content_type`, `http_status`, `status`,
#'       `indexability`, `indexability_status`, `canonical`, `redirect_to`,
#'       `redirect_type`, `crawl_allowed`, `indexing_allowed`, robots fields,
#'       language/timestamps, and selected crawl metrics. Optional absent
#'       fields are typed `NA` columns.}
#'     \item{redirects}{Raw `from` / `to` redirect pairs from valid 3xx rows.}
#'     \item{canonicals}{Raw `from` / `to` canonical pairs, including
#'       self-canonicals.}
#'     \item{indexability}{URL-level facts compatible with `pagerank()`'s
#'       indexability input.}
#'     \item{diagnostics}{Deterministic counts, missing optional and ignored
#'       columns, duplicate addresses, and row-level structural issues.}
#'     \item{provenance}{Input identity, retained input-row IDs, and the
#'       normalized-to-detected column manifest.}
#'   }
#'
#' @export
#' @examples
#' internal <- data.frame(
#'   Address = c("https://example.com/", "https://example.com/old"),
#'   `Status Code` = c("200", "301"),
#'   `Redirect URL` = c("", "https://example.com/new"),
#'   check.names = FALSE
#' )
#' imported <- screaming_frog_internal(internal)
#' imported$nodes
#' imported$redirects
screaming_frog_internal <- function(x) {
  raw <- .sf_read_input(x, "internal_all")
  schema <- attr(raw, "sf_schema")
  input_rows <- nrow(raw)
  input_row <- seq_len(input_rows)

  fields <- .sf_contract()$internal$order
  missing_optional <- setdiff(
    setdiff(fields, .sf_contract()$internal$required),
    names(raw)
  )
  raw <- .sf_add_missing_columns(raw, fields)

  status_code <- .sf_parse_status_code(raw$status_code)
  valid_address <- !is.na(raw$address)
  valid_status <- !is.na(status_code)
  redirect_destination <- !is.na(raw$redirect_to)
  canonical_destination <- !is.na(raw$canonical)
  is_redirect_status <- valid_status &
    status_code >= 300L & status_code <= 399L

  node_rows <- input_row[valid_address]
  nodes <- data.frame(
    url = raw$address[valid_address],
    segments = .sf_parse_integer(raw$segments[valid_address]),
    content_type = raw$content_type[valid_address],
    http_status = status_code[valid_address],
    status = raw$status[valid_address],
    indexability = raw$indexability[valid_address],
    indexability_status = raw$indexability_status[valid_address],
    canonical = raw$canonical[valid_address],
    redirect_to = raw$redirect_to[valid_address],
    redirect_type = raw$redirect_type[valid_address],
    crawl_allowed = .sf_parse_allowed(raw$crawl_allowed[valid_address]),
    indexing_allowed = .sf_parse_allowed(raw$indexing_allowed[valid_address]),
    meta_robots = raw$meta_robots[valid_address],
    x_robots_tag = raw$x_robots_tag[valid_address],
    language = raw$language[valid_address],
    crawl_timestamp = raw$crawl_timestamp[valid_address],
    last_modified = raw$last_modified[valid_address],
    size_bytes = .sf_parse_number(raw$size_bytes[valid_address]),
    word_count = .sf_parse_integer(raw$word_count[valid_address]),
    inlinks = .sf_parse_integer(raw$inlinks[valid_address]),
    unique_inlinks = .sf_parse_integer(raw$unique_inlinks[valid_address]),
    outlinks = .sf_parse_integer(raw$outlinks[valid_address]),
    unique_outlinks = .sf_parse_integer(raw$unique_outlinks[valid_address]),
    response_time_seconds = .sf_parse_number(
      raw$response_time_seconds[valid_address]
    ),
    stringsAsFactors = FALSE
  )

  redirect_rows <- valid_address & is_redirect_status & redirect_destination
  redirects <- data.frame(
    from = raw$address[redirect_rows],
    to = raw$redirect_to[redirect_rows],
    status_code = status_code[redirect_rows],
    redirect_type = raw$redirect_type[redirect_rows],
    stringsAsFactors = FALSE
  )

  canonical_rows <- valid_address & canonical_destination
  canonicals <- data.frame(
    from = raw$address[canonical_rows],
    to = raw$canonical[canonical_rows],
    stringsAsFactors = FALSE
  )

  indexability <- data.frame(
    url = raw$address[valid_address],
    indexability = raw$indexability[valid_address],
    indexability_status = raw$indexability_status[valid_address],
    crawl_allowed = .sf_parse_allowed(raw$crawl_allowed[valid_address]),
    indexing_allowed = .sf_parse_allowed(
      raw$indexing_allowed[valid_address]
    ),
    meta_robots = raw$meta_robots[valid_address],
    x_robots_tag = raw$x_robots_tag[valid_address],
    stringsAsFactors = FALSE
  )

  duplicate_address <- valid_address & (
    duplicated(raw$address) | duplicated(raw$address, fromLast = TRUE)
  )
  invalid_status <- !is.na(raw$status_code) & !valid_status
  invalid_crawl_allowed <- .sf_invalid_allowed(raw$crawl_allowed)
  invalid_indexing_allowed <- .sf_invalid_allowed(raw$indexing_allowed)

  issues <- data.frame(
    input_row = integer(0),
    field = character(0),
    value = character(0),
    issue = character(0),
    stringsAsFactors = FALSE
  )
  issues <- .sf_append_issues(
    issues, input_row[!valid_address], "address",
    raw$address[!valid_address], "missing_required_value"
  )
  issues <- .sf_append_issues(
    issues, input_row[invalid_status], "status_code",
    raw$status_code[invalid_status], "invalid_status_code"
  )
  issues <- .sf_append_issues(
    issues, input_row[is_redirect_status & !redirect_destination],
    "redirect_to", raw$redirect_to[is_redirect_status & !redirect_destination],
    "missing_3xx_destination"
  )
  issues <- .sf_append_issues(
    issues, input_row[!is_redirect_status & redirect_destination],
    "redirect_to", raw$redirect_to[!is_redirect_status & redirect_destination],
    "destination_on_non_3xx"
  )
  issues <- .sf_append_issues(
    issues, input_row[invalid_crawl_allowed], "crawl_allowed",
    raw$crawl_allowed[invalid_crawl_allowed], "invalid_allowed_value"
  )
  issues <- .sf_append_issues(
    issues, input_row[invalid_indexing_allowed], "indexing_allowed",
    raw$indexing_allowed[invalid_indexing_allowed], "invalid_allowed_value"
  )

  diagnostics <- list(
    input_rows = input_rows,
    node_rows = nrow(nodes),
    dropped_missing_address = sum(!valid_address),
    invalid_status_codes = sum(invalid_status),
    duplicate_address_rows = sum(duplicate_address),
    duplicate_addresses = unique(raw$address[duplicate_address]),
    redirect_rows = nrow(redirects),
    missing_3xx_destinations = sum(
      is_redirect_status & !redirect_destination
    ),
    ignored_non_3xx_destinations = sum(
      !is_redirect_status & redirect_destination
    ),
    canonical_rows = nrow(canonicals),
    self_canonical_rows = sum(
      canonical_rows & raw$address == raw$canonical,
      na.rm = TRUE
    ),
    missing_optional_columns = missing_optional,
    ignored_columns = schema$ignored_columns,
    issues = issues
  )

  provenance <- list(
    export_kind = "internal_all",
    source = if (is.data.frame(x)) "<data.frame>" else x,
    detected_columns = schema$columns,
    aliases = schema$aliases,
    ignored_columns = schema$ignored_columns,
    input_rows = input_rows,
    retained_input_row_ids = node_rows
  )

  structure(
    list(
      nodes = nodes,
      redirects = redirects,
      canonicals = canonicals,
      indexability = indexability,
      diagnostics = diagnostics,
      provenance = provenance
    ),
    class = "screaming_frog_internal"
  )
}

.sf_add_missing_columns <- function(x, fields) {
  for (field in setdiff(fields, names(x))) {
    x[[field]] <- NA_character_
  }
  x[, fields, drop = FALSE]
}

.sf_parse_status_code <- function(x) {
  value <- trimws(as.character(x))
  valid <- !is.na(x) & grepl("^[0-9]{3}$", value)
  out <- rep(NA_integer_, length(value))
  parsed <- suppressWarnings(as.integer(value[valid]))
  parsed[parsed < 100L | parsed > 599L] <- NA_integer_
  out[valid] <- parsed
  out
}

.sf_parse_number <- function(x) {
  value <- trimws(as.character(x))
  value <- gsub(",", "", value, fixed = TRUE)
  suppressWarnings(as.numeric(value))
}

.sf_parse_integer <- function(x) {
  value <- .sf_parse_number(x)
  value[value != floor(value)] <- NA_real_
  as.integer(value)
}

.sf_parse_allowed <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(value))
  out[value %in% c("allowed", "true", "yes", "1")] <- TRUE
  out[value %in% c("not allowed", "disallowed", "false", "no", "0")] <- FALSE
  out
}

.sf_invalid_allowed <- function(x) {
  !is.na(x) & is.na(.sf_parse_allowed(x))
}

.sf_append_issues <- function(issues, rows, field, values, issue) {
  if (length(rows) == 0L) {
    return(issues)
  }
  rbind(
    issues,
    data.frame(
      input_row = rows,
      field = rep(field, length(rows)),
      value = as.character(values),
      issue = rep(issue, length(rows)),
      stringsAsFactors = FALSE
    )
  )
}
