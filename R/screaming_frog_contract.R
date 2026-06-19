#' Screaming Frog import and bundle contract
#'
#' `pagerankr` accepts a Screaming Frog **Internal: All** export as node
#' metadata and one **All Inlinks** or **All Outlinks** bulk export as link
#' observations. Internal: All is never treated as an edge list. Both link
#' exports use `Source -> Destination`; the export kind records provenance and
#' never changes orientation.
#'
#' @section Input boundary:
#' Future Screaming Frog adapters accept either a file path or a data frame.
#' Files are CSV, may contain a UTF-8 byte-order mark, and are read by first
#' inspecting the header and then selecting only contract columns. This keeps
#' 500+ MB link exports bounded to the columns needed by the requested adapter.
#' Data-frame inputs follow the same alias resolution and column ordering.
#' Unknown extra columns are ignored and reported; missing required columns
#' fail with their normalized contract names.
#'
#' Raw URLs are preserved at this boundary. URL cleaning belongs to the
#' scoring path and is performed once with `pagerankr`'s pinned `rurl`
#' canonicalization profile.
#'
#' @section Link observation semantics:
#' Raw link observations and graph-eligible edges are separate tables.
#' Duplicate observations are preserved. Only rows whose `type` is
#' `"Hyperlink"` are graph-eligible by default. Resource, sitemap, hreflang,
#' redirect, and canonical observations are not silently promoted to PageRank
#' edges; redirects and canonicals remain dedicated signals.
#'
#' `follow` is the primary nofollow field. `rel` is retained and parsed
#' independently for diagnostics, so disagreements can be reported. Link
#' position is normalized only for `Navigation`, `Content`, `Footer`, `Header`,
#' and `Aside`; `Head`, blanks, and unknown values remain unmapped rather than
#' being guessed. Link origin and link path are preserved as provenance. Link
#' path is an XPath-like source locator, not a URL path.
#'
#' @section Stable bundle shape:
#' The public `screaming_frog_bundle` object introduced by the adapter tickets
#' is an S3 list with these stable top-level fields, in order:
#' \describe{
#'   \item{nodes}{Normalized Internal: All node facts.}
#'   \item{observations}{Lossless normalized link observations.}
#'   \item{edges}{The graph-eligible `from` / `to` subset.}
#'   \item{redirects}{`from` / `to` redirect signals.}
#'   \item{canonicals}{`from` / `to` canonical signals.}
#'   \item{indexability}{URL-level indexability facts.}
#'   \item{diagnostics}{Counts, omissions, invalid values, and disagreements.}
#'   \item{provenance}{Input kind, source, schema aliases, and schema clues.}
#' }
#'
#' This topic freezes the contract consumed by the SF1-SF5 implementation
#' tickets. The complete object is constructed by [screaming_frog_bundle()].
#'
#' @name screaming_frog_bundle
NULL

.sf_contract <- function() {
  list(
    version = 1L,
    bundle_fields = c(
      "nodes", "observations", "edges", "redirects", "canonicals",
      "indexability", "diagnostics", "provenance"
    ),
    export_kinds = c("internal_all", "all_inlinks", "all_outlinks"),
    graph_eligible_types = "Hyperlink",
    internal = list(
      order = c(
        "address", "segments", "content_type", "status_code", "status",
        "indexability", "indexability_status", "canonical", "redirect_to",
        "redirect_type", "crawl_allowed", "indexing_allowed",
        "meta_robots", "x_robots_tag", "language", "crawl_timestamp",
        "last_modified", "size_bytes", "word_count", "inlinks",
        "unique_inlinks", "outlinks", "unique_outlinks",
        "response_time_seconds"
      ),
      required = c("address", "status_code"),
      aliases = list(
        address = c("Address", "URL", "URI"),
        segments = c("Segments"),
        content_type = c("Content Type", "Content-Type"),
        status_code = c("Status Code", "HTTP Status Code"),
        status = c("Status"),
        indexability = c("Indexability"),
        indexability_status = c("Indexability Status"),
        canonical = c(
          "Canonical Link Element 1", "Canonical Link Element", "Canonical"
        ),
        redirect_to = c("Redirect URL", "Redirect URI", "Location"),
        redirect_type = c("Redirect Type"),
        crawl_allowed = c("Crawl Allowed"),
        indexing_allowed = c("Indexing Allowed"),
        meta_robots = c("Meta Robots 1", "Meta Robots"),
        x_robots_tag = c("X-Robots-Tag 1", "X-Robots-Tag"),
        language = c("Language"),
        crawl_timestamp = c("Crawl Timestamp", "Crawled At"),
        last_modified = c("Last Modified"),
        size_bytes = c("Size (Bytes)", "Size (bytes)", "Size Bytes"),
        word_count = c("Word Count"),
        inlinks = c("Inlinks"),
        unique_inlinks = c("Unique Inlinks"),
        outlinks = c("Outlinks"),
        unique_outlinks = c("Unique Outlinks"),
        response_time_seconds = c(
          "Response Time", "Response Time (Seconds)", "Response Time Seconds"
        )
      )
    ),
    links = list(
      order = c(
        "type", "source", "source_segments", "destination",
        "destination_segments", "size_bytes", "alt_text", "anchor",
        "status_code", "status", "crawlability", "follow", "target", "rel",
        "path_type", "link_path", "link_position", "link_origin"
      ),
      required = c("type", "source", "destination", "follow"),
      aliases = list(
        type = c("Type", "Link Type"),
        source = c("Source", "Source URL", "From"),
        source_segments = c("Source Segments"),
        destination = c("Destination", "Destination URL", "To"),
        destination_segments = c("Destination Segments"),
        size_bytes = c("Size (Bytes)", "Size (bytes)", "Size Bytes"),
        alt_text = c("Alt Text"),
        anchor = c("Anchor", "Anchor Text"),
        status_code = c("Status Code", "HTTP Status Code"),
        status = c("Status"),
        crawlability = c("Crawlability"),
        follow = c("Follow"),
        target = c("Target"),
        rel = c("Rel"),
        path_type = c("Path Type"),
        link_path = c("Link Path"),
        link_position = c("Link Position"),
        link_origin = c("Link Origin")
      )
    )
  )
}

.sf_schema <- function(export_kind) {
  export_kind <- match.arg(export_kind, .sf_contract()$export_kinds)
  if (identical(export_kind, "internal_all")) {
    .sf_contract()$internal
  } else {
    .sf_contract()$links
  }
}

.sf_header_key <- function(x) {
  x <- sub("^\ufeff", "", as.character(x))
  x <- tolower(trimws(x))
  gsub("[^a-z0-9]+", "", x)
}

.sf_resolve_schema <- function(column_names, export_kind) {
  schema <- .sf_schema(export_kind)
  input_keys <- .sf_header_key(column_names)
  resolved <- stats::setNames(rep(NA_character_, length(schema$order)), schema$order)

  for (field in schema$order) {
    alias_keys <- .sf_header_key(c(field, schema$aliases[[field]]))
    hit <- match(alias_keys, input_keys, nomatch = 0L)
    hit <- hit[hit > 0L]
    if (length(hit) > 0L) {
      resolved[[field]] <- column_names[[hit[[1L]]]]
    }
  }

  missing <- schema$required[is.na(resolved[schema$required])]
  if (length(missing) > 0L) {
    stop(
      "Screaming Frog `", export_kind,
      "` input is missing required column(s): ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  used <- unname(resolved[!is.na(resolved)])
  list(
    columns = resolved,
    aliases = resolved[resolved != names(resolved) & !is.na(resolved)],
    ignored = setdiff(column_names, used)
  )
}

.sf_read_input <- function(x, export_kind, fields = NULL) {
  export_kind <- match.arg(export_kind, .sf_contract()$export_kinds)
  schema <- .sf_schema(export_kind)

  if (is.null(fields)) {
    fields <- schema$order
  }
  if (!is.character(fields) || anyNA(fields) ||
        any(!fields %in% schema$order)) {
    stop(
      "`fields` must contain normalized fields from the `",
      export_kind, "` contract.",
      call. = FALSE
    )
  }
  fields <- schema$order[schema$order %in% unique(fields)]
  required_fields <- unique(c(schema$required, fields))

  if (is.data.frame(x)) {
    resolved <- .sf_resolve_schema(names(x), export_kind)
    selected <- resolved$columns[required_fields]
    selected <- selected[!is.na(selected)]
    out <- x[, unname(selected), drop = FALSE]
  } else if (is.character(x) && length(x) == 1L && !is.na(x)) {
    if (!file.exists(x)) {
      stop("Screaming Frog input file does not exist: ", x, call. = FALSE)
    }
    header <- utils::read.csv(
      x,
      nrows = 0L,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
    resolved <- .sf_resolve_schema(names(header), export_kind)
    selected <- resolved$columns[required_fields]
    selected <- selected[!is.na(selected)]
    col_classes <- rep("NULL", ncol(header))
    col_classes[match(unname(selected), names(header))] <- "character"
    out <- utils::read.csv(
      x,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      colClasses = col_classes,
      na.strings = character(0),
      fileEncoding = "UTF-8-BOM"
    )
  } else {
    stop("`x` must be a data frame or a single file path.", call. = FALSE)
  }

  canonical_names <- names(resolved$columns)[
    match(names(out), unname(resolved$columns))
  ]
  names(out) <- canonical_names
  out <- out[, fields[fields %in% names(out)], drop = FALSE]
  for (field in names(out)) {
    if (is.factor(out[[field]])) {
      out[[field]] <- as.character(out[[field]])
    }
    if (is.character(out[[field]])) {
      out[[field]] <- trimws(out[[field]])
      out[[field]][out[[field]] == ""] <- NA_character_
    }
  }

  attr(out, "sf_schema") <- list(
    export_kind = export_kind,
    columns = resolved$columns,
    aliases = resolved$aliases,
    ignored_columns = resolved$ignored
  )
  out
}

.sf_parse_follow <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(value))
  out[value %in% c("true", "yes", "1", "follow")] <- TRUE
  out[value %in% c("false", "no", "0", "nofollow")] <- FALSE
  out[is.na(x) | value == ""] <- NA
  out
}

.sf_rel_nofollow <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- vapply(strsplit(value, "[,[:space:]]+"), function(tokens) {
    "nofollow" %in% tokens
  }, logical(1))
  out[is.na(x) | value == ""] <- NA
  out
}

.sf_normalize_position <- function(x) {
  value <- tolower(trimws(as.character(x)))
  normalized <- c(
    navigation = "nav",
    header = "header",
    footer = "footer",
    aside = "sidebar",
    content = "content"
  )
  out <- unname(normalized[value])
  out[is.na(x) | value == ""] <- NA_character_
  out
}

.sf_graph_eligible <- function(type) {
  trimws(as.character(type)) %in% .sf_contract()$graph_eligible_types
}
