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

#' Screaming Frog import contract
#'
#' @description Returns the frozen contract that governs how Screaming Frog
#'   exports are read and normalized: the accepted export kinds, the column
#'   schemas (canonical field order, required fields, and header aliases) for
#'   the Internal and Inlinks/Outlinks exports, and which link types count as
#'   graph-eligible. Inspect it to see exactly which Screaming Frog column
#'   headers are recognized before importing a crawl.
#'
#' @return A list with components:
#'   \describe{
#'     \item{version}{Integer contract version.}
#'     \item{bundle_fields}{Character vector of the fields present on a
#'       \code{\link{screaming_frog_bundle}()} object.}
#'     \item{export_kinds}{Character vector of accepted export kinds, used by
#'       \code{\link{sf_read_input}()}.}
#'     \item{graph_eligible_types}{Link types treated as graph edges; see
#'       \code{\link{sf_graph_eligible}()}.}
#'     \item{internal, links}{Schemas for the Internal and Inlinks/Outlinks
#'       exports, each a list of \code{order}, \code{required}, and
#'       \code{aliases}.}
#'   }
#'
#' @family Screaming Frog toolkit
#' @export
#' @examples
#' contract <- sf_contract()
#' contract$export_kinds
#' contract$graph_eligible_types
#'
#' # Which Screaming Frog headers map onto the `address` field?
#' contract$internal$aliases$address
sf_contract <- function() {
  list(
    version = 1L,
    bundle_fields = c(
      "nodes", "observations", "edges", "redirects", "canonicals",
      "indexability", "diagnostics", "provenance"
    ),
    export_kinds = c("internal_all", "all_inlinks", "all_outlinks"),
    graph_eligible_types = "Hyperlink",
    internal = .sf_contract_internal(),
    links = .sf_contract_links()
  )
}

#' Internal: All contract schema (field order, required, aliases)
#'
#' @keywords internal
#' @noRd
.sf_contract_internal <- function() {
  list(
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
      segments = "Segments",
      content_type = c("Content Type", "Content-Type"),
      status_code = c("Status Code", "HTTP Status Code"),
      status = "Status",
      indexability = "Indexability",
      indexability_status = "Indexability Status",
      canonical = c(
        "Canonical Link Element 1", "Canonical Link Element", "Canonical"
      ),
      redirect_to = c("Redirect URL", "Redirect URI", "Location"),
      redirect_type = "Redirect Type",
      crawl_allowed = "Crawl Allowed",
      indexing_allowed = "Indexing Allowed",
      meta_robots = c("Meta Robots 1", "Meta Robots"),
      x_robots_tag = c("X-Robots-Tag 1", "X-Robots-Tag"),
      language = "Language",
      crawl_timestamp = c("Crawl Timestamp", "Crawled At"),
      last_modified = "Last Modified",
      size_bytes = c("Size (Bytes)", "Size (bytes)", "Size Bytes"),
      word_count = "Word Count",
      inlinks = "Inlinks",
      unique_inlinks = "Unique Inlinks",
      outlinks = "Outlinks",
      unique_outlinks = "Unique Outlinks",
      response_time_seconds = c(
        "Response Time", "Response Time (Seconds)", "Response Time Seconds"
      )
    )
  )
}

#' Link export contract schema (field order, required, aliases)
#'
#' @keywords internal
#' @noRd
.sf_contract_links <- function() {
  list(
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
      source_segments = "Source Segments",
      destination = c("Destination", "Destination URL", "To"),
      destination_segments = "Destination Segments",
      size_bytes = c("Size (Bytes)", "Size (bytes)", "Size Bytes"),
      alt_text = "Alt Text",
      anchor = c("Anchor", "Anchor Text"),
      status_code = c("Status Code", "HTTP Status Code"),
      status = "Status",
      crawlability = "Crawlability",
      follow = "Follow",
      target = "Target",
      rel = "Rel",
      path_type = "Path Type",
      link_path = "Link Path",
      link_position = "Link Position",
      link_origin = "Link Origin"
    )
  )
}

.sf_schema <- function(export_kind) {
  export_kind <- match.arg(export_kind, sf_contract()$export_kinds)
  if (identical(export_kind, "internal_all")) {
    sf_contract()$internal
  } else {
    sf_contract()$links
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
  resolved <- stats::setNames(
    rep(NA_character_, length(schema$order)), schema$order
  )

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
      toString(missing), ".",
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

#' @noRd
.sf_validate_fields <- function(fields, schema, export_kind) {
  if (is.null(fields)) {
    fields <- schema$order
  }
  ok <- is.character(fields)
  if (ok && anyNA(fields)) {
    ok <- FALSE
  }
  if (ok && !all(fields %in% schema$order)) {
    ok <- FALSE
  }
  if (!ok) {
    stop(
      "`fields` must contain normalized fields from the `",
      export_kind, "` contract.",
      call. = FALSE
    )
  }
  schema$order[schema$order %in% unique(fields)]
}

#' @noRd
.sf_read_df <- function(x, export_kind, required_fields) {
  resolved <- .sf_resolve_schema(names(x), export_kind)
  selected <- resolved$columns[required_fields]
  selected <- selected[!is.na(selected)]
  out <- x[, unname(selected), drop = FALSE]
  list(out = out, resolved = resolved)
}

#' @noRd
.sf_read_file <- function(x, export_kind, required_fields) {
  if (!file.exists(x)) {
    stop("Screaming Frog input file does not exist: ", x, call. = FALSE)
  }
  header <- utils::read.csv(
    x,
    nrows = 0L,
    check.names = FALSE,
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
    colClasses = col_classes,
    na.strings = character(0),
    fileEncoding = "UTF-8-BOM"
  )
  list(out = out, resolved = resolved)
}

#' @noRd
.sf_dispatch_read <- function(x, export_kind, required_fields) {
  if (is.data.frame(x)) {
    return(.sf_read_df(x, export_kind, required_fields))
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(.sf_read_file(x, export_kind, required_fields))
  }
  stop("`x` must be a data frame or a single file path.", call. = FALSE)
}

#' @noRd
.sf_finalize_output <- function(out, resolved, fields, export_kind) {
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

#' Read a Screaming Frog export into a normalized data frame
#'
#' @description Reads a Screaming Frog export -- either an in-memory data frame
#'   or a path to a CSV/Excel file -- and returns it with canonical snake_case
#'   column names, validated against the schema for \code{export_kind}. Header
#'   aliases are resolved (e.g. \code{"Address"}, \code{"URL"}, and
#'   \code{"URI"} all map to \code{address}), empty strings become \code{NA},
#'   and character columns are trimmed.
#'
#' @param x A data frame, or a path to a Screaming Frog CSV/Excel export.
#' @param export_kind Character, which export is being read. One of
#'   \code{sf_contract()$export_kinds}: \code{"internal_all"},
#'   \code{"all_inlinks"}, or \code{"all_outlinks"}.
#' @param fields Optional character vector of additional (non-required) fields
#'   to retain beyond the schema's required set. Default \code{NULL} keeps the
#'   schema's standard field order.
#'
#' @return A data frame with canonical snake_case columns. The resolved schema
#'   is attached as the \code{"sf_schema"} attribute, a list of
#'   \code{export_kind}, \code{columns}, \code{aliases}, and
#'   \code{ignored_columns}.
#'
#' @family Screaming Frog toolkit
#' @export
#' @examples
#' crawl <- data.frame(
#'   Address = c("https://example.com/", "https://example.com/a"),
#'   `Status Code` = c(200, 200),
#'   check.names = FALSE
#' )
#' out <- sf_read_input(crawl, "internal_all")
#' names(out)
#' attr(out, "sf_schema")$export_kind
sf_read_input <- function(x, export_kind, fields = NULL) {
  export_kind <- match.arg(export_kind, sf_contract()$export_kinds)
  schema <- .sf_schema(export_kind)

  fields <- .sf_validate_fields(fields, schema, export_kind)
  required_fields <- unique(c(schema$required, fields))

  read <- .sf_dispatch_read(x, export_kind, required_fields)
  .sf_finalize_output(read$out, read$resolved, fields, export_kind)
}

#' Parse a Screaming Frog follow flag to logical
#'
#' @description Converts the values Screaming Frog writes in a "Follow" column
#'   into a logical vector. Matching is case-insensitive and whitespace is
#'   trimmed.
#'
#' @param x A vector (typically character) of follow flags. \code{"true"},
#'   \code{"yes"}, \code{"1"}, and \code{"follow"} become \code{TRUE};
#'   \code{"false"}, \code{"no"}, \code{"0"}, and \code{"nofollow"} become
#'   \code{FALSE}.
#'
#' @return A logical vector the same length as \code{x}. Blank strings,
#'   \code{NA}, and unrecognized values yield \code{NA}.
#'
#' @family Screaming Frog toolkit
#' @export
#' @examples
#' sf_parse_follow(c("True", "nofollow", "yes", "", NA))
sf_parse_follow <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(value))
  out[value %in% c("true", "yes", "1", "follow")] <- TRUE
  out[value %in% c("false", "no", "0", "nofollow")] <- FALSE
  out[is.na(x) | value == ""] <- NA
  out
}

#' Detect `nofollow` in a rel attribute
#'
#' @description Tests whether each value of a link's \code{rel} attribute
#'   contains the \code{nofollow} token. Values are lowercased and split on
#'   commas and whitespace, so \code{"ugc nofollow"} and \code{"nofollow,ugc"}
#'   both count.
#'
#' @param x A vector (typically character) of \code{rel} attribute values.
#'
#' @return A logical vector the same length as \code{x}: \code{TRUE} when the
#'   \code{nofollow} token is present, \code{FALSE} when it is not, and
#'   \code{NA} for blank strings or \code{NA} input.
#'
#' @family Screaming Frog toolkit
#' @export
#' @examples
#' sf_rel_nofollow(c("nofollow", "ugc nofollow", "sponsored", "", NA))
sf_rel_nofollow <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- vapply(strsplit(value, "[,[:space:]]+"), function(tokens) {
    "nofollow" %in% tokens
  }, logical(1))
  out[is.na(x) | value == ""] <- NA
  out
}

#' Normalize a Screaming Frog link position
#'
#' @description Maps Screaming Frog's "Link Position" values onto the compact
#'   vocabulary pagerankr uses for placement-aware weighting: \code{navigation}
#'   becomes \code{"nav"}, while \code{header}, \code{footer}, \code{aside}, and
#'   \code{content} pass through unchanged. Matching is case-insensitive and
#'   whitespace is trimmed. The result is what [pagerank()] consumes through its
#'   \code{placement_col} argument.
#'
#' @param x A vector (typically character) of link positions.
#'
#' @return A character vector the same length as \code{x} containing
#'   \code{"nav"}, \code{"header"}, \code{"footer"}, \code{"aside"}, or
#'   \code{"content"}. Blank strings, \code{NA}, and unrecognized values yield
#'   \code{NA}.
#'
#' @family Screaming Frog toolkit
#' @export
#' @examples
#' sf_normalize_position(c("Navigation", "Aside", "Content", "", NA))
sf_normalize_position <- function(x) {
  value <- tolower(trimws(as.character(x)))
  normalized <- c(
    navigation = "nav",
    header = "header",
    footer = "footer",
    aside = "aside",
    content = "content"
  )
  out <- unname(normalized[value])
  out[is.na(x) | value == ""] <- NA_character_
  out
}

#' Derive a link's page region from its DOM path
#'
#' @description Reads the page region a link sits in out of Screaming Frog's
#'   \code{Link Path} (an XPath-like source locator), returning the same compact
#'   vocabulary as \code{\link{sf_normalize_position}()}. This is the preferred
#'   source of placement, because \code{Link Position} loses the enclosing
#'   region whenever a \code{<nav>} is nested inside one.
#'
#' @details
#' The region is the **outermost** layout container on the path ---
#' \code{header}, \code{footer}, or \code{aside} --- and \code{"nav"} applies
#' only when the link sits in a \code{<nav>} that is not inside one of those.
#' So a footer nav resolves to \code{"footer"}, a header nav to
#' \code{"header"}, and a standalone nav to \code{"nav"}. Anything else is
#' \code{"content"}, which is an acknowledged residual bucket rather than a
#' positive claim about the markup.
#'
#' Why not just read \code{Link Position}? On a site whose footer is marked up
#' as \code{footer > nav > a}, Screaming Frog reports every footer link as
#' \code{Navigation} and emits no \code{Footer} bucket at all, so \code{footer}
#' is not merely mislabeled but unreachable --- a user wanting footer at 0.05
#' and nav at 0.2 has no way to express it. Other sites do emit \code{Footer},
#' so the vocabulary silently varies with the site's markup. The DOM path has
#' the region unambiguously in both cases.
#'
#' Element names are matched on their own: predicates are stripped first, so a
#' \code{div[@class='site-footer']} is \emph{not} read as a footer. Only real
#' \code{<footer>} elements are.
#'
#' @param x A vector (typically character) of Screaming Frog link paths, e.g.
#'   \code{"//body/footer/nav/ul/li[1]/a"}.
#'
#' @return A character vector the same length as \code{x} containing
#'   \code{"nav"}, \code{"header"}, \code{"footer"}, \code{"aside"}, or
#'   \code{"content"}. Blank strings and \code{NA} yield \code{NA}, so a caller
#'   can fall back to \code{\link{sf_normalize_position}()}.
#'
#' @family Screaming Frog toolkit
#' @seealso [pagerank()], whose `placement_col` consumes the result.
#' @export
#' @examples
#' sf_region_from_path(c(
#'   "//body/footer/nav/ul/li[1]/a", # footer nav -> footer, not nav
#'   "//body/header/nav/ul/li[2]/a", # header nav -> header
#'   "//body/nav/ul/li[1]/a", # standalone nav -> nav
#'   "//body/main/article/p[5]/a[1]", # -> content
#'   "//body/div[@class='site-footer']/a" # a class is not an element
#' ))
sf_region_from_path <- function(x) {
  value <- trimws(as.character(x))
  out <- vapply(
    value, .sf_region_from_one_path, character(1),
    USE.NAMES = FALSE
  )
  out[is.na(x) | value == ""] <- NA_character_
  out
}

#' Resolve a single DOM path to a region
#'
#' Split into steps, strip predicates, then apply the precedence rule: the
#' outermost layout container wins, `nav` only counts outside one, and
#' everything else falls through to the `content` residual.
#'
#' @keywords internal
#' @noRd
.sf_region_from_one_path <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  steps <- strsplit(path, "/", fixed = TRUE)[[1]]
  # Drop predicates -- `div[@class='site-footer']` is a div, not a footer.
  steps <- tolower(sub("\\[.*$", "", steps))
  # A link outside <body> sits in no page region at all: Screaming Frog emits
  # `//head/link[...]` rows for stylesheets, canonicals, and hreflang. None are
  # graph-eligible, so this is NA rather than the `content` residual.
  if (!any(steps == "body")) {
    return(NA_character_)
  }
  containers <- c("header", "footer", "aside")
  outermost <- which(steps %in% containers)
  if (length(outermost) > 0L) {
    return(steps[outermost[1L]])
  }
  if (any(steps == "nav")) {
    return("nav")
  }
  "content"
}

#' Test whether a link type is graph-eligible
#'
#' @description Reports which Screaming Frog link types count as edges in the
#'   link graph. Only true hyperlinks build the graph; resource references such
#'   as images, stylesheets, and scripts are excluded. The eligible set is
#'   \code{sf_contract()$graph_eligible_types}.
#'
#' @param type A vector (typically character) of Screaming Frog link types,
#'   e.g. \code{"Hyperlink"} or \code{"Image"}. Whitespace is trimmed.
#'
#' @return A logical vector the same length as \code{type}, \code{TRUE} where
#'   the type is graph-eligible.
#'
#' @family Screaming Frog toolkit
#' @export
#' @examples
#' sf_graph_eligible(c("Hyperlink", "Image", "Stylesheet"))
sf_graph_eligible <- function(type) {
  trimws(as.character(type)) %in% sf_contract()$graph_eligible_types
}

#' Derive a link's container component from its DOM path
#'
#' @description Reduces a Screaming Frog \code{Link Path} to the **component
#'   the link sits in**, stable across every page that component appears on.
#'   This is the identity the boilerplate detector conditions on: see
#'   \code{\link{pagerank}()}'s \code{container_col}.
#'
#' Two steps:
#'
#' \enumerate{
#'   \item **Strip numeric predicates, keep class predicates.** Screaming Frog's
#'     \code{Link Path} is a hybrid, using \code{[@class='...']} where classes
#'     exist and positional \code{[n]} elsewhere. Positions are unstable --- the
#'     same recycled call-to-action lands at \code{p[5]} on a post with four
#'     preceding paragraphs and \code{p[3]} on a shorter one --- while a class
#'     is
#'     exactly the stable component identifier we want.
#'   \item **Drop the trailing \code{<a>} step**, whatever predicate it carries.
#'     The anchor's own class describes the link, not the component containing
#'     it.
#' }
#'
#' Note this cuts the **opposite** way from
#' \code{\link{sf_region_from_path}()}, which strips class predicates so that a
#' \code{div[@class='site-footer']} is not mistaken for a \code{<footer>}. The
#' two answer different questions --- *which region is this* versus *is this the
#' same component* --- and the inconsistency is deliberate.
#'
#' @param x A vector (typically character) of Screaming Frog link paths, e.g.
#'   \code{"//body/main/article/p[5]/a[1]"}.
#'
#' @return A character vector the same length as \code{x} holding the container
#'   path. Blank strings and \code{NA} yield \code{NA}, leaving those rows
#'   unscored by the detector.
#'
#' @family Screaming Frog toolkit
#' @seealso [pagerank()], whose `container_col` consumes the result.
#' @export
#' @examples
#' sf_container_from_path(c(
#'   "//body/main/article/p[5]/a[1]", # positions stripped
#'   "//body/main/article/p[3]/a[1]", # ... so these two agree
#'   "//body/div[@class='cta']/a", # class kept as the component identity
#'   "//body/div[@class='cta']/a[@class='btn']" # anchor's own class dropped
#' ))
sf_container_from_path <- function(x) {
  value <- trimws(as.character(x))
  # Numeric predicates only. `[@class='...']` is the component identifier and
  # must survive; see the note above on why this differs from the region parser.
  skeleton <- gsub("\\[[0-9]+\\]", "", value)
  # The trailing <a> is the link itself, not its container.
  out <- sub("/[aA](\\[[^]]*\\])?$", "", skeleton)
  out[is.na(x) | value == ""] <- NA_character_
  out
}
