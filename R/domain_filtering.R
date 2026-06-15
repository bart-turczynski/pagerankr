#' @title Filter Edge List by Domain or Host
#' @description
#' Filters an edge list by registrable domain and/or host rules. Rows are kept
#' only when both endpoints satisfy the keep/ignore logic. Ignore rules always
#' override keep rules. When keep rules are provided, `drop_third_party = TRUE`
#' removes URLs outside the keep lists.
#'
#' This function is intended as a **pre-processing step** before calling
#' [pagerank()]. For example, to scope a PageRank analysis to a single site
#' or exclude CDN / tracking domains.
#'
#' @param edge_list_df A data frame representing the edge list, with at least
#'   two URL columns.
#' @param from_col Name of the source URL column. Default `"from"`.
#' @param to_col Name of the target URL column. Default `"to"`.
#' @param keep_domains Character vector of registrable domains to keep (e.g.,
#'   `"example.com"`). Subdomains are included when their registrable domain
#'   matches.
#' @param keep_hosts Character vector of specific hosts to keep (e.g.,
#'   `"www.example.com"`). Only exact host matches are kept.
#' @param ignore_domains Character vector of registrable domains to drop.
#' @param ignore_hosts Character vector of specific hosts to drop.
#' @param drop_third_party Logical. When keep lists are provided and this is
#'   `TRUE` (default), URLs outside the keep lists are dropped. When `FALSE`,
#'   only explicitly ignored URLs are dropped.
#' @param return_report Logical. If `TRUE`, returns a list with the filtered
#'   data frame and a filter report. Default `FALSE`.
#'
#' @return If `return_report = FALSE` (default), the filtered data frame
#'   (preserving all columns). If `TRUE`, a list with elements
#'   `filtered_df` and `report`.
#' @export
#' @importFrom rurl get_host get_domain
#' @examples
#' links <- data.frame(
#'   from = c(
#'     "http://www.example.com/a", "http://example.com/b",
#'     "http://cdn.tracker.com/c"
#'   ),
#'   to = c(
#'     "http://example.com/b", "http://help.example.com/d",
#'     "http://www.example.com/a"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Keep only example.com edges
#' filter_links_by_domain(links, keep_domains = "example.com")
#'
#' # Ignore a specific subdomain
#' filter_links_by_domain(links, ignore_hosts = "cdn.tracker.com")
#'
#' # Get a report of what was filtered
#' result <- filter_links_by_domain(links,
#'   keep_domains = "example.com",
#'   return_report = TRUE
#' )
#' result$report
filter_links_by_domain <- function(edge_list_df,
                                   from_col = "from",
                                   to_col = "to",
                                   keep_domains = NULL,
                                   keep_hosts = NULL,
                                   ignore_domains = NULL,
                                   ignore_hosts = NULL,
                                   drop_third_party = TRUE,
                                   return_report = FALSE) {
  # --- Validation ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0 &&
        !all(c(from_col, to_col) %in% names(edge_list_df))) {
    stop(
      "`edge_list_df` must have '", from_col, "' and '", to_col, "' columns.",
      call. = FALSE
    )
  }
  if (!is.null(keep_domains) && !is.character(keep_domains)) {
    stop("`keep_domains` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.null(keep_hosts) && !is.character(keep_hosts)) {
    stop("`keep_hosts` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.null(ignore_domains) && !is.character(ignore_domains)) {
    stop("`ignore_domains` must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.null(ignore_hosts) && !is.character(ignore_hosts)) {
    stop("`ignore_hosts` must be a character vector or NULL.", call. = FALSE)
  }

  # --- Early return if empty ---
  if (nrow(edge_list_df) == 0) {
    if (return_report) {
      return(list(
        filtered_df = edge_list_df,
        report = list(rows_before = 0L, rows_after = 0L, rows_dropped = 0L)
      ))
    }
    return(edge_list_df)
  }

  # --- Build filter lists ---
  keep_domains_resolved <- .resolve_domains(keep_domains)
  keep_hosts_resolved <- .resolve_hosts(keep_hosts)
  ignore_domains_resolved <- .resolve_domains(ignore_domains)
  ignore_hosts_resolved <- .resolve_hosts(ignore_hosts)

  has_keep_list <- length(keep_domains_resolved) > 0 ||
    length(keep_hosts_resolved) > 0
  has_ignore_list <- length(ignore_domains_resolved) > 0 ||
    length(ignore_hosts_resolved) > 0

  # No filters active -- return as-is
  if (!has_keep_list && !has_ignore_list) {
    if (return_report) {
      n <- nrow(edge_list_df)
      return(list(
        filtered_df = edge_list_df,
        report = list(rows_before = n, rows_after = n, rows_dropped = 0L)
      ))
    }
    return(edge_list_df)
  }

  # --- Extract host/domain for all unique URLs ---
  from_urls <- as.character(edge_list_df[[from_col]])
  to_urls <- as.character(edge_list_df[[to_col]])
  url_maps <- .build_url_maps(c(from_urls, to_urls))

  # --- Classify each endpoint ---
  keep_from <- .classify_url_vector(
    from_urls, url_maps,
    keep_domains_resolved, keep_hosts_resolved,
    ignore_domains_resolved, ignore_hosts_resolved,
    has_keep_list, drop_third_party
  )
  keep_to <- .classify_url_vector(
    to_urls, url_maps,
    keep_domains_resolved, keep_hosts_resolved,
    ignore_domains_resolved, ignore_hosts_resolved,
    has_keep_list, drop_third_party
  )

  # Both endpoints must pass
  keep_mask <- keep_from & keep_to
  filtered_df <- edge_list_df[keep_mask, , drop = FALSE]
  row.names(filtered_df) <- NULL

  if (return_report) {
    return(list(
      filtered_df = filtered_df,
      report = list(
        rows_before = nrow(edge_list_df),
        rows_after = nrow(filtered_df),
        rows_dropped = nrow(edge_list_df) - nrow(filtered_df),
        keep_domains = keep_domains_resolved,
        keep_hosts = keep_hosts_resolved,
        ignore_domains = ignore_domains_resolved,
        ignore_hosts = ignore_hosts_resolved
      )
    ))
  }
  filtered_df
}


# --- Internal helpers ---

#' Ensure URLs have a scheme (needed for rurl::get_host / get_domain)
#' @noRd
.ensure_scheme <- function(urls) {
  urls <- as.character(urls)
  needs_scheme <- !grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", urls) &
    !grepl("^//", urls) &
    !is.na(urls)
  urls[needs_scheme] <- paste0("http://", urls[needs_scheme])
  # Handle scheme-relative //
  scheme_relative <- grepl("^//", urls) & !is.na(urls)
  urls[scheme_relative] <- paste0("http:", urls[scheme_relative])
  urls
}

#' Resolve domain strings (clean + extract registrable domain)
#' @noRd
.resolve_domains <- function(domains) {
  if (is.null(domains) || length(domains) == 0) {
    return(character(0))
  }
  domains <- trimws(as.character(domains))
  domains <- domains[!is.na(domains) & nzchar(domains)]
  if (length(domains) == 0) {
    return(character(0))
  }
  with_scheme <- .ensure_scheme(domains)
  extracted <- rurl::get_domain(with_scheme)
  extracted <- tolower(extracted)
  extracted <- extracted[!is.na(extracted) & nzchar(extracted)]
  sort(unique(extracted))
}

#' Resolve host strings (clean + extract host)
#' @noRd
.resolve_hosts <- function(hosts) {
  if (is.null(hosts) || length(hosts) == 0) {
    return(character(0))
  }
  hosts <- trimws(as.character(hosts))
  hosts <- hosts[!is.na(hosts) & nzchar(hosts)]
  if (length(hosts) == 0) {
    return(character(0))
  }
  with_scheme <- .ensure_scheme(hosts)
  extracted <- rurl::get_host(with_scheme)
  extracted <- tolower(extracted)
  extracted <- extracted[!is.na(extracted) & nzchar(extracted)]
  sort(unique(extracted))
}

#' Build named host/domain lookup maps for a vector of URLs
#' @noRd
.build_url_maps <- function(urls) {
  urls <- as.character(urls)
  urls <- urls[!is.na(urls) & nzchar(urls)]
  unique_urls <- unique(urls)
  if (length(unique_urls) == 0) {
    return(list(host_map = character(0), domain_map = character(0)))
  }
  with_scheme <- .ensure_scheme(unique_urls)
  hosts <- tolower(rurl::get_host(with_scheme))
  domains <- tolower(rurl::get_domain(with_scheme))
  list(
    host_map = stats::setNames(hosts, unique_urls),
    domain_map = stats::setNames(domains, unique_urls)
  )
}

#' Classify a vector of URLs as keep (TRUE) or drop (FALSE)
#' @noRd
.classify_url_vector <- function(urls, url_maps,
                                 keep_domains, keep_hosts,
                                 ignore_domains, ignore_hosts,
                                 has_keep_list, drop_third_party) {
  host <- url_maps$host_map[urls]
  domain <- url_maps$domain_map[urls]

  is_ignored <- (!is.na(host) & host %in% ignore_hosts) |
    (!is.na(domain) & domain %in% ignore_domains)

  if (!has_keep_list) {
    # No keep list: keep everything except ignored
    return(!is_ignored)
  }

  is_kept <- (!is.na(host) & host %in% keep_hosts) |
    (!is.na(domain) & domain %in% keep_domains)

  # Ignore always overrides keep
  ifelse(is_ignored, FALSE, ifelse(is_kept, TRUE, !drop_third_party))
}
