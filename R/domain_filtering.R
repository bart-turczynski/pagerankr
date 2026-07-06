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
#' **Ordering relative to folding:** when `pagerank()` calls this filter
#' internally (via its `keep_domains` / `exclude_domains` / `keep_hosts` /
#' `exclude_hosts` arguments), the filter runs *after* redirect and canonical
#' folding, so it scopes the post-fold (canonical) namespace. If an out-of-scope
#' canonical/redirect rewrites the crawled domain/host onto a different one,
#' filtering on the crawled value inside `pagerank()` matches nothing. To
#' domain-scope the **crawled input** instead, call `filter_links_by_domain()`
#' on the edge list yourself *before* folding (i.e. before passing it to
#' `pagerank()`).
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
#' @param psl_section Public Suffix List section used to derive registrable
#'   domains, passed to `rurl::get_domain()` / `rurl::safe_parse_urls()`. One of
#'   `"all"` (default, ICANN + private suffixes), `"icann"`, or `"private"`.
#'   Affects domain-based (not host-based) keep/ignore matching; e.g. under
#'   `"icann"`, `user.github.io` has registrable domain `github.io`, while under
#'   `"all"` it is `user.github.io`.
#' @param rurl_params A list of `rurl` canonicalization arguments overriding
#'   pagerankr's profile per key, used when extracting hosts/domains from both
#'   the edge URLs and the keep/ignore values. Pass the **same** profile used to
#'   clean the graph so the comparison keys are derived identically. The
#'   host-relevant knobs are `host_encoding` (`"keep"`/`"idna"`/`"unicode"` —
#'   IDN folding; e.g. `"idna"` makes `münchen.de` and `xn--mnchen-3ya.de`
#'   match), `www_handling`, `subdomain_levels_to_keep`, `case_handling`, and
#'   `protocol_handling`. Registrable-domain matching is encoding-independent.
#'   When called from [pagerank()], this is forwarded automatically.
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
#'   )
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
                                   return_report = FALSE,
                                   psl_section = c("all", "icann", "private"),
                                   rurl_params = list()) {
  psl_section <- match.arg(psl_section)
  # Resolve the canonicalization profile (user rurl_params override per key) so
  # host/domain extraction here matches how node URLs were (or will be) cleaned.
  rurl_profile <- .resolve_rurl_params(rurl_params)
  # --- Validation ---
  .validate_filter_inputs(
    edge_list_df, from_col, to_col,
    keep_domains, keep_hosts, ignore_domains, ignore_hosts
  )

  # --- Early return if empty ---
  if (nrow(edge_list_df) == 0) {
    return(.empty_filter_result(edge_list_df, return_report, 0L))
  }

  # --- Build filter lists ---
  keep_domains_resolved <- .resolve_domains(
    keep_domains, psl_section, rurl_profile
  )
  keep_hosts_resolved <- .resolve_hosts(keep_hosts, rurl_profile)
  ignore_domains_resolved <- .resolve_domains(
    ignore_domains, psl_section, rurl_profile
  )
  ignore_hosts_resolved <- .resolve_hosts(ignore_hosts, rurl_profile)

  has_keep_list <- length(keep_domains_resolved) > 0 ||
    length(keep_hosts_resolved) > 0
  has_ignore_list <- length(ignore_domains_resolved) > 0 ||
    length(ignore_hosts_resolved) > 0

  # No filters active -- return as-is
  if (!has_keep_list && !has_ignore_list) {
    return(.empty_filter_result(
      edge_list_df, return_report, nrow(edge_list_df)
    ))
  }

  # --- Extract host/domain for all unique URLs ---
  from_urls <- as.character(edge_list_df[[from_col]])
  to_urls <- as.character(edge_list_df[[to_col]])
  url_maps <- .build_url_maps(c(from_urls, to_urls), psl_section, rurl_profile)

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
    return(.build_filter_report(
      edge_list_df, filtered_df,
      keep_domains_resolved, keep_hosts_resolved,
      ignore_domains_resolved, ignore_hosts_resolved
    ))
  }
  filtered_df
}


# --- Validation / result helpers ---

#' Error unless `x` is NULL or a character vector
#' @noRd
.check_char_or_null <- function(x, name) {
  if (is.null(x)) {
    return(invisible())
  }
  if (!is.character(x)) {
    stop("`", name, "` must be a character vector or NULL.", call. = FALSE)
  }
  invisible()
}

#' Validate the inputs to [filter_links_by_domain()]
#'
#' Preserves the original validation order and error-message text verbatim.
#' @noRd
.validate_filter_inputs <- function(edge_list_df, from_col, to_col,
                                    keep_domains, keep_hosts,
                                    ignore_domains, ignore_hosts) {
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
  .check_char_or_null(keep_domains, "keep_domains")
  .check_char_or_null(keep_hosts, "keep_hosts")
  .check_char_or_null(ignore_domains, "ignore_domains")
  .check_char_or_null(ignore_hosts, "ignore_hosts")
  invisible()
}

#' Build the pass-through result for empty / no-filter cases
#'
#' Returns `edge_list_df` unchanged, or (when `return_report`) a list carrying
#' the unfiltered frame plus a minimal row-count report.
#' @noRd
.empty_filter_result <- function(edge_list_df, return_report, n) {
  if (return_report) {
    return(list(
      filtered_df = edge_list_df,
      report = list(rows_before = n, rows_after = n, rows_dropped = 0L)
    ))
  }
  edge_list_df
}

#' Assemble the full filter report list
#' @noRd
.build_filter_report <- function(edge_list_df, filtered_df,
                                 keep_domains, keep_hosts,
                                 ignore_domains, ignore_hosts) {
  list(
    filtered_df = filtered_df,
    report = list(
      rows_before = nrow(edge_list_df),
      rows_after = nrow(filtered_df),
      rows_dropped = nrow(edge_list_df) - nrow(filtered_df),
      keep_domains = keep_domains,
      keep_hosts = keep_hosts,
      ignore_domains = ignore_domains,
      ignore_hosts = ignore_hosts
    )
  )
}

# --- Internal helpers ---
#
# All extraction routes through `rurl::safe_parse_urls()` under the resolved
# canonicalization profile (`rurl_profile`), the SAME knobs that clean the node
# URLs, so filter keys and node keys are derived identically. The profile's
# `protocol_handling = "keep"` adds a scheme to scheme-less input (and handles
# scheme-relative `//`) and `case_handling = "lower_host"` lowercases the host,
# so no local scheme-prepending or lower-casing is needed. `tld_source`
# (= `psl_section`) selects the PSL section for the registrable domain.

#' Parse a vector of strings for filtering under the canonicalization profile
#' @noRd
.parse_for_filter <- function(urls, psl_section, rurl_profile) {
  do.call(
    rurl::safe_parse_urls,
    c(list(urls), rurl_profile, list(tld_source = psl_section))
  )
}

#' Resolve domain strings (extract registrable domain)
#'
#' The registrable domain is an identity, not a rendering, so its comparison key
#' must be encoding-independent (see `pagerank()`'s "Registrable-domain matching
#' is encoding-independent" note). We read `rurl`'s `domain_ascii` column (the
#' registrable domain in canonical Punycode form regardless of the profile's
#' `host_encoding`, `rurl` >= 2.1.0), so the Unicode (`münchen.de`) and Punycode
#' (`xn--mnchen-3ya.de`) spellings of one domain fold to a single key. This
#' needs no separate IDNA-forced parse -- the base profile's parse already
#' carries both spellings.
#' @noRd
.resolve_domains <- function(domains, psl_section, rurl_profile) {
  if (is.null(domains) || length(domains) == 0) {
    return(character(0))
  }
  domains <- trimws(as.character(domains))
  domains <- domains[!is.na(domains) & nzchar(domains)]
  if (length(domains) == 0) {
    return(character(0))
  }
  parsed <- .parse_for_filter(domains, psl_section, rurl_profile)
  extracted <- parsed$domain_ascii
  extracted <- extracted[!is.na(extracted) & nzchar(extracted)]
  sort(unique(extracted))
}

#' Resolve host strings (extract host)
#'
#' Reads the `host` column so the profile's host knobs (`host_encoding`,
#' `www_handling`, subdomain levels, case) govern the comparison form the same
#' way they do for the node URLs.
#' @noRd
.resolve_hosts <- function(hosts, rurl_profile) {
  if (is.null(hosts) || length(hosts) == 0) {
    return(character(0))
  }
  hosts <- trimws(as.character(hosts))
  hosts <- hosts[!is.na(hosts) & nzchar(hosts)]
  if (length(hosts) == 0) {
    return(character(0))
  }
  extracted <- .parse_for_filter(hosts, "all", rurl_profile)$host
  extracted <- extracted[!is.na(extracted) & nzchar(extracted)]
  sort(unique(extracted))
}

#' Build named host/domain lookup maps for a vector of URLs
#'
#' Parses each unique URL ONCE via `rurl::safe_parse_urls()` and reads the
#' `host` and `domain_ascii` columns under the resolved canonicalization
#' profile. Host uses the profile's encoding; the registrable domain uses
#' `domain_ascii` (canonical Punycode, independent of the profile's
#' `host_encoding`, `rurl` >= 2.1.0) so its comparison key is
#' encoding-independent -- no second IDNA-forced parse needed.
#' @noRd
.build_url_maps <- function(urls, psl_section, rurl_profile) {
  urls <- as.character(urls)
  urls <- urls[!is.na(urls) & nzchar(urls)]
  unique_urls <- unique(urls)
  if (length(unique_urls) == 0) {
    return(list(host_map = character(0), domain_map = character(0)))
  }
  parsed <- .parse_for_filter(unique_urls, psl_section, rurl_profile)
  list(
    host_map = stats::setNames(parsed$host, unique_urls),
    domain_map = stats::setNames(parsed$domain_ascii, unique_urls)
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
  !is_ignored & (is_kept | !drop_third_party)
}
