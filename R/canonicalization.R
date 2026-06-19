#' @title pagerankr URL Canonicalization Profile
#' @description The single, explicit set of `rurl` canonicalization arguments
#'   that define a pagerankr node identity. Every knob `rurl::get_clean_url()`
#'   (and `rurl::safe_parse_url()`) accepts is pinned here with an explicit
#'   value, so node keys never depend on `rurl`'s own defaults -- which have
#'   changed across `rurl` versions (e.g. `case_handling` flipped from `"keep"`
#'   to `"lower_host"`) and previously desynced the pagerankr <-> semantic join.
#'
#' @details The canonical node key is **scheme + host + path** (port, query,
#'   fragment and userinfo are dropped by `get_clean_url`). These ten arguments
#'   fix exactly how that key is derived. The values below intentionally mirror
#'   `rurl`'s current defaults, so pinning them is behavior-preserving today
#'   and purely guards against future default drift.
#'
#'   The same ten arguments are accepted by both `rurl::get_clean_url()` (the
#'   cleaning path) and `rurl::safe_parse_url()` (the domain-filtering path), so
#'   one profile drives both and the two paths stay symmetrical.
#'
#'   The cross-repo contract requires **semantic** to pin the identical profile;
#'   change both repos together.
#'
#' @return A named list of `rurl` canonicalization arguments.
#' @export
canonical_profile <- function() {
  list(
    protocol_handling = "keep",
    case_handling = "lower_host",
    www_handling = "none",
    trailing_slash_handling = "none",
    index_page_handling = "keep",
    path_normalization = "none",
    scheme_relative_handling = "keep",
    subdomain_levels_to_keep = NULL,
    host_encoding = "keep",
    path_encoding = "keep"
  )
}

#' @title Merge User rurl Parameters Over the Canonical Profile
#' @description Returns the canonical profile with any user-supplied `rurl`
#'   parameters overriding individual keys. This is the single place pagerankr
#'   resolves the effective canonicalization arguments for both cleaning and
#'   filtering, so the two paths cannot drift apart.
#'
#' @param user_params A named list of `rurl` arguments to override the profile.
#'   Unknown keys are passed through (so new `rurl` arguments work without a
#'   code change here); `NULL` or empty yields the bare profile.
#' @return A named list of effective `rurl` canonicalization arguments.
#' @noRd
.resolve_rurl_params <- function(user_params = list()) {
  if (is.null(user_params) || length(user_params) == 0) {
    return(canonical_profile())
  }
  if (!is.list(user_params)) {
    stop("`rurl_params` must be a list.", call. = FALSE)
  }
  utils::modifyList(canonical_profile(), user_params)
}
