#' @title pagerankr URL Canonicalization Profile
#' @description The explicit set of `rurl` canonicalization arguments that
#'   determine a pagerankr node identity (scheme + host + path). Every knob that
#'   *shapes that key* is pinned here with an explicit value, so node keys never
#'   depend on `rurl`'s own defaults -- which have changed across `rurl`
#'   versions (e.g. `case_handling` flipped from `"keep"` to `"lower_host"`) and
#'   previously desynced the pagerankr <-> semantic join. Knobs that only govern
#'   components the key drops (port, query, fragment, userinfo) are deliberately
#'   left unpinned -- see the "Knobs deliberately left unpinned" note in
#'   @details.
#'
#' @details The canonical node key is **scheme + host + path**, with the path
#'   percent-decoded and RFC 3986 dot-segments removed (port, query, fragment
#'   and userinfo are dropped by `get_clean_url`). These ten arguments fix
#'   exactly how that key is derived.
#'
#'   The pinned values are chosen to reproduce that committed canonical key, not
#'   to match `rurl`'s defaults. As of `rurl` 2.1.0 they intentionally
#'   **override** two defaults: `path_normalization = "dot_segments"` (default
#'   `"none"`) and `path_encoding = "decode"` (default `"keep"`). `rurl` 2.1.0
#'   redefined its `"none"`/`"keep"` path defaults to keep the path verbatim,
#'   which silently changed the key and desynced the pagerankr <-> semantic
#'   join for paths containing dot-segments or percent-encoding; pinning the
#'   explicit values above restores the original keys. The remaining knobs still
#'   equal `rurl`'s current defaults. (The anti-drift guarantee ultimately lives
#'   in semantic's byte-parity oracle test, which caught this; the pin is a
#'   convenience that must be re-pinned when a dependency redefines a value.)
#'
#'   The same ten arguments are accepted by both `rurl::get_clean_url()` (the
#'   cleaning path) and `rurl::safe_parse_url()` (the domain-filtering path), so
#'   one profile drives both and the two paths stay symmetrical.
#'
#'   **Knobs deliberately left unpinned.** `rurl::get_clean_url()` has grown
#'   options that govern components the node key does not carry:
#'   `query_handling` (default `"drop"`) and its parameter sub-options,
#'   `port_handling` (default `"exclude"`), and the `url_standard` selector
#'   (default `NULL` -- no standard profile, added in `rurl` 2.2.0). Under the
#'   scheme+host+path key these have no visible effect at their defaults, so
#'   pinning them would add noise without changing identity. They are
#'   intentionally **not** part of this profile;
#'   instead the behavioral guard in `test-canonicalization.R` asserts that a
#'   canonical key really does drop the port, query, and fragment, so a future
#'   `rurl` default flip on one of these unpinned knobs is caught on the
#'   pagerankr side rather than silently changing node identity.
#'
#'   The cross-repo contract requires **semantic** to pin the identical profile;
#'   change both repos together.
#'
#'   **Accepted divergence on un-canonicalizable input.** For a value `rurl`
#'   cannot parse (an unsupported scheme like `mailto:`/`tel:`, whitespace, a
#'   dotless bare token), `rurl` returns `NA`. pagerankr's [clean_url_columns()]
#'   keeps such a value as its raw self so it survives as an opaque graph node
#'   (see that function; PR #50), whereas semantic's `canonical_url()` returns
#'   `None` and drops it (FR-05 rurl byte-parity). This is intentional and does
#'   **not** break the `node_score` <-> `page` join: valid URLs still produce
#'   byte-identical keys on both sides (the actual contract), and in the
#'   semantic -> pagerankr bridge semantic canonicalizes and drops
#'   un-canonicalizable inputs *before* pagerankr sees the edges, so the raw
#'   fallback never fires on that path. It only affects pagerankr run standalone
#'   on raw crawl data, where such tokens become opaque nodes instead of being
#'   dropped.
#'
#' @return A named list of `rurl` canonicalization arguments.
#' @export
#' @examples
#' # The pinned profile that determines pagerankr node identity.
#' profile <- canonical_profile()
#' str(profile)
#'
#' # Key knobs that shape the scheme + host + path node key.
#' profile$case_handling      # "lower_host"
#' profile$path_normalization # "dot_segments"
#' profile$path_encoding      # "decode"
canonical_profile <- function() {
  list(
    protocol_handling = "keep",
    case_handling = "lower_host",
    www_handling = "none",
    trailing_slash_handling = "none",
    index_page_handling = "keep",
    # rurl 2.1.0 redefined "none"/"keep" to keep the path verbatim; pin the
    # explicit values that reproduce the committed key (decode + dot-segment
    # removal) so node identities stay stable across the rurl upgrade and in
    # parity with semantic. See @details.
    path_normalization = "dot_segments",
    scheme_relative_handling = "keep",
    subdomain_levels_to_keep = NULL,
    host_encoding = "keep",
    path_encoding = "decode"
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
