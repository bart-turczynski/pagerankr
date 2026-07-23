#' @title Simulate PageRank Changes on a Screaming Frog Bundle
#' @description The Screaming Frog entry point for [simulate_changes()]. A thin
#'   wrapper that mirrors [pagerank_screaming_frog()]: it reuses the same
#'   bundle -> [pagerank()] adapter to build the baseline inputs (edges,
#'   redirects, canonicals, indexability, placement, nofollow, ...), applies the
#'   same URL- and edge-level verbs, and delegates to the shared changeset
#'   engine. There is no Screaming Frog-specific simulation logic; both entry
#'   points funnel into one engine, so CSV and bundle users get identical
#'   what-if capability. The what-if's modeled redirects compose on top of the
#'   bundle's real crawled redirects (a changeset redirect for a source wins).
#'
#' @param bundle A `screaming_frog_bundle` object.
#' @param add_links_df,remove_links_df,redirect_urls_df,remove_urls The
#'   changeset verbs. See [simulate_changes()] for their semantics. Link and
#'   redirect endpoints (and `remove_urls`) match the bundle's raw crawled URLs
#'   (the `from`/`to` node identities before any folding), using `from`/`to`
#'   columns. A modeled `remove_urls` 404 composes on top of the bundle's real
#'   crawled status.
#' @param on_unknown_target How to treat a redirect or link target absent from
#'   the current graph. See [simulate_changes()].
#' @param accepted_placements,link_origins,placement_weights,weight_col Bundle
#'   scoring controls forwarded to the shared adapter, identical to
#'   [pagerank_screaming_frog()].
#' @param apply_canonicals,apply_redirects,preset Fold and view controls
#'   forwarded to the shared adapter, identical to [pagerank_screaming_frog()].
#'   All of these shape the baseline and the proposed model equally, so the two
#'   differ only by the changeset.
#' @param ... Additional scoring controls passed through to both [pagerank()]
#'   calls (e.g. `self_loops`, `nofollow_action`, `damping`, `position_col`).
#' @param label_baseline,label_proposed Labels for the two models in the
#'   comparison output. Defaults `"baseline"` / `"proposed"`.
#'
#' @return The [simulate_changes()] output: a [compare_pagerank()] table with a
#'   `node_status` column, plus `summary`, `proposed`, and `manifest`
#'   attributes. See [simulate_changes()].
#'
#' @seealso [simulate_changes()] for the bare edge-list entry point and full
#'   verb semantics; [pagerank_screaming_frog()] for scoring a bundle without a
#'   what-if.
#' @export
#' @examples
#' internal <- data.frame(
#'   Address = c(
#'     "https://example.com/", "https://example.com/a", "https://example.com/b"
#'   ),
#'   `Status Code` = c("200", "200", "200"),
#'   check.names = FALSE
#' )
#' links <- data.frame(
#'   Type = c("Hyperlink", "Hyperlink"),
#'   Source = c("https://example.com/", "https://example.com/a"),
#'   Destination = c("https://example.com/a", "https://example.com/b"),
#'   Follow = c("TRUE", "TRUE"),
#'   check.names = FALSE
#' )
#' bundle <- screaming_frog_bundle(internal, links, "all_outlinks")
#'
#' # Retire /a behind a redirect to /b
#' retire <- data.frame(
#'   from = "https://example.com/a", to = "https://example.com/b"
#' )
#' simulate_changes_screaming_frog(bundle, redirect_urls_df = retire)
#'
#' # Model /a 404-ing (its inbound authority evaporates to the waste sink)
#' simulate_changes_screaming_frog(
#'   bundle,
#'   remove_urls = "https://example.com/a"
#' )
simulate_changes_screaming_frog <- function(bundle,
                                            add_links_df = NULL,
                                            remove_links_df = NULL,
                                            redirect_urls_df = NULL,
                                            remove_urls = NULL,
                                            on_unknown_target = c(
                                              "warn", "error", "allow"
                                            ),
                                            accepted_placements = NULL,
                                            link_origins = NULL,
                                            placement_weights = NULL,
                                            weight_col = NULL,
                                            apply_canonicals = TRUE,
                                            apply_redirects = TRUE,
                                            preset = NULL,
                                            ...,
                                            label_baseline = "baseline",
                                            label_proposed = "proposed") {
  on_unknown_target <- match.arg(on_unknown_target)

  prep <- .sf_prepare_pr_args(
    bundle, accepted_placements, link_origins, placement_weights, weight_col,
    apply_canonicals, apply_redirects, preset, list(...),
    missing(apply_canonicals), missing(apply_redirects)
  )

  .simulate_changes_engine(
    baseline_args = prep$pr_args,
    add_links_df = add_links_df,
    remove_links_df = remove_links_df,
    redirect_urls_df = redirect_urls_df,
    remove_urls = remove_urls,
    on_unknown_target = on_unknown_target,
    edge_from_col = "from",
    edge_to_col = "to",
    redirect_from_col = "from",
    redirect_to_col = "to",
    label_baseline = label_baseline,
    label_proposed = label_proposed
  )
}
