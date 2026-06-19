#' @title Topic Feeder PageRank (seeded reverse-graph PageRank)
#' @description The reverse-graph sibling of [topic_sensitive_pagerank()].
#'   Where Topic-Sensitive PageRank answers *"given I care about this cluster,
#'   which pages are most **authoritative** for it?"* (authority flows
#'   downstream **from** the seed cluster), `topic_feeder_pagerank()` answers
#'   the inverse
#'   question: *"which pages **feed / power** this cluster?"* — i.e. the
#'   strongest internal hubs whose outlinks point **into** the target pages.
#'
#'   It is personalized PageRank with the teleport prior concentrated on the
#'   **target cluster**, run on the **transposed** link graph
#'   (`reverse = TRUE`). Mass teleports onto the cluster and then walks
#'   *backward* along links, so it accumulates on the pages that funnel
#'   authority toward the cluster. The further (in out-link hops) a page is from
#'   the cluster, the less feeder credit it earns — the PageRank damping factor
#'   is exactly that attenuation.
#'
#'   Like [trustrank()] and [topic_sensitive_pagerank()], this introduces **no
#'   new solver**: it builds a `prior_df` from the seed set
#'   ([feeder_seed_prior()]) and hands it to [pagerank()] with `reverse = TRUE`.
#'   The caller supplies the
#'   cluster; there is no topic inference.
#'
#' @details
#' ## Why this is not recoverable from forward PageRank
#'
#' In PageRank, authority flows **along** link direction: linking *to* an
#' important page does not make the linker important. So "the pages that feed
#' cluster X" is **not** a re-reading of forward (or Topic-Sensitive) PageRank
#' scores — it is the reversed-graph notion. Forward PageRank and
#' [topic_sensitive_pagerank()] rank pages by **inflow** (important because
#' important pages point at them); `topic_feeder_pagerank()` ranks by **outflow
#' toward the cluster** (important because *it* points at the cluster).
#'
#' ## How to read the result
#'
#' The seed (cluster) pages carry the teleport mass directly, so they appear in
#' `prior_weight` with a positive value and tend to score highly *by
#' construction* — that is teleport, not a feeder signal. **The feeders are
#' the high-`pagerank` pages whose `prior_weight` is `0`** (pages outside the
#' cluster that nonetheless accumulate reverse-walk mass). Rank by `pagerank`
#' and read
#' off the top non-seed rows, or filter `prior_weight == 0`.
#'
#' ## Relationship to neighbouring tools
#' \describe{
#'   \item{[pagerank()] (forward)}{Global inflow authority. Feeder PageRank is
#'     its transpose, biased to a cluster.}
#'   \item{[topic_sensitive_pagerank()] (G2)}{The forward-graph sibling: same
#'     personalization plumbing, opposite flow direction. G2 finds a cluster's
#'     *authorities*; this finds its *feeders*. They are complementary, not
#'     substitutes.}
#'   \item{Inverse PageRank (`pagerank(reverse = TRUE)`)}{The **global**,
#'     unseeded outflow centrality — "which pages funnel authority outward
#'     anywhere on the site". Feeder PageRank adds the cluster bias: not "good
#'     hub in general" but "good hub *for this cluster*". With no `seeds` you
#'     would just call `pagerank(reverse = TRUE)` directly.}
#'   \item{HITS hubs ([hits()])}{Also an outflow notion, but a co-computed
#'     eigenvector pair (hub <-> authority) with no teleport prior and no
#'     damped-surfer / dangling handling. Feeder PageRank is the
#'     random-surfer-model, cluster-seedable counterpart that stays inside the
#'     [pagerank()] graph-preparation contract (redirects, canonicals,
#'     duplicate-edge policy, weights).}
#' }
#'
#' Seed weights are an **additive feeder budget**: if two seed URLs fold onto
#' the same vertex (redirect / canonical variants) their weights sum, exactly as
#' the
#' [pagerank()] / [align_prior_to_vertices()] prior contract specifies.
#'
#' Everything [pagerank()] accepts flows through `...`: redirects, canonicals,
#' URL cleaning, domain/host filtering, edge weights, duplicate-edge policy, and
#' the prior-shaping knobs (`prior_transform`, `prior_alpha`). Because this owns
#' both the prior and the graph orientation, passing `prior_df`,
#' `prior_url_col`, `prior_weight_col`, or `reverse` is an error. Note that the
#' direction-sensitive forward-flow devices that [pagerank()] already rejects
#' under `reverse = TRUE` (`nofollow_action = "evaporate"`, `indexability_df`)
#' are likewise unavailable here.
#'
#' @param edge_list_df A data frame edge list, as passed to [pagerank()].
#' @param seeds The target cluster. Either a character vector of cluster URLs
#'   (each gets equal seed weight unless `seed_weight` is given), or a data
#'   frame with a URL column and a numeric weight column (see `seed_url_col` /
#'   `seed_weight_col`) for unequal cluster emphasis.
#' @param seed_weight Optional numeric weight for a character-vector `seeds`:
#'   either one value per seed or a single value recycled to all seeds. Ignored
#'   when `seeds` is a data frame. Default `NULL` (every cluster page weight
#'   `1`, a uniform distribution over the cluster).
#' @param seed_url_col,seed_weight_col Column names used when `seeds` is a data
#'   frame. Defaults `"url"` / `"weight"`. Ignored for a character vector.
#' @param ... Additional arguments forwarded to [pagerank()] (e.g.
#'   `redirects_df`, `canonicals_df`, `rurl_params`, `weight_col`,
#'   `prior_transform`, `prior_alpha`, `damping`). Passing `prior_df`,
#'   `prior_url_col`, `prior_weight_col`, or `reverse` is an error.
#'
#' @return [feeder_seed_prior()] returns a data frame with `url` and `weight`
#'   columns, suitable as the `prior_df` argument to [pagerank()].
#'
#'   [topic_feeder_pagerank()] returns the [pagerank()] result data frame
#'   (`node_name`, `pagerank`, and the `prior_weight` column the prior path
#'   adds), sorted by `pagerank` descending, carrying the usual
#'   `"transition_audit"` attribute. The audit's model configuration records
#'   `reverse = TRUE`.
#'
#' @seealso [topic_sensitive_pagerank()], [trustrank()], [pagerank()],
#'   [align_prior_to_vertices()], [hits()]
#' @name topic_feeder_pagerank
#' @examples
#' edges <- data.frame(
#'   from = c("/hub", "/hub", "/feeder", "/blog", "/ai", "/news"),
#'   to = c("/ai", "/ai-demo", "/ai", "/ai", "/ai-demo", "/sports"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Which pages feed the AI-Agent cluster?
#' fr <- topic_feeder_pagerank(
#'   edges,
#'   seeds = c("/ai", "/ai-demo"),
#'   clean_edge_urls = FALSE
#' )
#'
#' # Feeders are the top-scoring rows OUTSIDE the cluster (prior_weight == 0).
#' fr[fr$prior_weight == 0, c("node_name", "pagerank")]
#'
#' # Build the cluster prior explicitly and run it yourself, if you prefer:
#' prior <- feeder_seed_prior(c("/ai", "/ai-demo"))
#' identical_run <- pagerank(
#'   edges, prior_df = prior, reverse = TRUE, clean_edge_urls = FALSE
#' )
NULL

#' @rdname topic_feeder_pagerank
#' @export
feeder_seed_prior <- function(seeds,
                              seed_weight = NULL,
                              seed_url_col = "url",
                              seed_weight_col = "weight") {
  if (is.data.frame(seeds)) {
    if (!all(c(seed_url_col, seed_weight_col) %in% names(seeds))) {
      stop("`seeds` (a data frame) must have '", seed_url_col,
        "' and '", seed_weight_col, "' columns.",
        call. = FALSE
      )
    }
    if (!is.null(seed_weight)) {
      stop("`seed_weight` applies only when `seeds` is a character vector; ",
        "put per-seed weights in the '", seed_weight_col, "' column instead.",
        call. = FALSE
      )
    }
    urls <- as.character(seeds[[seed_url_col]])
    wts <- suppressWarnings(as.numeric(seeds[[seed_weight_col]]))
  } else if (is.character(seeds) || is.factor(seeds)) {
    urls <- as.character(seeds)
    if (is.null(seed_weight)) {
      wts <- rep(1, length(urls))
    } else {
      if (!is.numeric(seed_weight)) {
        stop("`seed_weight` must be numeric or NULL.", call. = FALSE)
      }
      if (length(seed_weight) == 1L) {
        wts <- rep(seed_weight, length(urls))
      } else if (length(seed_weight) == length(urls)) {
        wts <- seed_weight
      } else {
        stop("`seed_weight` must be length 1 or match the number of seeds (",
          length(urls), ").",
          call. = FALSE
        )
      }
    }
  } else {
    stop("`seeds` must be a character vector of URLs or a data frame with URL ",
      "and weight columns.",
      call. = FALSE
    )
  }

  keep <- !is.na(urls) & nzchar(urls)
  urls <- urls[keep]
  wts <- wts[keep]
  if (length(urls) == 0) {
    stop("`seeds` has no usable cluster URLs.", call. = FALSE)
  }
  if (any(!is.na(wts) & wts < 0)) {
    stop("Cluster seed weights must be non-negative.", call. = FALSE)
  }

  data.frame(url = urls, weight = wts, stringsAsFactors = FALSE)
}

#' @rdname topic_feeder_pagerank
#' @export
topic_feeder_pagerank <- function(edge_list_df,
                                  seeds,
                                  seed_weight = NULL,
                                  seed_url_col = "url",
                                  seed_weight_col = "weight",
                                  ...) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  dots <- list(...)
  owned <- intersect(
    c("prior_df", "prior_url_col", "prior_weight_col", "reverse"),
    names(dots)
  )
  if (length(owned) > 0) {
    stop("Do not pass ", paste0("`", owned, "`", collapse = ", "),
      " to topic_feeder_pagerank(); the teleport prior is built from `seeds` ",
      "and the graph is always reversed (reverse = TRUE).",
      call. = FALSE
    )
  }

  prior <- feeder_seed_prior(
    seeds,
    seed_weight = seed_weight,
    seed_url_col = seed_url_col,
    seed_weight_col = seed_weight_col
  )

  res <- do.call(
    pagerank,
    c(
      list(edge_list_df = edge_list_df, prior_df = prior, reverse = TRUE),
      dots
    )
  )
  audit <- attr(res, "transition_audit")
  res <- res[order(-res$pagerank, res$node_name), , drop = FALSE]
  row.names(res) <- NULL
  attr(res, "transition_audit") <- audit
  res
}
