#' @title TrustRank Seed Prior and Seed-Biased PageRank
#' @description TrustRank (Gyöngyi, Garcia-Molina & Pedersen, 2004) is
#'   personalized PageRank whose teleport vector is concentrated on a set of
#'   **trusted seed** pages instead of being uniform. Trust then flows outward
#'   along links and attenuates with distance (the PageRank damping factor *is*
#'   the trust-attenuation mechanism), so pages well-linked from the trusted
#'   core score high and pages far from it score low.
#'
#'   `pagerankr` implements this with **no new solver**: a trusted-seed prior is
#'   exactly a `prior_df` for the existing TIPR personalization path.
#'   [trust_seed_prior()] builds that prior from a seed set, and [trustrank()]
#'   is the worked convenience wrapper that builds the seed prior and runs
#'   [pagerank()] with it.
#'
#'   This is **seed-biased PageRank**, not a full spam-detection system: it
#'   reproduces the biased-propagation core of TrustRank, leaving seed selection
#'   (expert-reviewed "good" pages) to the caller.
#'
#' @param trusted_seeds The trusted seed set. Either a character vector of
#'   trusted URLs (each gets equal seed weight unless `seed_weight` is given),
#'   or a data frame with a URL column and a numeric weight column (see
#'   `seed_url_col` / `seed_weight_col`) for unequal trust.
#' @param seed_weight Optional numeric trust weight for a character-vector
#'   `trusted_seeds`: either one value per seed or a single value recycled to
#'   all seeds. Ignored when `trusted_seeds` is a data frame. Default `NULL`
#'   (every seed weight `1`, i.e. a uniform distribution over the trusted set,
#'   as in the original TrustRank).
#' @param seed_url_col,seed_weight_col Column names used when `trusted_seeds` is
#'   a data frame. Defaults `"url"` / `"weight"`. Ignored for a character
#'   vector.
#'
#' @details
#' The seed weights are an **additive trust budget**: when two seed URLs fold
#' onto the same vertex (redirect/canonical variants) their weights sum, exactly
#' as the [pagerank()] / [align_prior_to_vertices()] prior contract specifies.
#' Equal weights reproduce TrustRank's uniform seed distribution; unequal
#' weights express graded trust.
#'
#' [trustrank()] forwards `...` to [pagerank()], so the full graph-preparation
#' surface (redirects, canonicals, URL cleaning, domain/host filtering, edge
#' weights, duplicate-edge policy) and the prior-shaping knobs
#' (`prior_transform`, `prior_alpha`) are all available. In particular
#' `prior_alpha` mixes a uniform teleport baseline back in: `prior_alpha = 0`
#' (the default) is pure trust teleport (untrusted, unreachable pages get no
#' teleport mass), while a small positive value gives every page a floor.
#' Because this owns the prior, passing `prior_df`, `prior_url_col`, or
#' `prior_weight_col` to [trustrank()] is an error — supply `trusted_seeds`.
#'
#' @return [trust_seed_prior()] returns a data frame with `url` and `weight`
#'   columns, suitable as the `prior_df` argument to [pagerank()].
#'
#'   [trustrank()] returns the [pagerank()] result data frame (`node_name`,
#'   `pagerank`, and the `prior_weight` column the prior path adds), carrying
#'   the usual `"transition_audit"` attribute.
#'
#' @seealso [pagerank()], [align_prior_to_vertices()],
#'   [topic_sensitive_pagerank()]
#' @name trustrank
#' @examples
#' edges <- data.frame(
#'   from = c("/", "/", "/hub", "/hub", "/spam", "/good"),
#'   to = c("/hub", "/good", "/good", "/deep", "/good", "/hub")
#' )
#'
#' # Build a trusted-seed prior, then run it through pagerank() manually.
#' prior <- trust_seed_prior(c("/", "/hub"))
#' pr <- pagerank(edges, prior_df = prior, clean_edge_urls = FALSE)
#'
#' # ...or in one call with the convenience wrapper.
#' tr <- trustrank(edges, c("/", "/hub"), clean_edge_urls = FALSE)
#' print(tr)
NULL

#' @rdname trustrank
#' @export
trust_seed_prior <- function(trusted_seeds,
                             seed_weight = NULL,
                             seed_url_col = "url",
                             seed_weight_col = "weight") {
  if (is.data.frame(trusted_seeds)) {
    if (!all(c(seed_url_col, seed_weight_col) %in% names(trusted_seeds))) {
      stop("`trusted_seeds` (a data frame) must have '", seed_url_col,
        "' and '", seed_weight_col, "' columns.",
        call. = FALSE
      )
    }
    if (!is.null(seed_weight)) {
      stop("`seed_weight` applies only when `trusted_seeds` is a character ",
        "vector; put per-seed weights in the '", seed_weight_col,
        "' column instead.",
        call. = FALSE
      )
    }
    urls <- as.character(trusted_seeds[[seed_url_col]])
    wts <- suppressWarnings(as.numeric(trusted_seeds[[seed_weight_col]]))
  } else if (is.character(trusted_seeds) || is.factor(trusted_seeds)) {
    urls <- as.character(trusted_seeds)
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
    stop("`trusted_seeds` must be a character vector of URLs or a data frame ",
      "with URL and weight columns.",
      call. = FALSE
    )
  }

  keep <- !is.na(urls) & nzchar(urls)
  urls <- urls[keep]
  wts <- wts[keep]
  if (length(urls) == 0) {
    stop("`trusted_seeds` has no usable seed URLs.", call. = FALSE)
  }
  if (any(!is.na(wts) & wts < 0)) {
    stop("Trusted-seed weights must be non-negative.", call. = FALSE)
  }

  data.frame(url = urls, weight = wts)
}

#' @rdname trustrank
#' @param edge_list_df A data frame edge list, as passed to [pagerank()].
#' @param ... Additional arguments forwarded to [pagerank()] (e.g.
#'   `redirects_df`, `rurl_params`, `prior_transform`, `prior_alpha`,
#'   `damping`). Passing `prior_df`, `prior_url_col`, or `prior_weight_col` is
#'   an error.
#' @export
trustrank <- function(edge_list_df,
                      trusted_seeds,
                      seed_weight = NULL,
                      seed_url_col = "url",
                      seed_weight_col = "weight",
                      ...) {
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  dots <- list(...)
  owned <- intersect(
    c("prior_df", "prior_url_col", "prior_weight_col"),
    names(dots)
  )
  if (length(owned) > 0) {
    stop("Do not pass ", paste0("`", owned, "`", collapse = ", "),
      " to trustrank(); the teleport prior is built from `trusted_seeds`.",
      call. = FALSE
    )
  }

  prior <- trust_seed_prior(
    trusted_seeds,
    seed_weight = seed_weight,
    seed_url_col = seed_url_col,
    seed_weight_col = seed_weight_col
  )

  do.call(
    pagerank,
    c(list(edge_list_df = edge_list_df, prior_df = prior), dots)
  )
}
