#' @title Seed Teleport Prior for Personalized PageRank
#' @description Build a teleport prior (a `prior_df`) concentrated on a set of
#'   **seed** pages. A seed prior is the teleportation vector that biases the
#'   random surfer toward the seeds instead of jumping uniformly, and it is the
#'   single ingredient shared by the seed-biased members of the PageRank family:
#'   [trustrank()] (trusted seeds) and [topic_feeder_pagerank()] (a target
#'   cluster) both build one and hand it to [pagerank()].
#'
#'   `seed_prior()` is **orientation-agnostic**: it does nothing but turn a seed
#'   set into a `url`/`weight` prior. Whether teleport mass then flows *outward*
#'   from the seeds (trust) or is accumulated by pages that *point into* the
#'   seeds (feeders) is a property of the **graph**, chosen by the caller — not
#'   of this builder. [trustrank()] runs the prior on the forward graph;
#'   [topic_feeder_pagerank()] runs the identical prior on the reversed graph
#'   (`pagerank(reverse = TRUE)`). That is precisely why one builder serves
#'   both: the direction lives in the wrapper, not in the prior.
#'
#'   For the multi-topic case ([topic_sensitive_pagerank()]) the prior is built
#'   internally per topic from a named list; `seed_prior()` covers the
#'   single-seed-set case that the two convenience wrappers share.
#'
#' @param seeds The seed set. Either a character vector of seed URLs (each gets
#'   equal weight unless `seed_weight` is given), or a data frame with a URL
#'   column and a numeric weight column (see `seed_url_col` / `seed_weight_col`)
#'   for unequal emphasis.
#' @param seed_weight Optional numeric weight for a character-vector `seeds`:
#'   either one value per seed or a single value recycled to all seeds. Ignored
#'   when `seeds` is a data frame. Default `NULL` (every seed weight `1`, i.e. a
#'   uniform distribution over the seed set).
#' @param seed_url_col,seed_weight_col Column names used when `seeds` is a data
#'   frame. Defaults `"url"` / `"weight"`. Ignored for a character vector.
#'
#' @details
#' Seed weights are an **additive teleport budget**: when two seed URLs fold
#' onto the same vertex (redirect / canonical variants) their weights sum,
#' exactly as the [pagerank()] / [align_prior_to_vertices()] prior contract
#' specifies. Equal weights give a uniform distribution over the seed set;
#' unequal weights express graded emphasis (graded trust for [trustrank()],
#' graded cluster importance for [topic_feeder_pagerank()]).
#'
#' @return A data frame with `url` and `weight` columns, suitable as the
#'   `prior_df` argument to [pagerank()].
#'
#' @seealso [trustrank()], [topic_feeder_pagerank()],
#'   [topic_sensitive_pagerank()], [pagerank()], [align_prior_to_vertices()]
#' @examples
#' # A trusted-seed prior for TrustRank: run on the FORWARD graph, trust flows
#' # outward from the seeds.
#' prior <- seed_prior(c("/", "/hub"))
#' prior
#'
#' edges <- data.frame(
#'   from = c("/", "/hub", "/feeder"),
#'   to = c("/hub", "/ai", "/ai")
#' )
#' pagerank(edges, prior_df = prior, clean_edge_urls = FALSE)
#'
#' # The SAME builder makes a cluster prior for feeder PageRank; the only
#' # difference is the graph orientation you run it on (reverse = TRUE).
#' cluster <- seed_prior("/ai")
#' pagerank(edges, prior_df = cluster, reverse = TRUE, clean_edge_urls = FALSE)
#'
#' # Graded emphasis via a data frame.
#' seed_prior(data.frame(url = c("/a", "/b"), weight = c(3, 1)))
#' @export
seed_prior <- function(seeds,
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
    wts <- .resolve_seed_weights(seed_weight, urls)
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
    stop("`seeds` has no usable seed URLs.", call. = FALSE)
  }
  if (any(!is.na(wts) & wts < 0)) {
    stop("Seed weights must be non-negative.", call. = FALSE)
  }

  data.frame(url = urls, weight = wts)
}

#' Resolve per-seed weights for a character vector of seeds.
#'
#' Shared by [seed_prior()]; expands a scalar or per-seed `seed_weight` (or the
#' `NULL` default of uniform weight `1`) to one weight per seed URL.
#' @keywords internal
#' @noRd
.resolve_seed_weights <- function(seed_weight, urls) {
  if (is.null(seed_weight)) {
    return(rep(1, length(urls)))
  }
  if (!is.numeric(seed_weight)) {
    stop("`seed_weight` must be numeric or NULL.", call. = FALSE)
  }
  if (length(seed_weight) == 1L) {
    rep(seed_weight, length(urls))
  } else if (length(seed_weight) == length(urls)) {
    seed_weight
  } else {
    stop("`seed_weight` must be length 1 or match the number of seeds (",
      length(urls), ").",
      call. = FALSE
    )
  }
}

#' Reject caller-supplied arguments the wrapper owns.
#'
#' The seed-biased wrappers build (and, for the feeder, orient) the teleport
#' prior themselves, so caller-supplied `prior_df` / `prior_url_col` /
#' `prior_weight_col` (and `reverse` for the feeder) are user errors rather than
#' silently-overridden inputs. Shared by [trustrank()],
#' [topic_feeder_pagerank()], and [topic_sensitive_pagerank()].
#'
#' @param dots The captured `list(...)` forwarded to [pagerank()].
#' @param owned_names Character vector of argument names the wrapper owns.
#' @param caller The wrapper's function name, used in the error message.
#' @param detail A trailing clause explaining what to supply instead.
#' @keywords internal
#' @noRd
.reject_owned_args <- function(dots, owned_names, caller, detail) {
  owned <- intersect(owned_names, names(dots))
  if (length(owned) > 0) {
    stop("Do not pass ", paste0("`", owned, "`", collapse = ", "),
      " to ", caller, "(); ", detail,
      call. = FALSE
    )
  }
  invisible(NULL)
}
