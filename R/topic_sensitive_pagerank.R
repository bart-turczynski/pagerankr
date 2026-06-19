#' @title Topic-Sensitive PageRank (multi-vector personalized PageRank)
#' @description Computes a per-topic PageRank by running the standard
#'   [pagerank()] engine once per topic, biasing the random surfer's teleport
#'   toward each topic's seed pages, then optionally blends the per-topic scores
#'   into a single combined ranking.
#'
#'   This is Haveliwala's (2002) Topic-Sensitive PageRank adapted to a single
#'   site: instead of one global ranking, each "topic" is a content cluster
#'   (e.g. the *pricing* section, the *AI-Agent* product area, the *support*
#'   docs) defined by a set of seed URLs. A page can be highly authoritative for
#'   one topic and unimportant for another on the *same* link graph — the only
#'   thing that changes between runs is where the surfer teleports.
#'
#'   Mechanically this is pure orchestration over the existing TIPR
#'   personalization path: each topic becomes a `prior_df` handed to
#'   [pagerank()] (see [align_prior_to_vertices()]). There is no new solver and
#'   no topic inference — the caller supplies the seed sets.
#'
#' @param edge_list_df A data frame edge list, exactly as passed to [pagerank()]
#'   (see `edge_from_col` / `edge_to_col`).
#' @param topics A **uniquely named** list, one element per topic. Each element
#'   defines that topic's teleport seed set and is either:
#'   \describe{
#'     \item{a character vector of seed URLs}{each seed gets equal weight `1`;}
#'     \item{a data frame}{with a URL column and a numeric weight column (see
#'       `topic_url_col` / `topic_weight_col`) for weighted seeds.}
#'   }
#'   The list names become the per-topic score column names in the result, so
#'   they must be non-empty, unique, and must not be the reserved names
#'   `"node_name"` or `"blended"`. Seed URLs are canonicalized and redirect- /
#'   canonical-folded into the graph's vertex namespace by [pagerank()] before
#'   alignment, identically to any other prior.
#' @param topic_weights Optional blend weights for the `blended` column. Either
#'   a named numeric whose names match `topics`, or an unnamed numeric of the
#'   same length as `topics` (applied in list order). Must be non-negative,
#'   finite, and sum to a positive value; they are normalized to sum to 1
#'   internally. Default `NULL` gives every topic equal weight.
#' @param topic_url_col,topic_weight_col Column names used when a topic is
#'   supplied as a data frame. Defaults `"url"` / `"weight"`. Ignored for topics
#'   given as plain character vectors.
#' @param ... Additional arguments forwarded to [pagerank()] and onward to
#'   `igraph::page_rank` (e.g. `redirects_df`, `canonicals_df`, `rurl_params`,
#'   `weight_col`, `prior_transform`, `prior_alpha`, `damping`). Because this
#'   function owns the teleport prior, passing `prior_df`, `prior_url_col`, or
#'   `prior_weight_col` here is an error — supply `topics` instead. Inner
#'   per-topic alignment diagnostics are silenced by default
#'   (`prior_verbose = FALSE`); pass `prior_verbose = TRUE` to re-enable them.
#'
#' @details
#' All topics are scored on the **same** prepared graph: graph construction
#' (URL cleaning, redirect/canonical folding, domain/host filtering, duplicate
#' and isolate handling) depends only on `edge_list_df` and the forwarded
#' options, never on the teleport prior, so the vertex set is identical across
#' topics. The per-topic results are combined with a full outer join on
#' `node_name`; any node missing from a topic (which can only happen if you opt
#' into `prior_inject_unmatched = TRUE`, where unmatched seed URLs are injected
#' as topic-specific isolates) is filled with score `0` for that topic.
#'
#' The `blended` column is the weight-normalized linear combination
#' \eqn{\sum_t w_t \cdot score_t}. Each per-topic column carries the same mass
#' semantics as a single [pagerank()] run (it can sum to less than 1 under
#' nofollow evaporation or `robots_blocked_action = "vanish"`), and the blend
#' inherits that — it is a weighted average of the per-topic distributions, not
#' renormalized.
#'
#' @return A data frame with one row per node, sorted by `blended` descending:
#'   \describe{
#'     \item{node_name}{Node identifier (shared vertex namespace).}
#'     \item{<one column per topic>}{The personalized PageRank score for that
#'       topic, named after the corresponding `topics` entry.}
#'     \item{blended}{The `topic_weights`-weighted combination of the per-topic
#'       scores.}
#'   }
#'   Two attributes are attached: `"topic_weights"`, the normalized weights used
#'   for the blend, and `"topic_audits"`, a named list of the per-topic
#'   [transition_audit] objects from each underlying [pagerank()] run.
#'
#' @seealso [pagerank()], [align_prior_to_vertices()], [compare_pagerank()]
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("/", "/", "/", "/ai", "/ai", "/blog", "/pricing"),
#'   to = c("/ai", "/blog", "/pricing", "/ai-demo", "/pricing", "/ai", "/"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Two topics: the AI cluster and the pricing cluster.
#' res <- topic_sensitive_pagerank(
#'   edges,
#'   topics = list(
#'     ai_agent = c("/ai", "/ai-demo"),
#'     pricing = "/pricing"
#'   ),
#'   clean_edge_urls = FALSE
#' )
#' print(res)
#'
#' # Bias the blend 70/30 toward the AI cluster.
#' res2 <- topic_sensitive_pagerank(
#'   edges,
#'   topics = list(
#'     ai_agent = c("/ai", "/ai-demo"),
#'     pricing = "/pricing"
#'   ),
#'   topic_weights = c(ai_agent = 0.7, pricing = 0.3),
#'   clean_edge_urls = FALSE
#' )
#' attr(res2, "topic_weights")
topic_sensitive_pagerank <- function(edge_list_df,
                                     topics,
                                     topic_weights = NULL,
                                     topic_url_col = "url",
                                     topic_weight_col = "weight",
                                     ...) {
  # --- Validate topics ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!is.list(topics) || is.data.frame(topics) || length(topics) == 0) {
    stop(
      "`topics` must be a non-empty named list, one element per topic.",
      call. = FALSE
    )
  }
  topic_names <- names(topics)
  if (is.null(topic_names) || anyNA(topic_names) ||
        !all(nzchar(topic_names))) {
    stop("`topics` must be a list with a non-empty name for every element.",
      call. = FALSE
    )
  }
  if (anyDuplicated(topic_names)) {
    stop("`topics` names must be unique (they become result columns).",
      call. = FALSE
    )
  }
  reserved <- c("node_name", "blended")
  if (any(topic_names %in% reserved)) {
    stop("Topic names must not be 'node_name' or 'blended' (reserved result ",
      "columns).",
      call. = FALSE
    )
  }

  # --- Resolve and validate blend weights ---
  w <- .resolve_topic_weights(topic_weights, topic_names)

  # --- Guard against caller-supplied prior args (we own the prior) ---
  dots <- list(...)
  owned <- intersect(
    c("prior_df", "prior_url_col", "prior_weight_col"),
    names(dots)
  )
  if (length(owned) > 0) {
    stop("Do not pass ", paste0("`", owned, "`", collapse = ", "),
      " to topic_sensitive_pagerank(); the teleport prior is built from ",
      "`topics`.",
      call. = FALSE
    )
  }
  # Silence per-topic TIPR alignment messages unless the caller opts in.
  if (!("prior_verbose" %in% names(dots))) {
    dots$prior_verbose <- FALSE
  }

  # --- Run one personalized PageRank per topic ---
  score_dfs <- vector("list", length(topics))
  audits <- vector("list", length(topics))
  for (i in seq_along(topics)) {
    nm <- topic_names[[i]]
    prior <- .normalize_topic_prior(
      topics[[i]], nm, topic_url_col, topic_weight_col
    )
    res <- do.call(
      pagerank,
      c(list(edge_list_df = edge_list_df, prior_df = prior), dots)
    )
    if (!all(c("node_name", "pagerank") %in% names(res))) {
      stop("Internal: pagerank() did not return expected columns for topic '",
        nm, "'.",
        call. = FALSE
      )
    }
    df <- res[, c("node_name", "pagerank"), drop = FALSE]
    names(df) <- c("node_name", nm)
    score_dfs[[i]] <- df
    audits[[i]] <- attr(res, "transition_audit")
  }
  names(audits) <- topic_names

  # --- Merge per-topic scores (full outer join; absent node -> 0) ---
  merged <- Reduce(
    function(a, b) merge(a, b, by = "node_name", all = TRUE),
    score_dfs
  )
  for (nm in topic_names) {
    merged[[nm]][is.na(merged[[nm]])] <- 0
  }

  # --- Blend: weight-normalized linear combination of per-topic scores ---
  score_mat <- as.matrix(merged[, topic_names, drop = FALSE])
  merged[["blended"]] <- as.numeric(score_mat %*% w[topic_names])

  # --- Order columns and rows for a ranking-friendly result ---
  merged <- merged[, c("node_name", topic_names, "blended"), drop = FALSE]
  merged <- merged[order(-merged[["blended"]], merged[["node_name"]]),
    ,
    drop = FALSE
  ]
  row.names(merged) <- NULL

  attr(merged, "topic_weights") <- w
  attr(merged, "topic_audits") <- audits
  merged
}

#' Normalize one topic specification into a `prior_df` for pagerank().
#' @keywords internal
#' @noRd
.normalize_topic_prior <- function(topic, name, url_col, weight_col) {
  if (is.data.frame(topic)) {
    if (!all(c(url_col, weight_col) %in% names(topic))) {
      stop("Topic '", name, "' (a data frame) must have '", url_col,
        "' and '", weight_col, "' columns.",
        call. = FALSE
      )
    }
    urls <- as.character(topic[[url_col]])
    wts <- suppressWarnings(as.numeric(topic[[weight_col]]))
  } else if (is.character(topic) || is.factor(topic)) {
    urls <- as.character(topic)
    wts <- rep(1, length(urls))
  } else {
    stop("Topic '", name, "' must be a character vector of seed URLs or a ",
      "data frame with URL and weight columns.",
      call. = FALSE
    )
  }

  keep <- !is.na(urls) & nzchar(urls)
  urls <- urls[keep]
  wts <- wts[keep]
  if (length(urls) == 0) {
    stop("Topic '", name, "' has no usable seed URLs.", call. = FALSE)
  }

  data.frame(url = urls, weight = wts, stringsAsFactors = FALSE)
}

#' Resolve and normalize topic blend weights to sum to 1.
#' @keywords internal
#' @noRd
.resolve_topic_weights <- function(topic_weights, topic_names) {
  n <- length(topic_names)
  if (is.null(topic_weights)) {
    w <- rep(1 / n, n)
    names(w) <- topic_names
    return(w)
  }
  if (!is.numeric(topic_weights)) {
    stop("`topic_weights` must be a numeric vector or NULL.", call. = FALSE)
  }
  if (!is.null(names(topic_weights))) {
    if (!setequal(names(topic_weights), topic_names)) {
      stop("Named `topic_weights` must have exactly the same names as ",
        "`topics`.",
        call. = FALSE
      )
    }
    w <- topic_weights[topic_names]
  } else {
    if (length(topic_weights) != n) {
      stop("Unnamed `topic_weights` must have one weight per topic (got ",
        length(topic_weights), " for ", n, " topics).",
        call. = FALSE
      )
    }
    w <- topic_weights
    names(w) <- topic_names
  }
  if (anyNA(w) || !all(is.finite(w))) {
    stop("`topic_weights` must be finite and non-missing.", call. = FALSE)
  }
  if (any(w < 0)) {
    stop("`topic_weights` must be non-negative.", call. = FALSE)
  }
  s <- sum(w)
  if (s <= 0) {
    stop("`topic_weights` must sum to a positive value.", call. = FALSE)
  }
  w / s
}
