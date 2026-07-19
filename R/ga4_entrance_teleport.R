#' @title GA4 Entrance / Landing-Page Teleport Adapter (PROXY)
#' @description Turns GA4 \strong{entrance / landing-page counts} into the
#'   teleport (reset / personalization) vector for weighted PageRank with an
#'   \emph{entrance-biased reset}. Each session start is treated as a teleport
#'   event whose destination is the landing page, so pages where users more
#'   often \emph{begin} a session receive proportionally more of the random
#'   surfer's reset mass — replacing the uniform teleport of standard PageRank.
#'
#'   This is the cheapest of the three behavioral-reset models (it reuses the
#'   standard PageRank machine unchanged), and it is deliberately a
#'   \strong{proxy}: see the dedicated note below.
#'
#' @section This is a PROXY, not an identity:
#'   Session starts are \strong{not literally equivalent to every PageRank
#'   teleport event}. The teleport in PageRank fires on \emph{every} damping
#'   draw (including mid-session "I got bored, jump elsewhere" restarts), while
#'   GA4 entrances only observe the \emph{first} page of a session. Using
#'   entrances as the reset distribution is a defensible approximation of "where
#'   browsing tends to (re)start," but it is an approximation. Higher-fidelity
#'   models — page-specific exit probabilities (a discrete behavioral Markov
#'   model) and continuous-time BrowseRank with dwell time — are explicitly
#'   \strong{out of scope here}. Treat, report, and cite this vector as the
#'   \emph{entrance-biased teleport proxy}.
#'
#' @section Distinct from the backlink-authority prior:
#'   This adapter and the external-authority TIPR prior (e.g. Ahrefs referring
#'   domains; see [align_prior_to_vertices()]) both flow through the same
#'   \code{prior_df} / [align_prior_to_vertices()] plumbing, but they answer
#'   \strong{different questions} and should not be conflated:
#'   \itemize{
#'     \item \strong{Backlink-authority prior} — where \emph{authority enters}
#'       the graph from outside (off-site links). A structural/link signal.
#'     \item \strong{Entrance teleport (this adapter)} — where \emph{users}
#'       enter / restart browsing (observed session starts). A behavioral
#'       signal, and only a proxy for the teleport event (see above).
#'   }
#'   They can be used as alternatives, or — outside this function's remit —
#'   blended; that mixing policy is not decided here.
#'
#' @section Naming decision (prior_df vs teleport_df/reset_df):
#'   We \strong{reuse the existing \code{prior_df} machinery} rather than
#'   introducing a separate \code{teleport_df} / \code{reset_df}. Rationale:
#'   (1) entrances are \strong{additive raw counts}, so they satisfy the same
#'   TIPR additive-count contract as referring-domain counts — duplicate /
#'   redirect-folded URLs combine by summation, which is exactly what
#'   [align_prior_to_vertices()] already does; (2) both signals produce a
#'   teleport vector over the \emph{same} final vertex set with the \emph{same}
#'   canonicalization + redirect fold, so a parallel data-frame type and a
#'   parallel alignment path would be duplicated machinery for no behavioral
#'   gain; (3) the \emph{semantic} distinction (authority-in vs users-in) is
#'   carried by documentation and by the proxy labeling here, not by the data
#'   structure. If a future model needs to \emph{blend} a backlink prior and an
#'   entrance reset in a single \code{pagerank()} call, that is the point to
#'   revisit and split the type (tracked as research-notes Q5 / Q3).
#'
#' @param entrances_df A data frame with one row per (landing page, entrance
#'   count) observation, e.g. a GA4 "Landing page" report. Multiple rows for the
#'   same URL are summed (entrances are additive raw counts). Rows with a
#'   missing URL or a missing / negative count are dropped.
#' @param url_col Name of the landing-page URL column in \code{entrances_df}.
#'   Default \code{"url"}.
#' @param entrances_col Name of the numeric entrance-count column in
#'   \code{entrances_df}. Default \code{"entrances"}. \strong{Contract: this
#'   must be an additive raw count} (session starts / entrances), never a rate,
#'   share, or computed score — folding two URLs onto one representative is a
#'   meaningful sum. This mirrors the TIPR additive-count contract in
#'   [align_prior_to_vertices()].
#' @param vertex_names Optional character vector of the graph's vertex names, in
#'   graph order (e.g. \code{igraph::V(graph)$name}). When supplied, the adapter
#'   returns the \strong{aligned teleport vector} directly by delegating to
#'   [align_prior_to_vertices()] (passing \code{transform}, \code{alpha},
#'   \code{exclude_nodes}, \code{verbose}). The \code{vertex_names} you pass
#'   must already be canonicalized + redirect-folded like the edges; pass
#'   \code{NULL} (the default) to get back a \code{prior_df}-shaped data frame
#'   and let [pagerank()] perform that canonicalization + fold (the recommended
#'   path — single source of truth for folding).
#' @param transform,alpha,exclude_nodes,verbose Passed through to
#'   [align_prior_to_vertices()] when \code{vertex_names} is supplied; ignored
#'   otherwise. See that function for semantics. \code{alpha = 1} recovers the
#'   standard uniform teleport.
#'
#' @return If \code{vertex_names} is \code{NULL} (default), a data frame with
#'   columns \code{url} and \code{weight} (one row per unique landing-page URL,
#'   entrances summed) ready to pass to \code{pagerank(prior_df = , alpha = )}.
#'   If \code{vertex_names} is supplied, a numeric teleport vector the same
#'   length and order as \code{vertex_names}, summing to 1 (the return value of
#'   [align_prior_to_vertices()]).
#'
#' @details
#' \strong{Uniform entrances recover uniform teleport.} If every (real) vertex
#' has the same entrance count, the entrance share is uniform, so the resulting
#' teleport vector equals the standard uniform PageRank reset — the proxy
#' degrades gracefully to the default when there is no entrance signal.
#'
#' Recommended usage (let \code{pagerank()} own the fold):
#' \preformatted{
#'   tp <- ga4_entrance_teleport(ga4_landing_report,
#'                               url_col = "landing_page",
#'                               entrances_col = "sessions")
#'   pagerank(edges, prior_df = tp)   # prior_df = data.frame(url, weight)
#' }
#'
#' @seealso [align_prior_to_vertices()], [pagerank()], [transform_weights()]
#' @export
#' @examples
#' ga4 <- data.frame(
#'   url = c("https://x/a", "https://x/a", "https://x/b"),
#'   entrances = c(60, 30, 10)
#' )
#' # As a prior_df for pagerank() (it does the canonicalize + fold):
#' ga4_entrance_teleport(ga4)
#'
#' # Or align directly to a known final vertex set:
#' v <- c("https://x/a", "https://x/b", "https://x/c")
#' ga4_entrance_teleport(ga4, vertex_names = v, verbose = FALSE)
ga4_entrance_teleport <- function(entrances_df,
                                  url_col = "url",
                                  entrances_col = "entrances",
                                  vertex_names = NULL,
                                  transform = c(
                                    "none", "log", "percentile",
                                    "minmax", "zipf", "rank_linear"
                                  ),
                                  alpha = 0,
                                  exclude_nodes = character(0),
                                  verbose = TRUE) {
  transform <- match.arg(transform)

  # --- Validation ---
  if (!is.data.frame(entrances_df)) {
    stop("`entrances_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(entrances_df) > 0 &&
        !all(c(url_col, entrances_col) %in% names(entrances_df))) {
    stop("`entrances_df` must have '", url_col, "' and '", entrances_col,
      "' columns.",
      call. = FALSE
    )
  }

  # --- Normalize to the additive-count prior contract (sum per URL) ---
  prior_df <- .ga4_entrance_prior_df(entrances_df, url_col, entrances_col)

  # --- Without a vertex set: return the prior_df for pagerank() to fold ---
  if (is.null(vertex_names)) {
    return(prior_df)
  }

  # --- With a vertex set: align directly (reuses the TIPR machinery) ---
  align_prior_to_vertices(
    vertex_names = vertex_names,
    prior_df = prior_df,
    prior_url_col = "url",
    prior_weight_col = "weight",
    transform = transform,
    alpha = alpha,
    exclude_nodes = exclude_nodes,
    verbose = verbose
  )
}

#' Normalize GA4 entrances to the additive-count prior contract (sum per URL).
#'
#' Drops rows with missing or negative counts, then sums counts per URL.
#' Returns a `data.frame(url, weight)` (empty when no rows survive).
#' @noRd
.ga4_entrance_prior_df <- function(entrances_df, url_col, entrances_col) {
  prior_url <- character(0)
  prior_weight <- numeric(0)
  if (nrow(entrances_df) > 0) {
    urls <- as.character(entrances_df[[url_col]])
    ent <- suppressWarnings(as.numeric(entrances_df[[entrances_col]]))
    # Entrances are non-negative additive counts: drop missing / negative rows.
    keep <- !is.na(urls) & !is.na(ent) & ent >= 0
    urls <- urls[keep]
    ent <- ent[keep]
    if (length(urls) > 0) {
      agg <- tapply(ent, urls, sum)
      prior_url <- names(agg)
      prior_weight <- as.numeric(agg)
    }
  }

  data.frame(
    url = prior_url,
    weight = prior_weight
  )
}
