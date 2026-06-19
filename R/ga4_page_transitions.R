#' @title Build Page-Transition Counts from a GA4 BigQuery Export
#' @description Builds consecutive-page-view **transition counts** from a
#'   Google Analytics 4 (GA4) BigQuery event-export data frame. The result is a
#'   `from`/`to` edge list with a count column, in the shape that
#'   [pagerank()] accepts (pass the count column via `weight_col`).
#'
#'   This function operates entirely on a data frame **you supply** — it does
#'   **not** query BigQuery and adds no database dependencies. Extract the GA4
#'   `events_*` rows you care about (typically `page_view` events, with the
#'   session-identity and ordering fields un-nested from
#'   `event_params` / the `batch` struct) into a data frame, then pass it here.
#'
#' @section What this measures (transition, NOT link-click):
#'   The output is a **behavioral navigation signal** — the empirical
#'   "where did users go next" sequence of page views within a session. It is
#'   **not** a measured link-click probability. GA4 page-view sequences are
#'   contaminated by page reloads, browser back/forward navigation, server
#'   redirects, single-page-application route changes, dropped/missing events,
#'   and off-site returns. A transition `A -> B` means "a session viewed page A
#'   and then viewed page B next", which is **not** the same as "a user clicked
#'   a link from A to B." For link-click instrumentation (the actual element
#'   clicked), a separate `ga4_link_clicks()` product is required; do not use
#'   this function as a substitute for it.
#'
#' @section Session / event ordering contract:
#'   Within each session, events are ordered by `event_timestamp` and then by a
#'   deterministic chain of tie-break fields, **in this order**:
#'   \enumerate{
#'     \item `event_timestamp` (microseconds since epoch),
#'     \item `batch_page_id`,
#'     \item `batch_ordering_id`,
#'     \item `batch_event_index`.
#'   }
#'   `event_timestamp` **alone is insufficient**: GA4 batches events and
#'   multiple events in a session can share the exact same `event_timestamp`.
#'   When timestamps tie, the `batch_*` fields (assigned by the GA4 SDK in the
#'   order events were recorded on the client) break the tie so the ordering is
#'   stable and reproducible. Any tie-break column that is absent from
#'   `events_df` is simply skipped, but supplying all of them is strongly
#'   recommended to guarantee a deterministic order. As a final stabiliser the
#'   original row order of `events_df` is used, so the result never depends on
#'   the platform's sort implementation.
#'
#'   A *session* is identified by the combination of `user_id_col` and
#'   `session_id_col` (GA4: `user_pseudo_id` and the `ga_session_id` event
#'   parameter). Transitions are only formed **within** a single session;
#'   consecutive page views that cross a session boundary are never joined.
#'
#' @param events_df A data frame of GA4 export rows, one row per event
#'   (typically filtered to `page_view` events upstream). Must contain the
#'   session-identity, page, and timestamp columns named below; the `batch_*`
#'   tie-break columns are optional but recommended.
#' @param user_id_col Name of the user-identity column. GA4 default
#'   `"user_pseudo_id"`.
#' @param session_id_col Name of the session-identity column (the un-nested
#'   `ga_session_id` event parameter). GA4 default `"ga_session_id"`.
#' @param page_col Name of the page-identity column whose consecutive values
#'   form the transitions. GA4 default `"page_location"`.
#' @param timestamp_col Name of the primary ordering column. GA4 default
#'   `"event_timestamp"`.
#' @param batch_page_id_col,batch_ordering_id_col,batch_event_index_col Names
#'   of the GA4 batch tie-break columns, applied in this order after
#'   `timestamp_col`. GA4 defaults `"batch_page_id"`, `"batch_ordering_id"`,
#'   `"batch_event_index"`. A column that is not present in `events_df` is
#'   skipped.
#' @param from_col,to_col Names of the source/target columns in the returned
#'   edge list. Defaults `"from"` / `"to"` (the [pagerank()] defaults).
#' @param count_col Name of the transition-count column in the returned edge
#'   list. Default `"n"`. Pass this name to `pagerank(weight_col = ...)`.
#' @param drop_self_transitions Logical. If `TRUE` (default), consecutive page
#'   views of the **same** page (reloads, SPA re-renders to the same route) are
#'   dropped before counting. If `FALSE`, self-transitions are kept and counted.
#'
#' @return A data frame with one row per distinct `from -> to` page transition,
#'   carrying the columns named by `from_col`, `to_col`, and `count_col`. The
#'   count column is an integer tally of how many times that consecutive
#'   page-view transition was observed across all sessions. Rows are ordered by
#'   `from` then `to` for stable output. When no transitions exist (e.g. every
#'   session has a single page view), an empty data frame with the correct
#'   columns is returned.
#'
#' @seealso [pagerank()] for consuming the result; [transform_weights()] for
#'   turning raw transition counts into PageRank edge weights.
#'
#' @export
#' @examples
#' events <- data.frame(
#'   user_pseudo_id = c("u1", "u1", "u1", "u2", "u2"),
#'   ga_session_id = c(1, 1, 1, 9, 9),
#'   page_location = c("/home", "/blog", "/contact", "/home", "/blog"),
#'   event_timestamp = c(100, 200, 300, 100, 200),
#'   batch_page_id = c(0, 1, 2, 0, 1),
#'   batch_ordering_id = c(0, 0, 0, 0, 0),
#'   batch_event_index = c(0, 1, 2, 0, 1),
#'   stringsAsFactors = FALSE
#' )
#' transitions <- ga4_page_transitions(events)
#' transitions
#' # Feed to pagerank() as a behavioral transition model:
#' # pagerank(transitions, weight_col = "n", clean_edge_urls = FALSE)
ga4_page_transitions <- function(events_df,
                                 user_id_col = "user_pseudo_id",
                                 session_id_col = "ga_session_id",
                                 page_col = "page_location",
                                 timestamp_col = "event_timestamp",
                                 batch_page_id_col = "batch_page_id",
                                 batch_ordering_id_col = "batch_ordering_id",
                                 batch_event_index_col = "batch_event_index",
                                 from_col = "from",
                                 to_col = "to",
                                 count_col = "n",
                                 drop_self_transitions = TRUE) {
  # --- Validation ---
  if (!is.data.frame(events_df)) {
    stop("`events_df` must be a data frame.", call. = FALSE)
  }

  char_args <- list(
    user_id_col = user_id_col,
    session_id_col = session_id_col,
    page_col = page_col,
    timestamp_col = timestamp_col,
    from_col = from_col,
    to_col = to_col,
    count_col = count_col
  )
  for (nm in names(char_args)) {
    val <- char_args[[nm]]
    if (!is.character(val) || length(val) != 1 || is.na(val)) {
      stop("`", nm, "` must be a single non-NA character string.",
        call. = FALSE
      )
    }
  }

  if (!is.logical(drop_self_transitions) ||
        length(drop_self_transitions) != 1 ||
        is.na(drop_self_transitions)) {
    stop("`drop_self_transitions` must be TRUE or FALSE.", call. = FALSE)
  }

  # Required identity / page / ordering columns.
  required_cols <- c(user_id_col, session_id_col, page_col, timestamp_col)
  missing_required <- required_cols[!required_cols %in% names(events_df)]
  if (length(missing_required) > 0) {
    stop(
      "`events_df` is missing required column(s): ",
      paste(missing_required, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # Optional tie-break columns, applied in the documented order. Skip any that
  # the user did not supply in `events_df`.
  tie_break_cols <- c(
    batch_page_id_col, batch_ordering_id_col, batch_event_index_col
  )
  tie_break_cols <- tie_break_cols[
    !vapply(tie_break_cols, is.null, logical(1))
  ]
  tie_break_cols <- tie_break_cols[tie_break_cols %in% names(events_df)]

  # --- Empty input: return an empty edge list with the right shape. ---
  empty_result <- function() {
    out <- data.frame(
      a = character(0), b = character(0), c = integer(0),
      stringsAsFactors = FALSE
    )
    names(out) <- c(from_col, to_col, count_col)
    out
  }

  if (nrow(events_df) == 0) {
    return(empty_result())
  }

  # --- Deterministic ordering contract ---
  # Order by session identity, then event_timestamp, then the batch tie-break
  # fields in order, then original row order as a final stabiliser.
  page <- as.character(events_df[[page_col]])
  user <- events_df[[user_id_col]]
  session <- events_df[[session_id_col]]

  order_keys <- c(
    list(user, session, events_df[[timestamp_col]]),
    lapply(tie_break_cols, function(cc) events_df[[cc]]),
    list(seq_len(nrow(events_df)))
  )
  ord <- do.call(order, order_keys)

  user <- user[ord]
  session <- session[ord]
  page <- page[ord]

  # --- Build within-session consecutive transitions ---
  n <- length(page)
  if (n < 2L) {
    return(empty_result())
  }

  # A transition is valid only between adjacent rows of the SAME session.
  # Compare each row with its predecessor.
  same_session <- (user[-1] == user[-n]) & (session[-1] == session[-n])
  # Guard against NA session identities (treat NA-involving pairs as a break).
  same_session[is.na(same_session)] <- FALSE

  from_vec <- page[-n][same_session]
  to_vec <- page[-1][same_session]

  # Drop transitions with an NA page on either side.
  keep <- !is.na(from_vec) & !is.na(to_vec)
  from_vec <- from_vec[keep]
  to_vec <- to_vec[keep]

  if (drop_self_transitions) {
    not_self <- from_vec != to_vec
    from_vec <- from_vec[not_self]
    to_vec <- to_vec[not_self]
  }

  if (length(from_vec) == 0) {
    return(empty_result())
  }

  # --- Aggregate to transition counts ---
  agg <- stats::aggregate(
    list(count = rep(1L, length(from_vec))),
    by = list(from = from_vec, to = to_vec),
    FUN = sum
  )

  # Stable output order: by from, then to.
  agg <- agg[order(agg$from, agg$to), , drop = FALSE]

  out <- data.frame(
    a = as.character(agg$from),
    b = as.character(agg$to),
    c = as.integer(agg$count),
    stringsAsFactors = FALSE
  )
  names(out) <- c(from_col, to_col, count_col)
  row.names(out) <- NULL
  out
}
