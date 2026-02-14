#' @title Resolve Redirects in an Edge List
#' @description Updates an edge list by replacing URLs with their final
#'   destinations based on a redirect data frame. Handles redirect chains,
#'   detects cycles, and resolves conflicting redirects using configurable
#'   policies.
#'
#' @param edge_list_df A data frame representing the edge list.
#' @param redirects_df A data frame containing redirect rules, with 'from' and
#'   'to' columns specifying the source and target of a redirect.
#' @param edge_from_col Character, the name of the column in `edge_list_df`
#'   containing source URLs. Default "from".
#' @param edge_to_col Character, the name of the column in `edge_list_df`
#'   containing target URLs. Default "to".
#' @param redirect_from_col Character, the name of the column in `redirects_df`
#'   containing source URLs of redirects. Default "from".
#' @param redirect_to_col Character, the name of the column in `redirects_df`
#'   containing target URLs of redirects. Default "to".
#' @param duplicate_from_policy Character, how to handle conflicting redirects
#'   (same source URL mapping to multiple distinct targets). One of:
#'   \describe{
#'     \item{"strict"}{(Default) Error on any conflict.}
#'     \item{"first_wins"}{Keep the first occurrence for each conflicting
#'       source.}
#'     \item{"last_wins"}{Keep the last occurrence for each conflicting source.}
#'     \item{"most_frequent"}{Keep the most common target. Ties broken by first
#'       occurrence.}
#'     \item{"prune_source"}{Remove ALL redirects from any conflicting source.}
#'     \item{"resolve_if_consistent"}{Allow exact duplicates; error only on
#'       true conflicts where targets differ.}
#'   }
#'
#' @return An updated `edge_list_df` with URLs in `edge_from_col` and
#'   `edge_to_col` replaced by their final resolved destinations.
#' @export
#' @examples
#' edges <- data.frame(
#'   from = c("A", "B", "C"),
#'   to = c("B", "C", "D"),
#'   stringsAsFactors = FALSE
#' )
#' redirects <- data.frame(
#'   from = c("B", "C", "E"),
#'   to = c("B_final", "C_final", "E_final"),
#'   stringsAsFactors = FALSE
#' )
#' resolve_redirects(edges, redirects)
#'
#' # Example with a redirect chain
#' edges_chain <- data.frame(from = "X", to = "Y", stringsAsFactors = FALSE)
#' redirects_chain <- data.frame(
#'   from = c("Y", "Z"),
#'   to = c("Z", "Z_final"),
#'   stringsAsFactors = FALSE
#' )
#' resolve_redirects(edges_chain, redirects_chain)
#'
#' # Example with conflicting redirects resolved via first_wins
#' edges_conflict <- data.frame(
#'   from = "A", to = "B", stringsAsFactors = FALSE
#' )
#' redirects_conflict <- data.frame(
#'   from = c("B", "B"),
#'   to = c("C", "D"),
#'   stringsAsFactors = FALSE
#' )
#' resolve_redirects(edges_conflict, redirects_conflict,
#'                   duplicate_from_policy = "first_wins")
#'
#' # Example with different column names
#' edges_custom <- data.frame(
#'   source_url = "Page1", target_url = "Page2"
#' )
#' redirects_custom <- data.frame(
#'   original = "Page2", final = "Page2_resolved"
#' )
#' resolve_redirects(edges_custom, redirects_custom,
#'                   edge_from_col = "source_url",
#'                   edge_to_col = "target_url",
#'                   redirect_from_col = "original",
#'                   redirect_to_col = "final")
#' @details
#' Self-referencing redirects (where from == to) and any redirects with NA
#' in from or to are automatically filtered out before processing.
#'
#' When crawl data contains conflicting redirects (the same URL redirecting
#' to different targets), use \code{duplicate_from_policy} to control the
#' behavior. The default \code{"strict"} preserves backward compatibility
#' by erroring on any conflict.

resolve_redirects <- function(edge_list_df,
                              redirects_df,
                              edge_from_col = "from",
                              edge_to_col = "to",
                              redirect_from_col = "from",
                              redirect_to_col = "to",
                              duplicate_from_policy = c("strict",
                                                        "first_wins",
                                                        "last_wins",
                                                        "most_frequent",
                                                        "prune_source",
                                                        "resolve_if_consistent")) {

  duplicate_from_policy <- match.arg(duplicate_from_policy)

  # --- Input Validation ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(edge_list_df) > 0 && !all(c(edge_from_col, edge_to_col) %in% names(edge_list_df))) {
    stop(paste0("`edge_list_df` must have '", edge_from_col, "' and '",
                edge_to_col, "' columns if not empty."), call. = FALSE)
  }

  if (!is.data.frame(redirects_df)) {
    stop("`redirects_df` must be a data frame.", call. = FALSE)
  }
  if (nrow(redirects_df) > 0 && !all(c(redirect_from_col, redirect_to_col) %in% names(redirects_df))) {
    stop("`redirects_df` must have '", redirect_from_col, "' and '",
         redirect_to_col, "' columns if not empty.", call. = FALSE)
  }

  # If redirects_df is empty or has no valid rules, return edge_list_df as is.
  if (nrow(redirects_df) == 0) {
    return(edge_list_df)
  }

  # Filter out NA values and self-referencing redirects (from == to).
  na_mask <- !is.na(redirects_df[[redirect_from_col]]) &
             !is.na(redirects_df[[redirect_to_col]])
  redirects_df <- redirects_df[na_mask, , drop = FALSE]

  if (nrow(redirects_df) > 0) {
    self_ref <- as.character(redirects_df[[redirect_from_col]]) ==
                as.character(redirects_df[[redirect_to_col]])
    redirects_df <- redirects_df[!self_ref, , drop = FALSE]
  }

  # --- Preprocess: handle conflicting redirects ---
  redirect_sources <- as.character(redirects_df[[redirect_from_col]])
  redirect_targets <- as.character(redirects_df[[redirect_to_col]])

  valid_redirect_indices <- !is.na(redirect_sources) & !is.na(redirect_targets)
  redirect_sources <- redirect_sources[valid_redirect_indices]
  redirect_targets <- redirect_targets[valid_redirect_indices]

  if (length(redirect_sources) == 0) {
    return(edge_list_df)
  }

  clean_df <- data.frame(from = redirect_sources, to = redirect_targets,
                         stringsAsFactors = FALSE)
  clean_df <- .preprocess_redirects(clean_df, "from", "to",
                                    policy = duplicate_from_policy)

  if (nrow(clean_df) == 0) {
    return(edge_list_df)
  }

  # --- Build redirect map ---
  redirect_map <- stats::setNames(clean_df$to, clean_df$from)

  # --- Resolve URLs in Edge List ---
  resolved_edge_list <- edge_list_df

  for (col_name in c(edge_from_col, edge_to_col)) {
    if (col_name %in% names(resolved_edge_list)) {
      original_urls <- as.character(resolved_edge_list[[col_name]])
      resolved_urls <- vapply(original_urls, function(url) {
        if (is.na(url)) return(NA_character_)
        .trace_redirect_path(url = url, redirect_map = redirect_map,
                             path = character(0))
      }, character(1))
      resolved_edge_list[[col_name]] <- resolved_urls
    }
  }

  return(resolved_edge_list)
}


# --- Internal: preprocess conflicting redirects ---

#' Preprocess redirects to handle conflicting sources
#'
#' Given a two-column data frame of (from, to) redirect pairs (already cleaned
#' of NAs and self-refs), detect conflicting sources and apply the chosen policy.
#'
#' @param redirects_df Data frame with columns named by `from_col` and `to_col`.
#' @param from_col Character, name of the source column.
#' @param to_col Character, name of the target column.
#' @param policy One of "strict", "first_wins", "last_wins", "most_frequent",
#'   "prune_source", "resolve_if_consistent".
#' @return A deduplicated data frame where each source maps to exactly one
#'   target.
#' @noRd
.preprocess_redirects <- function(redirects_df, from_col, to_col,
                                  policy = "strict") {

  sources <- redirects_df[[from_col]]
  targets <- redirects_df[[to_col]]

  # --- Detect conflicting sources ---
  # A source is "conflicting" if it has >1 distinct target
  src_target_pairs <- paste0(sources, "\t", targets)
  unique_pairs_df <- redirects_df[!duplicated(src_target_pairs), , drop = FALSE]
  dup_sources <- unique_pairs_df[[from_col]][
    duplicated(unique_pairs_df[[from_col]])
  ]
  conflicting_sources <- unique(dup_sources)

  # No conflicts: just deduplicate exact duplicates and return

  if (length(conflicting_sources) == 0) {
    return(unique_pairs_df)
  }

  # --- Apply policy ---
  if (policy == "strict") {
    first_conflict <- conflicting_sources[1]
    conflict_targets <- unique(targets[sources == first_conflict])
    stop("Ambiguous redirect: URL '", first_conflict,
         "' maps to multiple distinct targets: ",
         paste(conflict_targets, collapse = ", "), call. = FALSE)
  }

  if (policy == "resolve_if_consistent") {
    first_conflict <- conflicting_sources[1]
    conflict_targets <- unique(targets[sources == first_conflict])
    stop("Ambiguous redirect: URL '", first_conflict,
         "' maps to multiple distinct targets: ",
         paste(conflict_targets, collapse = ", "), call. = FALSE)
  }

  if (policy == "first_wins") {
    return(redirects_df[!duplicated(sources), , drop = FALSE])
  }

  if (policy == "last_wins") {
    return(redirects_df[!duplicated(sources, fromLast = TRUE), , drop = FALSE])
  }

  if (policy == "prune_source") {
    keep <- !(sources %in% conflicting_sources)
    # Also deduplicate the remaining non-conflicting redirects
    result <- redirects_df[keep, , drop = FALSE]
    dedup_key <- paste0(result[[from_col]], "\t", result[[to_col]])
    return(result[!duplicated(dedup_key), , drop = FALSE])
  }

  if (policy == "most_frequent") {
    # For non-conflicting sources: just deduplicate
    is_conflict <- sources %in% conflicting_sources
    non_conflict <- redirects_df[!is_conflict, , drop = FALSE]
    nc_key <- paste0(non_conflict[[from_col]], "\t", non_conflict[[to_col]])
    non_conflict <- non_conflict[!duplicated(nc_key), , drop = FALSE]

    # For conflicting sources: find the mode target per source
    conflict_df <- redirects_df[is_conflict, , drop = FALSE]
    resolved_rows <- lapply(conflicting_sources, function(src) {
      idx <- conflict_df[[from_col]] == src
      src_targets <- conflict_df[[to_col]][idx]
      freq <- table(src_targets)
      max_freq <- max(freq)
      candidates <- names(freq)[freq == max_freq]
      # Tie-break: first occurrence in original data
      winner <- candidates[1]
      for (cand in candidates) {
        if (match(cand, src_targets) < match(winner, src_targets)) {
          winner <- cand
        }
      }
      data.frame(from = src, to = winner, stringsAsFactors = FALSE)
    })
    resolved <- do.call(rbind, resolved_rows)
    names(resolved) <- c(from_col, to_col)
    return(rbind(non_conflict, resolved))
  }

  # Should not reach here due to match.arg in caller, but as safeguard
  stop("Unknown duplicate_from_policy: ", policy, call. = FALSE) # nocov
} 