#' @title Transform Edge Weights Per Source (Grouped)
#' @description Applies a weight transformation \emph{within each source page's
#'   outgoing choice set}, rather than across one global vector. Link ranks and
#'   transition weights are normally meaningful relative to the other links on
#'   the \emph{same} source page: a "position 1" link on page A and a
#'   "position 1" link on page B should each be top-of-choice-set for their own
#'   source. A global rank (as computed by \code{\link{transform_weights}})
#'   conflates them; this helper computes the transform separately within each
#'   \code{by} group.
#'
#'   In addition to the transformed weight, it returns a normalized
#'   \code{transition_probability} that sums to 1 within each \code{by} group,
#'   so the per-source choice distribution can be inspected and validated before
#'   it reaches the solver (igraph re-normalizes edge strengths internally, but
#'   that normalization is not otherwise visible to the user).
#'
#' @param edge_list_df A data frame of edges. Must contain the column named by
#'   \code{by} and the column named by \code{value_col}.
#' @param value_col Character, the name of the column holding the raw numeric
#'   signal to transform (e.g. link positions, GA4 click counts).
#' @param by Character, the name of the grouping column defining each choice
#'   set. Default \code{"from"} (the source page). May name multiple columns to
#'   group by their combination.
#' @param method Character, the transformation strategy, passed through to
#'   \code{\link{transform_weights}}. One of \code{"none"},
#'   \code{"rank_linear"}, \code{"zipf"}, \code{"log"}, \code{"minmax"},
#'   \code{"percentile"}. Default \code{"zipf"}.
#' @param weight_col Character, the name of the output column to hold the
#'   transformed weight. Default \code{"weight"}.
#' @param prob_col Character, the name of the output column to hold the
#'   per-source normalized \code{transition_probability}. Default
#'   \code{"transition_probability"}.
#' @param ... Additional arguments forwarded to \code{\link{transform_weights}}
#'   (e.g. \code{alpha}, \code{offset}, \code{floor_value}, \code{descending}).
#'
#' @return The input data frame with two columns added (or overwritten):
#'   \code{weight_col} (the per-source transformed weight) and \code{prob_col}
#'   (the per-source transition probability, summing to 1 within each
#'   \code{by} group across non-\code{NA} weights). Row order is preserved.
#'
#' @details The transform is applied independently per group by calling
#'   \code{\link{transform_weights}} on each group's slice of
#'   \code{value_col} -- it reuses, rather than re-implements, the existing
#'   methods. \code{transition_probability} is then formed by dividing each
#'   group's transformed weights by their group sum. \code{NA} transformed
#'   weights (e.g. from \code{NA} inputs) are carried through and excluded from
#'   the probability total. A group whose transformed weights sum to zero (or
#'   are all \code{NA}) yields \code{NA} probabilities for that group, since no
#'   meaningful distribution can be formed.
#'
#' @seealso \code{\link{transform_weights}} for the single-vector (global)
#'   transform and the full description of each \code{method}.
#'
#' @export
#' @examples
#' # Two source pages, each with its own link positions (1 = top)
#' edges <- data.frame(
#'   from = c("A", "A", "A", "B", "B"),
#'   to = c("B", "C", "D", "C", "D"),
#'   position = c(1, 2, 3, 1, 2)
#' )
#'
#' # Zipf weights computed within each source's choice set
#' transform_edge_weights(edges, "position",
#'   method = "zipf", descending = FALSE
#' )
#'
#' # The transition_probability column sums to 1 within each `from`
transform_edge_weights <- function(edge_list_df,
                                   value_col,
                                   by = "from",
                                   method = c(
                                     "zipf", "none", "rank_linear",
                                     "log", "minmax", "percentile"
                                   ),
                                   weight_col = "weight",
                                   prob_col = "transition_probability",
                                   ...) {
  method <- match.arg(method)

  # --- Validation ---
  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  if (!is.character(value_col) || length(value_col) != 1) {
    stop("`value_col` must be a single column name.", call. = FALSE)
  }
  if (!is.character(by) || length(by) < 1) {
    stop("`by` must be one or more column names.", call. = FALSE)
  }
  if (!is.character(weight_col) || length(weight_col) != 1) {
    stop("`weight_col` must be a single column name.", call. = FALSE)
  }
  if (!is.character(prob_col) || length(prob_col) != 1) {
    stop("`prob_col` must be a single column name.", call. = FALSE)
  }

  missing_cols <- setdiff(c(value_col, by), names(edge_list_df))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in `edge_list_df`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(edge_list_df[[value_col]])) {
    stop("`value_col` (", value_col, ") must be numeric.", call. = FALSE)
  }

  n <- nrow(edge_list_df)
  weight <- rep(NA_real_, n)
  prob <- rep(NA_real_, n)

  if (n > 0) {
    # Build a per-row group key from the `by` column(s), preserving order.
    group_key <- interaction(edge_list_df[by], drop = TRUE, lex.order = TRUE)
    row_idx <- split(seq_len(n), group_key)

    for (idx in row_idx) {
      vals <- edge_list_df[[value_col]][idx]
      w <- transform_weights(vals, method = method, ...)
      weight[idx] <- w

      total <- sum(w, na.rm = TRUE)
      if (is.finite(total) && total > 0) {
        prob[idx] <- w / total
      }
      # else: leave NA (degenerate / all-NA / zero-sum choice set)
    }
  }

  edge_list_df[[weight_col]] <- weight
  edge_list_df[[prob_col]] <- prob
  edge_list_df
}
