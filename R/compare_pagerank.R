#' @title Compare Two PageRank Results
#' @description Performs a full outer join on two PageRank result data frames
#'   and computes deltas, percentage changes, and rank changes for each node.
#'   Summary statistics are attached as an attribute.
#'
#' @param pr_a A data frame of PageRank results (model A / baseline).
#' @param pr_b A data frame of PageRank results (model B / comparison).
#' @param node_col Name of the node column present in both data frames.
#'   Default `"node_name"`.
#' @param pr_col Name of the PageRank value column present in both data frames.
#'   Default `"pagerank"`.
#' @param label_a Short label for model A (used in output column names).
#'   Default `"a"`.
#' @param label_b Short label for model B (used in output column names).
#'   Default `"b"`.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{node_name}{Node identifier}
#'     \item{pagerank_a, pagerank_b}{PageRank scores from each model (`NA` when
#'       the node is absent from that model)}
#'     \item{delta}{`pagerank_b - pagerank_a`}
#'     \item{pct_change}{Percentage change from a to b (`NA` when a is `NA` or
#'       0)}
#'     \item{rank_a, rank_b}{Ordinal rank (1 = highest PageRank) within each
#'       model (`NA` when the node is absent)}
#'     \item{rank_delta}{`rank_a - rank_b` (positive = improved in b)}
#'   }
#'
#'   A `"summary"` attribute (named list) is attached with:
#'   \describe{
#'     \item{spearman_rho}{Spearman rank correlation on common nodes}
#'     \item{mean_abs_delta}{Mean of absolute delta on common nodes}
#'     \item{nodes_gained}{Count of nodes in b but not a}
#'     \item{nodes_lost}{Count of nodes in a but not b}
#'   }
#'
#' @export
#' @examples
#' pr_a <- data.frame(
#'   node_name = c("A", "B", "C"),
#'   pagerank = c(0.5, 0.3, 0.2)
#' )
#' pr_b <- data.frame(
#'   node_name = c("A", "B", "D"),
#'   pagerank = c(0.4, 0.35, 0.25)
#' )
#' result <- compare_pagerank(pr_a, pr_b)
#' print(result)
#' attr(result, "summary")
compare_pagerank <- function(pr_a, pr_b,
                             node_col = "node_name",
                             pr_col = "pagerank",
                             label_a = "a",
                             label_b = "b") {
  # --- Validation ---
  .validate_compare_pagerank_frames(pr_a, pr_b, node_col, pr_col)

  # --- Extract and Rank ---
  a <- data.frame(
    node = as.character(pr_a[[node_col]]),
    pr_a = as.numeric(pr_a[[pr_col]])
  )
  b <- data.frame(
    node = as.character(pr_b[[node_col]]),
    pr_b = as.numeric(pr_b[[pr_col]])
  )

  # Ordinal rank: 1 = highest PR
  a$rank_a <- rank(-a$pr_a, ties.method = "min")
  b$rank_b <- rank(-b$pr_b, ties.method = "min")

  # --- Full Outer Join ---
  merged <- merge(a, b, by = "node", all = TRUE)

  # --- Compute Deltas ---
  merged$delta <- merged$pr_b - merged$pr_a
  merged$pct_change <- ifelse(
    is.na(merged$pr_a) | merged$pr_a == 0,
    NA_real_,
    (merged$pr_b - merged$pr_a) / merged$pr_a * 100
  )
  # rank_delta: positive = improved in b (lower rank number in b)
  merged$rank_delta <- merged$rank_a - merged$rank_b

  # --- Rename Columns + sort by absolute delta descending ---
  result <- .compare_pagerank_finalize(
    merged, node_col, pr_col, label_a, label_b
  )

  # --- Summary Statistics ---
  attr(result, "summary") <- .compare_pagerank_summary(merged)

  result
}

#' Validate the two input data frames and required columns for compare_pagerank
#' @keywords internal
#' @noRd
.validate_compare_pagerank_frames <- function(pr_a, pr_b, node_col, pr_col) {
  if (!is.data.frame(pr_a)) stop("`pr_a` must be a data frame.", call. = FALSE)
  if (!is.data.frame(pr_b)) stop("`pr_b` must be a data frame.", call. = FALSE)
  if (!(node_col %in% names(pr_a))) {
    stop("Column '", node_col, "' not found in `pr_a`.", call. = FALSE)
  }
  if (!(node_col %in% names(pr_b))) {
    stop("Column '", node_col, "' not found in `pr_b`.", call. = FALSE)
  }
  if (!(pr_col %in% names(pr_a))) {
    stop("Column '", pr_col, "' not found in `pr_a`.", call. = FALSE)
  }
  if (!(pr_col %in% names(pr_b))) {
    stop("Column '", pr_col, "' not found in `pr_b`.", call. = FALSE)
  }
  invisible(NULL)
}

#' Rename the merged comparison columns and sort by absolute delta descending
#' @keywords internal
#' @noRd
.compare_pagerank_finalize <- function(merged, node_col, pr_col,
                                       label_a, label_b) {
  pr_col_a <- paste0(pr_col, "_", label_a)
  pr_col_b <- paste0(pr_col, "_", label_b)
  rank_col_a <- paste0("rank_", label_a)
  rank_col_b <- paste0("rank_", label_b)

  result <- data.frame(
    node = merged$node,
    pr_a_val = merged$pr_a,
    pr_b_val = merged$pr_b,
    delta = merged$delta,
    pct_change = merged$pct_change,
    rank_a_val = merged$rank_a,
    rank_b_val = merged$rank_b,
    rank_delta = merged$rank_delta
  )
  names(result) <- c(
    node_col, pr_col_a, pr_col_b, "delta", "pct_change",
    rank_col_a, rank_col_b, "rank_delta"
  )

  abs_delta <- abs(result$delta)
  abs_delta[is.na(abs_delta)] <- -Inf
  result <- result[order(abs_delta, decreasing = TRUE), , drop = FALSE]
  row.names(result) <- NULL
  result
}

#' Compute the comparison summary statistics on the common (matched) nodes
#' @keywords internal
#' @noRd
.compare_pagerank_summary <- function(merged) {
  common_mask <- !is.na(merged$pr_a) & !is.na(merged$pr_b)
  common_a_ranks <- merged$rank_a[common_mask]
  common_b_ranks <- merged$rank_b[common_mask]

  spearman_rho <- if (sum(common_mask) >= 3) {
    # cor() warns when sd is zero (e.g. all ranks identical); return NA directly
    if (stats::sd(common_a_ranks) == 0 || stats::sd(common_b_ranks) == 0) {
      NA_real_
    } else {
      stats::cor(common_a_ranks, common_b_ranks, method = "spearman")
    }
  } else {
    NA_real_
  }
  mean_abs_delta <- if (sum(common_mask) > 0) {
    mean(abs(merged$delta[common_mask]), na.rm = TRUE)
  } else {
    NA_real_
  }
  nodes_gained <- sum(is.na(merged$pr_a) & !is.na(merged$pr_b))
  nodes_lost <- sum(!is.na(merged$pr_a) & is.na(merged$pr_b))

  list(
    spearman_rho = spearman_rho,
    mean_abs_delta = mean_abs_delta,
    nodes_gained = nodes_gained,
    nodes_lost = nodes_lost
  )
}
