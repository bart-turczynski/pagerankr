#' @title Transition-construction audit / provenance object
#' @name transition_audit
#' @description Builds a stable, documented audit / provenance record describing
#'   what happened to the edges and weights as [pagerank()] turned a raw edge
#'   list into the transition graph it scored. It is the backbone of
#'   reproducibility and of downstream diagnostics: it carries the row/edge
#'   counts, behavioral-weight coverage, normalization totals, the data that was
#'   dropped along the way (rows lost to NA / deduplication / self-loop removal,
#'   and authority-prior URLs that never folded onto a vertex), and the relevant
#'   [pagerank()] configuration.
#'
#' @details
#' ## Structure and contract
#'
#' The object is an S3 list with class `"transition_audit"` (a list was chosen
#' over a bare list so that it prints a human-readable summary while remaining a
#' plain, inspectable `list` for programmatic access — `audit$counts$n_edges`
#' works as expected, mirroring the existing [audit_redirects()] /
#' [audit_canonicals()] objects in this package). The documented top-level
#' fields are **stable**; callers may rely on them being present.
#'
#' \describe{
#'   \item{counts}{A list of integer counts: `n_input_rows` (rows in the raw
#'     `edge_list_df`), `n_edges` (directed edges remaining after URL folding,
#'     deduplication and self-loop handling — i.e. the edges actually scored),
#'     and `n_vertices` (vertices in the returned result).}
#'   \item{coverage}{A list describing behavioral-weight coverage: `weighted`
#'     (logical, whether a `weight_col` was in effect), `weight_col` (its name
#'     or `NULL`), `n_edges_weighted` (edges carrying a finite, positive
#'     weight), and `coverage` (the fraction `n_edges_weighted / n_edges`, or
#'     `NA_real_` when there are no edges / no weighting).}
#'   \item{normalization}{A list of normalization totals: `pagerank_total` (sum
#'     of the returned PageRank scores; `< 1` when mass evaporated via nofollow,
#'     vanished robots-blocked pages, etc.).}
#'   \item{dropped}{A list accounting for data removed during construction:
#'     `n_rows_na` (input rows dropped because `from`/`to` was `NA`),
#'     `n_rows_duplicate` (rows collapsed by edge deduplication),
#'     `n_self_loops` (self-loop edges dropped when `self_loops = "drop"`),
#'     `n_rows_collapsed` (total input rows that did not survive as distinct
#'     scored edges = `n_input_rows - n_edges`), `n_prior_unmatched` (authority
#'     prior URLs that did not fold onto any vertex; `NA_integer_` when no
#'     `prior_df` was supplied), and `n_robots_blocked` (URLs treated as
#'     robots.txt-blocked).}
#'   \item{config}{A list of the [pagerank()] arguments that materially shape
#'     the transition graph: `self_loops`, `drop_isolates_flag`, `reverse`,
#'     `weight_col`, `nofollow_col`, `nofollow_action`, `robots_blocked_action`,
#'     `prior_alpha`, `prior_transform`, `prior_inject_unmatched`, and the
#'     logical flags `has_redirects` / `has_canonicals` (whether that signal
#'     *materially* folded an edge — an effective no-op such as a self-canonical
#'     reads `FALSE`), `has_indexability`, and `has_prior`.}
#'   \item{mass}{A list of page-mass accounting fields. **Stubbed here as `NULL`
#'     placeholders** — they are populated by the mass-accounting feature
#'     (B2); the keys (`reported`, `sink`, `hidden`, `total`) are reserved so
#'     that work can fill them without changing this object's shape.}
#' }
#'
#' The constructor [new_transition_audit()] is internal plumbing for
#' [pagerank()]; the object is normally obtained via
#' `attr(result, "transition_audit")` (see [pagerank()]).
#'
#' @seealso [pagerank()], [audit_redirects()], [audit_canonicals()]
NULL

#' Construct a transition_audit object
#'
#' Internal constructor used by [pagerank()] to assemble the audit record from
#' counts gathered along the aggregation / validation / cleaning path. Every
#' argument has a default so that partially-known states (e.g. an empty edge
#' list) still produce a well-formed object with the documented fields present.
#'
#' @param n_input_rows Integer, rows in the raw `edge_list_df`.
#' @param n_edges Integer, directed edges that survived folding, dedup and
#'   self-loop handling (the edges actually scored).
#' @param n_vertices Integer, vertices in the returned result.
#' @param weighted Logical, whether an edge `weight_col` was in effect.
#' @param weight_col Character or `NULL`, the weight column name.
#' @param n_edges_weighted Integer, edges carrying a finite positive weight.
#' @param n_rows_na Integer, input rows dropped due to `NA` endpoints.
#' @param n_rows_duplicate Integer, rows collapsed by deduplication.
#' @param n_self_loops Integer, self-loop edges dropped.
#' @param n_prior_unmatched Integer or `NA`, prior URLs that did not fold onto a
#'   vertex.
#' @param n_robots_blocked Integer, URLs treated as robots.txt-blocked.
#' @param pagerank_total Numeric, sum of the returned PageRank scores.
#' @param config A named list of the relevant [pagerank()] configuration.
#' @return An object of class `"transition_audit"` (see [transition_audit]).
#' @noRd
new_transition_audit <- function(n_input_rows = 0L,
                                 n_edges = 0L,
                                 n_vertices = 0L,
                                 weighted = FALSE,
                                 weight_col = NULL,
                                 n_edges_weighted = 0L,
                                 n_rows_na = 0L,
                                 n_rows_duplicate = 0L,
                                 n_self_loops = 0L,
                                 n_prior_unmatched = NA_integer_,
                                 n_robots_blocked = 0L,
                                 pagerank_total = NA_real_,
                                 config = list()) {
  n_input_rows <- as.integer(n_input_rows)
  n_edges <- as.integer(n_edges)

  coverage_frac <- if (isTRUE(weighted) && n_edges > 0L) {
    as.numeric(n_edges_weighted) / as.numeric(n_edges)
  } else {
    NA_real_
  }

  audit <- list(
    counts = list(
      n_input_rows = n_input_rows,
      n_edges = n_edges,
      n_vertices = as.integer(n_vertices)
    ),
    coverage = list(
      weighted = isTRUE(weighted),
      weight_col = weight_col,
      n_edges_weighted = as.integer(n_edges_weighted),
      coverage = coverage_frac
    ),
    normalization = list(
      pagerank_total = as.numeric(pagerank_total)
    ),
    dropped = list(
      n_rows_na = as.integer(n_rows_na),
      n_rows_duplicate = as.integer(n_rows_duplicate),
      n_self_loops = as.integer(n_self_loops),
      n_rows_collapsed = n_input_rows - n_edges,
      n_prior_unmatched = if (is.na(n_prior_unmatched)) {
        NA_integer_
      } else {
        as.integer(n_prior_unmatched)
      },
      n_robots_blocked = as.integer(n_robots_blocked)
    ),
    config = config,
    # --- Mass accounting (B2 / PAGE-mqsxrcdz): stubbed placeholders. ---
    # Reserved keys so mass accounting can fill them without reshaping this
    # object. Do not populate here.
    mass = list(
      reported = NULL,
      sink = NULL,
      hidden = NULL,
      total = NULL
    )
  )

  class(audit) <- "transition_audit"
  audit
}

#' Print a transition_audit object
#'
#' @param x A `transition_audit` object.
#' @param ... Unused; for S3 compatibility.
#' @return `x`, invisibly.
#' @export
print.transition_audit <- function(x, ...) {
  cat("=== Transition Construction Audit ===\n\n")

  cat("Counts\n")
  cat("  Input rows:        ", x$counts$n_input_rows, "\n")
  cat("  Edges (scored):    ", x$counts$n_edges, "\n")
  cat("  Vertices (result): ", x$counts$n_vertices, "\n")

  cat("\nDropped / collapsed\n")
  cat("  Rows w/ NA endpoint:", x$dropped$n_rows_na, "\n")
  cat("  Duplicate rows:     ", x$dropped$n_rows_duplicate, "\n")
  cat("  Self-loops dropped: ", x$dropped$n_self_loops, "\n")
  cat("  Rows collapsed:     ", x$dropped$n_rows_collapsed, "\n")
  if (!is.na(x$dropped$n_prior_unmatched)) {
    cat("  Prior URLs unmatched:", x$dropped$n_prior_unmatched, "\n")
  }
  if (x$dropped$n_robots_blocked > 0L) {
    cat("  Robots-blocked URLs:", x$dropped$n_robots_blocked, "\n")
  }

  cat("\nBehavioral coverage\n")
  if (isTRUE(x$coverage$weighted)) {
    cat("  Weight column:      ", x$coverage$weight_col, "\n")
    cat("  Weighted edges:     ", x$coverage$n_edges_weighted, "\n")
    cat(
      "  Coverage:           ",
      if (is.na(x$coverage$coverage)) {
        "NA"
      } else {
        pct <- formatC(100 * x$coverage$coverage, digits = 1, format = "f")
        paste0(pct, "%")
      },
      "\n"
    )
  } else {
    cat("  (unweighted / all edges equal)\n")
  }

  cat("\nNormalization\n")
  cat(
    "  PageRank total:     ",
    if (is.na(x$normalization$pagerank_total)) {
      "NA"
    } else {
      formatC(x$normalization$pagerank_total, digits = 6, format = "f")
    },
    "\n"
  )

  # Mass accounting is stubbed (B2); only report once populated.
  if (!is.null(x$mass$total)) {
    cat("\nPage mass\n")
    cat("  Reported:           ", x$mass$reported, "\n")
    cat("  Sink:               ", x$mass$sink, "\n")
    cat("  Hidden:             ", x$mass$hidden, "\n")
    cat("  Total:              ", x$mass$total, "\n")
  }

  invisible(x)
}

# Register the print method at load time. The package's NAMESPACE (regenerated
# from the @export tag above) is the canonical registration; this `.onLoad`
# guarantees S3 dispatch also works under `pkgload::load_all()` during
# development, before document() has refreshed NAMESPACE.
.onLoad <- function(libname, pkgname) {
  registerS3method(
    "print", "transition_audit", print.transition_audit,
    envir = asNamespace(pkgname)
  )
}
