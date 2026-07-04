#' @title Transition-construction audit / provenance object
#' @name transition_audit
#' @description Builds a stable, documented audit / provenance record describing
#'   what happened to the edges and weights as [pagerank()] turned a raw edge
#'   list into the transition graph it scored. It is the backbone of
#'   reproducibility and of downstream diagnostics: it carries the row/edge
#'   counts, behavioral-weight coverage, normalization totals, the data that was
#'   dropped along the way (rows lost to NA / deduplication / self-loop removal,
#'   and authority-prior URLs that never folded onto a vertex), and the relevant
#'   [pagerank()] configuration. It also records the duplicate-edge policy used
#'   to build transitions, so callers can distinguish the default
#'   destination-level surfer from opt-in aggregate / link-slot models.
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
#'   \item{duplicates}{A list describing duplicate-edge handling:
#'     `policy` (the `duplicate_edge_policy` passed to [pagerank()]),
#'     `n_duplicate_rows` (post-fold duplicate input rows), `instance_count_col`
#'     (the internal audit column used by `"count_instances"`, or `NULL`), and
#'     `n_duplicate_instances` (the number of duplicate link instances folded
#'     into transition weights), and `duplicate_edges` (a compact data frame of
#'     counted edges with more than one link instance, or `NULL`).}
#'   \item{config}{A list of the [pagerank()] arguments that materially shape
#'     the transition graph: `self_loops`, `drop_isolates_flag`, `reverse`,
#'     `weight_col`, `nofollow_col`, `nofollow_action`, `robots_blocked_action`,
#'     `prior_alpha`, `prior_transform`, `prior_inject_unmatched`, and the
#'     logical flags `has_redirects` / `has_canonicals` (whether that signal
#'     *materially* folded an edge — an effective no-op such as a self-canonical
#'     reads `FALSE`), `has_indexability`, and `has_prior`.}
#'   \item{mass}{A list decomposing the internal stationary vector (which
#'     always sums to 1) into its accounted-for components: `reported` (the
#'     mass on returned, visible pages — equals the summed result scores),
#'     `sink` (the **evaporated mass**: authority sent to the synthetic
#'     nofollow-evaporation sink under `nofollow_action = "evaporate"`),
#'     `hidden` (the **hidden mass**: authority trapped on hidden /
#'     robots-blocked nodes removed under `robots_blocked_action = "vanish"`),
#'     and `total` (their sum, which reconciles to 1 by construction). These
#'     are the precise components of the deficit between the reported scores
#'     and 1 — it is evaporated and hidden mass, not undifferentiated
#'     "leakage". Each is `NULL` when the stationary vector is undefined (e.g.
#'     an empty graph).}
#'   \item{fold}{A list recording how **out-of-scope folds** were handled — a
#'     composed fold-map entry whose *target* (the representative a crawled
#'     source folds onto) is not itself a crawled node, which silently invents
#'     a phantom vertex. `policy` (the `out_of_scope_fold` argument,
#'     `"relabel"` or `"keep"`), `n_out_of_scope` (count of such entries),
#'     `applied` (logical: `TRUE` when they were relabeled / folded through,
#'     `FALSE` when skipped / kept as crawled), and `out_of_scope` (a data
#'     frame of the offending `source` / `target` / `signal` rows, or `NULL`
#'     when there were none). Recorded regardless of policy.}
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
#' @param duplicate_edge_policy Character, the duplicate-edge policy used by
#'   [pagerank()].
#' @param instance_count_col Character or `NULL`, internal count column used by
#'   `duplicate_edge_policy = "count_instances"`.
#' @param n_duplicate_instances Integer, duplicate link instances folded into
#'   transition weights.
#' @param duplicate_edges Data frame or `NULL`, compact counted-edge audit rows.
#' @param n_rows_na Integer, input rows dropped due to `NA` endpoints.
#' @param n_rows_duplicate Integer, rows collapsed by deduplication.
#' @param n_self_loops Integer, self-loop edges dropped.
#' @param n_prior_unmatched Integer or `NA`, prior URLs that did not fold onto a
#'   vertex.
#' @param n_robots_blocked Integer, URLs treated as robots.txt-blocked.
#' @param pagerank_total Numeric, sum of the returned PageRank scores.
#' @param mass_reported Numeric, stationary mass on returned/visible pages
#'   (typically equal to `pagerank_total`).
#' @param mass_evaporated Numeric, stationary mass sent to the nofollow
#'   evaporation sink (authority wasted on nofollowed outlinks). `0` when no
#'   evaporation occurred.
#' @param mass_hidden Numeric, stationary mass trapped on hidden / vanished
#'   robots-blocked nodes that were removed from the results. `0` when none.
#' @param out_of_scope_fold Character, the `out_of_scope_fold` policy used
#'   (`"relabel"` or `"keep"`).
#' @param n_out_of_scope_folds Integer, count of composed fold-map entries whose
#'   target was not a crawled node.
#' @param out_of_scope_folds_applied Logical, `TRUE` when the out-of-scope folds
#'   were relabeled (applied), `FALSE` when they were skipped (kept).
#' @param out_of_scope_fold_list Data frame or `NULL`, the out-of-scope folds
#'   as `source` / `target` / `signal` rows.
#' @param config A named list of the relevant [pagerank()] configuration.
#' @return An object of class `"transition_audit"` (see [transition_audit]).
#' @noRd
#' @export
new_transition_audit <- function(n_input_rows = 0L,
                                 n_edges = 0L,
                                 n_vertices = 0L,
                                 weighted = FALSE,
                                 weight_col = NULL,
                                 n_edges_weighted = 0L,
                                 duplicate_edge_policy = "collapse",
                                 instance_count_col = NULL,
                                 n_duplicate_instances = 0L,
                                 duplicate_edges = NULL,
                                 n_rows_na = 0L,
                                 n_rows_duplicate = 0L,
                                 n_self_loops = 0L,
                                 n_prior_unmatched = NA_integer_,
                                 n_robots_blocked = 0L,
                                 pagerank_total = NA_real_,
                                 mass_reported = NA_real_,
                                 mass_evaporated = NA_real_,
                                 mass_hidden = NA_real_,
                                 out_of_scope_fold = "relabel",
                                 n_out_of_scope_folds = 0L,
                                 out_of_scope_folds_applied = TRUE,
                                 out_of_scope_fold_list = NULL,
                                 config = list()) {
  n_input_rows <- as.integer(n_input_rows)
  n_edges <- as.integer(n_edges)

  coverage_frac <- if (isTRUE(weighted) && n_edges > 0L) {
    as.numeric(n_edges_weighted) / as.numeric(n_edges)
  } else {
    NA_real_
  }

  # Page-mass accounting. The internal stationary vector sums to 1; we split
  # it into reported (visible) mass, evaporated (nofollow-sink) mass, and
  # hidden (robots-blocked vanish) mass, whose total reconciles to 1. When the
  # stationary vector is undefined (empty graph -> reported is NA) we leave the
  # reserved NULL stubs in place rather than fabricating a decomposition.
  mass <- if (is.na(mass_reported)) {
    list(reported = NULL, sink = NULL, hidden = NULL, total = NULL)
  } else {
    reported <- as.numeric(mass_reported)
    sink <- if (is.na(mass_evaporated)) 0 else as.numeric(mass_evaporated)
    hidden <- if (is.na(mass_hidden)) 0 else as.numeric(mass_hidden)
    list(
      reported = reported,
      sink = sink,
      hidden = hidden,
      total = reported + sink + hidden
    )
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
    duplicates = list(
      policy = duplicate_edge_policy,
      n_duplicate_rows = as.integer(n_rows_duplicate),
      instance_count_col = instance_count_col,
      n_duplicate_instances = as.integer(n_duplicate_instances),
      duplicate_edges = duplicate_edges
    ),
    config = config,
    # --- Mass accounting (B2 / PAGE-mqsxrcdz). ---
    # Decomposes the internal stationary vector (which always sums to 1) into
    # its accounted-for components. `reported` is the visible page mass;
    # `sink` is the EVAPORATED mass (authority sent to the nofollow sink);
    # `hidden` is the mass trapped on hidden/vanished robots-blocked nodes;
    # `total` is their sum, which reconciles to 1 by construction. Left as the
    # reserved NULL stubs when the stationary vector is undefined (empty graph).
    mass = mass,
    # --- Out-of-scope fold accounting (SF-scope / PAGE-ttlaxjkw). ---
    # Records how composed fold-map entries whose TARGET is not a crawled node
    # were handled. `policy` is the `out_of_scope_fold` argument;
    # `n_out_of_scope` counts such entries; `applied` is TRUE when they were
    # relabeled (folded through) and FALSE when they were skipped (kept as
    # crawled); `out_of_scope` is a data frame of the offending
    # source/target/signal rows, or NULL when there were none.
    fold = list(
      policy = out_of_scope_fold,
      n_out_of_scope = as.integer(n_out_of_scope_folds),
      applied = isTRUE(out_of_scope_folds_applied),
      out_of_scope = out_of_scope_fold_list
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

  if (!is.null(x$duplicates)) {
    cat("\nDuplicate edge policy\n")
    cat("  Policy:             ", x$duplicates$policy, "\n")
    cat("  Duplicate rows:     ", x$duplicates$n_duplicate_rows, "\n")
    if (!is.null(x$duplicates$instance_count_col)) {
      cat("  Instance count col: ", x$duplicates$instance_count_col, "\n")
    }
    if (is.data.frame(x$duplicates$duplicate_edges)) {
      cat("  Counted dup edges:  ", nrow(x$duplicates$duplicate_edges), "\n")
    }
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

  # Mass accounting: only report once the stationary vector is defined.
  if (!is.null(x$mass$total)) {
    fmt_mass <- function(v) formatC(v, digits = 6, format = "f")
    cat("\nPage mass (stationary vector sums to 1)\n")
    cat("  Reported (visible): ", fmt_mass(x$mass$reported), "\n")
    cat("  Evaporated (sink):  ", fmt_mass(x$mass$sink), "\n")
    cat("  Hidden (robots):    ", fmt_mass(x$mass$hidden), "\n")
    cat("  Total:              ", fmt_mass(x$mass$total), "\n")
  }

  # Out-of-scope fold accounting: reported regardless of policy.
  if (!is.null(x$fold)) {
    cat("\nOut-of-scope folds (target not a crawled node)\n")
    cat("  Policy:             ", x$fold$policy, "\n")
    cat("  Out-of-scope folds: ", x$fold$n_out_of_scope, "\n")
    if (x$fold$n_out_of_scope > 0L) {
      cat(
        "  Action:             ",
        if (isTRUE(x$fold$applied)) "relabeled (applied)" else "skipped (kept)",
        "\n"
      )
    }
  }

  invisible(x)
}
