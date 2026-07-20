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
#'     the transition graph. `preset` records the *provenance* of the rest:
#'     the name of the [pr_preset()] bundle the caller asked for (e.g.
#'     `"declared"`), `"custom"` for a hand-rolled bundle, or `NULL` when no
#'     preset was used — so a run made as a named view stays distinguishable
#'     from the same arguments typed out by hand. `placement` records
#'     placement-aware weighting when it was used (`placement_col`,
#'     `accepted_placements`, `placement_weights`, and `n_rows_dropped`, the
#'     number of edge rows the placement filter removed), or `NULL` when it was
#'     not — so a downweighted edge can be explained by the region it sits in
#'     rather than only by the opaque weight column it produced. The other
#'     fields are the
#'     resolved configuration itself: `self_loops`, `drop_isolates_flag`,
#'     `reverse`,
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
#'     `leaked` (the **leaked mass**: authority sent to the synthetic leak sink
#'     under `out_of_scope_fold = "leak"`, i.e. equity that flowed into
#'     out-of-scope-folded sources and left the measured graph — `0` when no
#'     leak occurred), `hidden` (the **hidden mass**: authority trapped on
#'     hidden / robots-blocked nodes removed under
#'     `robots_blocked_action = "vanish"`), and `total` (their sum, which
#'     reconciles to 1 by construction). These are the precise components of
#'     the deficit between the reported scores and 1 — it is evaporated, leaked
#'     and hidden mass, not undifferentiated "leakage". Each is `NULL` when the
#'     stationary vector is undefined (e.g. an empty graph).}
#'   \item{fold}{A list recording how **out-of-scope folds** were handled — a
#'     composed fold-map entry whose *target* (the representative a crawled
#'     source folds onto) is not itself a crawled node, which silently invents
#'     a phantom vertex. `policy` (the `out_of_scope_fold` argument,
#'     `"relabel"`, `"keep"` or `"leak"`), `n_out_of_scope` (count of such
#'     entries), `applied` (logical: `TRUE` when they were acted upon —
#'     relabeled / folded through under `"relabel"`, or routed to the leak sink
#'     under `"leak"` — and `FALSE` when skipped / kept as crawled under
#'     `"keep"`; combine with `policy` to distinguish relabel from leak), and
#'     `out_of_scope` (a data frame of the offending `source` / `target` /
#'     `signal` rows, or `NULL` when there were none), and `collisions` (a data
#'     frame of **fold-target collisions** — uncrawled URLs that a fold
#'     relabeled a crawled source onto while they were ALSO independently
#'     linked, so the two silently merge into one vertex and the crawled page
#'     absorbs the inbound link equity of that uncrawled URL; columns `target`,
#'     `n_independent_refs` and the folded `source`(s) — or `NULL` when none). A
#'     collision triggers a `warning()` naming the merged URL(s). This
#'     diagnostic requires crawl-URL knowledge to distinguish an uncrawled fold
#'     target from a genuinely crawled leaf page, so it is only computed when an
#'     `indexability_df` is supplied to [pagerank()]; without it, `collisions`
#'     is `NULL`. Recorded regardless of `out_of_scope_fold` policy.}
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
#' @param mass_leaked Numeric, stationary mass sent to the leak sink under
#'   `out_of_scope_fold = "leak"` (authority that flowed into
#'   out-of-scope-folded sources, treated like an external redirect). `0` when
#'   no leak occurred.
#' @param mass_hidden Numeric, stationary mass trapped on hidden / vanished
#'   robots-blocked nodes that were removed from the results. `0` when none.
#' @param out_of_scope_fold Character, the `out_of_scope_fold` policy used
#'   (`"relabel"`, `"keep"` or `"leak"`).
#' @param n_out_of_scope_folds Integer, count of composed fold-map entries whose
#'   target was not a crawled node.
#' @param out_of_scope_folds_applied Logical, `TRUE` when the out-of-scope folds
#'   were acted upon (relabeled under `"relabel"`, or routed to the leak sink
#'   under `"leak"`), `FALSE` when they were skipped (kept) under `"keep"`.
#' @param out_of_scope_fold_list Data frame or `NULL`, the out-of-scope folds
#'   as `source` / `target` / `signal` rows.
#' @param fold_collisions Data frame or `NULL`, fold-target collisions detected
#'   on the pre-fold edge list: rows of `target` / `n_independent_refs` /
#'   `source` for uncrawled URLs that a fold relabeled a crawled source onto
#'   while they were also independently linked. `NULL` when no `indexability_df`
#'   crawl-URL set was available to detect them.
#' @param config A named list of the relevant [pagerank()] configuration.
#' @return An object of class `"transition_audit"` (see [transition_audit]).
#' @keywords internal
#' @examples
#' # Low-level plumbing: normally you obtain a transition_audit via
#' # attr(pagerank(...), "transition_audit") rather than by hand. Every
#' # argument defaults, so a bare call yields a well-formed, empty-graph object.
#' audit <- new_transition_audit()
#' audit$counts
#'
#' # Populate a few fields to describe a small scored graph.
#' audit <- new_transition_audit(
#'   n_input_rows = 4L,
#'   n_edges = 3L,
#'   n_vertices = 3L,
#'   n_rows_duplicate = 1L,
#'   pagerank_total = 1,
#'   mass_reported = 1
#' )
#' audit$counts$n_edges
#' audit$dropped$n_rows_collapsed
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
                                 mass_leaked = NA_real_,
                                 mass_hidden = NA_real_,
                                 out_of_scope_fold = "relabel",
                                 n_out_of_scope_folds = 0L,
                                 out_of_scope_folds_applied = TRUE,
                                 out_of_scope_fold_list = NULL,
                                 fold_collisions = NULL,
                                 config = list()) {
  n_input_rows <- as.integer(n_input_rows)
  n_edges <- as.integer(n_edges)

  coverage_frac <- .ta_coverage_frac(weighted, n_edges_weighted, n_edges)
  mass <- .ta_mass(mass_reported, mass_evaporated, mass_leaked, mass_hidden)

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
    dropped = .ta_dropped(
      n_rows_na, n_rows_duplicate, n_self_loops, n_input_rows, n_edges,
      n_prior_unmatched, n_robots_blocked
    ),
    duplicates = list(
      policy = duplicate_edge_policy,
      n_duplicate_rows = as.integer(n_rows_duplicate),
      instance_count_col = instance_count_col,
      n_duplicate_instances = as.integer(n_duplicate_instances),
      duplicate_edges = duplicate_edges
    ),
    config = config,
    # Mass accounting (B2 / PAGE-mqsxrcdz; leaked: PAGE-xkmqsbqv). See .ta_mass.
    mass = mass,
    # Out-of-scope fold accounting (SF-scope / PAGE-ttlaxjkw,
    # collisions PAGE-rjrduvmy). See .ta_fold.
    fold = .ta_fold(
      out_of_scope_fold, n_out_of_scope_folds, out_of_scope_folds_applied,
      out_of_scope_fold_list, fold_collisions
    )
  )

  class(audit) <- "transition_audit"
  audit
}

#' Compute behavioral-weight coverage fraction for a transition_audit
#' @keywords internal
#' @noRd
.ta_coverage_frac <- function(weighted, n_edges_weighted, n_edges) {
  if (isTRUE(weighted) && n_edges > 0L) {
    as.numeric(n_edges_weighted) / as.numeric(n_edges)
  } else {
    NA_real_
  }
}

#' Decompose the stationary vector into reported/sink/leaked/hidden page mass
#'
#' The internal stationary vector sums to 1; we split it into reported
#' (visible) mass, evaporated (nofollow-sink) mass, leaked (leak-sink) mass,
#' and hidden (robots-blocked vanish) mass, whose total reconciles to 1. When
#' the stationary vector is undefined (empty graph -> reported is NA) we leave
#' the reserved NULL stubs in place rather than fabricating a decomposition.
#' @keywords internal
#' @noRd
.ta_mass <- function(mass_reported, mass_evaporated, mass_leaked, mass_hidden) {
  if (is.na(mass_reported)) {
    return(list(
      reported = NULL, sink = NULL, leaked = NULL, hidden = NULL, total = NULL
    ))
  }
  reported <- as.numeric(mass_reported)
  sink <- if (is.na(mass_evaporated)) 0 else as.numeric(mass_evaporated)
  leaked <- if (is.na(mass_leaked)) 0 else as.numeric(mass_leaked)
  hidden <- if (is.na(mass_hidden)) 0 else as.numeric(mass_hidden)
  list(
    reported = reported,
    sink = sink,
    leaked = leaked,
    hidden = hidden,
    total = reported + sink + leaked + hidden
  )
}

#' Build the `dropped` accounting sub-list for a transition_audit
#' @keywords internal
#' @noRd
.ta_dropped <- function(n_rows_na, n_rows_duplicate, n_self_loops,
                        n_input_rows, n_edges, n_prior_unmatched,
                        n_robots_blocked) {
  list(
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
  )
}

#' Build the out-of-scope `fold` accounting sub-list for a transition_audit
#'
#' Records how composed fold-map entries whose TARGET is not a crawled node
#' were handled. `policy` is the `out_of_scope_fold` argument; `n_out_of_scope`
#' counts such entries; `applied` is TRUE when they were relabeled (folded
#' through) and FALSE when they were skipped (kept as crawled); `out_of_scope`
#' is a data frame of the offending source/target/signal rows, or NULL when
#' there were none. `collisions` records fold-target collisions: uncrawled URLs
#' a fold relabeled a crawled source onto while they were also independently
#' linked, silently merging inbound equity. A data frame of
#' target/n_independent_refs/source rows, or NULL when there were none.
#' @keywords internal
#' @noRd
.ta_fold <- function(out_of_scope_fold, n_out_of_scope_folds,
                     out_of_scope_folds_applied, out_of_scope_fold_list,
                     fold_collisions) {
  list(
    policy = out_of_scope_fold,
    n_out_of_scope = as.integer(n_out_of_scope_folds),
    applied = isTRUE(out_of_scope_folds_applied),
    out_of_scope = out_of_scope_fold_list,
    collisions = fold_collisions
  )
}

#' Print the preset provenance line of a transition_audit
#'
#' Printed only when a preset was actually used, so the default output of a
#' plain `pagerank()` run is unchanged.
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_preset <- function(x) {
  preset <- x$config$preset
  if (is.null(preset)) {
    return(invisible(NULL))
  }
  cat("Preset:              ", preset, "\n\n")
  invisible(NULL)
}

#' Print the counts section of a transition_audit
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_counts <- function(x) {
  cat("Counts\n")
  cat("  Input rows:        ", x$counts$n_input_rows, "\n")
  cat("  Edges (scored):    ", x$counts$n_edges, "\n")
  cat("  Vertices (result): ", x$counts$n_vertices, "\n")
  invisible(NULL)
}

#' Print the dropped/collapsed section of a transition_audit
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_dropped <- function(x) {
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
  invisible(NULL)
}

#' Print the behavioral-coverage section of a transition_audit
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_coverage <- function(x) {
  cat("\nBehavioral coverage\n")
  if (!isTRUE(x$coverage$weighted)) {
    cat("  (unweighted / all edges equal)\n")
    return(invisible(NULL))
  }
  cat("  Weight column:      ", x$coverage$weight_col, "\n")
  cat("  Weighted edges:     ", x$coverage$n_edges_weighted, "\n")
  cov <- if (is.na(x$coverage$coverage)) {
    "NA"
  } else {
    paste0(formatC(100 * x$coverage$coverage, digits = 1, format = "f"), "%")
  }
  cat("  Coverage:           ", cov, "\n")
  invisible(NULL)
}

#' Print the duplicate-edge-policy section of a transition_audit
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_duplicates <- function(x) {
  if (is.null(x$duplicates)) {
    return(invisible(NULL))
  }
  cat("\nDuplicate edge policy\n")
  cat("  Policy:             ", x$duplicates$policy, "\n")
  cat("  Duplicate rows:     ", x$duplicates$n_duplicate_rows, "\n")
  if (!is.null(x$duplicates$instance_count_col)) {
    cat("  Instance count col: ", x$duplicates$instance_count_col, "\n")
  }
  if (is.data.frame(x$duplicates$duplicate_edges)) {
    cat("  Counted dup edges:  ", nrow(x$duplicates$duplicate_edges), "\n")
  }
  invisible(NULL)
}

#' Print the normalization section of a transition_audit
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_normalization <- function(x) {
  total <- if (is.na(x$normalization$pagerank_total)) {
    "NA"
  } else {
    formatC(x$normalization$pagerank_total, digits = 6, format = "f")
  }
  cat("\nNormalization\n")
  cat("  PageRank total:     ", total, "\n")
  invisible(NULL)
}

#' Print the page-mass section of a transition_audit
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_mass <- function(x) {
  # Mass accounting: only report once the stationary vector is defined.
  if (is.null(x$mass$total)) {
    return(invisible(NULL))
  }
  fmt_mass <- function(v) formatC(v, digits = 6, format = "f")
  cat("\nPage mass (stationary vector sums to 1)\n")
  cat("  Reported (visible): ", fmt_mass(x$mass$reported), "\n")
  cat("  Evaporated (sink):  ", fmt_mass(x$mass$sink), "\n")
  cat("  Leaked (out-scope): ", fmt_mass(x$mass$leaked), "\n")
  cat("  Hidden (robots):    ", fmt_mass(x$mass$hidden), "\n")
  cat("  Total:              ", fmt_mass(x$mass$total), "\n")
  invisible(NULL)
}

#' Print the fold-target collisions of a transition_audit
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_fold_collisions <- function(x) {
  collisions <- x$fold$collisions
  if (!is.data.frame(collisions)) {
    return(invisible(NULL))
  }
  if (nrow(collisions) == 0L) {
    return(invisible(NULL))
  }
  cat("  Fold-target collisions (merged inbound equity):\n")
  for (i in seq_len(nrow(collisions))) {
    cat(
      "    - ", collisions$target[i],
      " (", collisions$n_independent_refs[i],
      " independent ref(s); source: ", collisions$source[i], ")\n",
      sep = ""
    )
  }
  invisible(NULL)
}

#' Print the out-of-scope fold section of a transition_audit
#' @param x A `transition_audit` object.
#' @return `NULL`, invisibly; called for its printed output.
#' @noRd
.print_ta_fold <- function(x) {
  # Out-of-scope fold accounting: reported regardless of policy.
  if (is.null(x$fold)) {
    return(invisible(NULL))
  }
  cat("\nOut-of-scope folds (target not a crawled node)\n")
  cat("  Policy:             ", x$fold$policy, "\n")
  cat("  Out-of-scope folds: ", x$fold$n_out_of_scope, "\n")
  if (x$fold$n_out_of_scope > 0L) {
    action <- if (identical(x$fold$policy, "leak")) {
      "routed to leak sink"
    } else if (isTRUE(x$fold$applied)) {
      "relabeled (applied)"
    } else {
      "skipped (kept)"
    }
    cat("  Action:             ", action, "\n")
  }
  .print_ta_fold_collisions(x)
  invisible(NULL)
}

#' Print a transition_audit object
#'
#' @param x A `transition_audit` object.
#' @param ... Unused; for S3 compatibility.
#' @return `x`, invisibly.
#' @examples
#' # A transition_audit is attached to every pagerank() result; print it to
#' # get a human-readable construction / provenance summary.
#' edges <- data.frame(from = c("a", "a", "b"), to = c("b", "c", "c"))
#' result <- pagerank(edges)
#' audit <- attr(result, "transition_audit")
#' print(audit)
#' @export
print.transition_audit <- function(x, ...) {
  cat("=== Transition Construction Audit ===\n\n")
  .print_ta_preset(x)
  .print_ta_counts(x)
  .print_ta_dropped(x)
  .print_ta_coverage(x)
  .print_ta_duplicates(x)
  .print_ta_normalization(x)
  .print_ta_mass(x)
  .print_ta_fold(x)
  invisible(x)
}
