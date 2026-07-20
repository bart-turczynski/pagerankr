#' Named argument bundles for common PageRank views
#'
#' [pagerank()] exposes many graph-preparation knobs. A **preset** is a small,
#' named bundle of those arguments describing one recurring *view* of the link
#' graph, so a view is a one-liner instead of a hand-assembled argument list.
#'
#' `pr_preset()` returns the bundle as a plain named list, so it is
#' inspectable (print it to audit exactly what a preset does) and spliceable
#' (`do.call(pagerank, c(list(edges), pr_preset("raw")))`). Passing the name
#' directly -- `pagerank(edges, preset = "raw")` -- is equivalent.
#'
#' @section Presets:
#'
#' \describe{
#'   \item{`"raw"`}{The graph exactly as crawled: nothing is applied. Self
#'     loops are kept, isolates are kept, `rel=nofollow` is ignored (the edge
#'     votes like any other), and fold-map entries pointing at uncrawled
#'     targets are dropped rather than relabeling crawled pages onto phantom
#'     vertices. This is the faithful baseline to compare every other view
#'     against.}
#'   \item{`"declared"`}{The graph after honoring the signals the site
#'     *declares*: `rel=nofollow` evaporates, robots-blocked pages are removed
#'     from the results, declared canonical/redirect targets are followed even
#'     when they were not crawled, and self loops and isolates are dropped.
#'     Note that the declared **data** (`redirects_df`, `canonicals_df`,
#'     `indexability_df`) still has to be supplied by the caller -- a preset
#'     sets policy, never data.}
#' }
#'
#' @section Precedence:
#'
#' Arguments resolve **explicit argument > preset > base default**. A preset
#' value is applied only to arguments the caller did not name, so an explicit
#' argument is never silently overridden:
#'
#' ```r
#' pagerank(edges, preset = "raw")                        # nofollow kept
#' pagerank(edges, preset = "raw", nofollow_action = "drop")  # "drop" wins
#' ```
#'
#' This holds through the wrappers that forward `...` to [pagerank()]
#' ([trustrank()], [topic_sensitive_pagerank()],
#' [topic_feeder_pagerank()], [pagerank_screaming_frog()]), with one
#' boundary: arguments a wrapper sets itself are wrapper-owned and a preset
#' cannot change them.
#'
#' @param name A single string naming a registered preset. See "Presets".
#'
#' @return A named list of [pagerank()] arguments.
#' @export
#' @examples
#' pr_preset("raw")
#' pr_preset("declared")
#'
#' edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
#'
#' # Equivalent ways to run the raw view
#' pagerank(edges, preset = "raw")
#' do.call(pagerank, c(list(edges), pr_preset("raw")))
#'
#' # An explicit argument always wins over the preset
#' pagerank(edges, preset = "raw", drop_isolates_flag = TRUE)
pr_preset <- function(name) {
  registry <- .pr_preset_registry()
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a single preset name (a string).", call. = FALSE)
  }
  if (!name %in% names(registry)) {
    stop(
      "Unknown preset \"",
      name,
      "\". Available presets: ",
      toString(names(registry)),
      ".",
      call. = FALSE
    )
  }
  registry[[name]]
}

# Single source of truth for preset names and their expansions. Every entry
# must be a named list of `pagerank()` formals (enforced by tests) so that a
# preset can never introduce an argument `pagerank()` does not have.
.pr_preset_registry <- function() {
  list(
    raw = list(
      self_loops = "keep",
      drop_isolates_flag = FALSE,
      nofollow_action = "keep",
      out_of_scope_fold = "keep"
    ),
    declared = list(
      self_loops = "drop",
      drop_isolates_flag = TRUE,
      nofollow_action = "evaporate",
      robots_blocked_action = "vanish",
      out_of_scope_fold = "relabel"
    )
  )
}

# Normalize `preset` (NULL / name / hand-rolled list) to a named list of
# `pagerank()` arguments. Hand-rolled lists are validated against
# `pagerank()`'s formals so a typo fails loudly here instead of silently
# sliding through `...` into compute_pagerank().
.pr_resolve_preset <- function(preset) {
  if (is.null(preset)) {
    return(list())
  }
  if (is.character(preset)) {
    return(pr_preset(preset))
  }
  if (!is.list(preset)) {
    stop(
      "`preset` must be a preset name, a `pr_preset()` result, or NULL.",
      call. = FALSE
    )
  }
  .pr_check_preset_names(preset)
  preset
}

.pr_check_preset_names <- function(preset) {
  nms <- names(preset)
  if (length(preset) > 0L && (is.null(nms) || !all(nzchar(nms)))) {
    stop("Every element of a `preset` list must be named.", call. = FALSE)
  }
  if (anyDuplicated(nms) > 0L) {
    stop(
      "Duplicated `preset` argument(s): ",
      toString(unique(nms[duplicated(nms)])),
      ".",
      call. = FALSE
    )
  }
  reserved <- c("edge_list_df", "preset", "...")
  bad_reserved <- intersect(nms, reserved)
  if (length(bad_reserved) > 0L) {
    stop(
      "A `preset` cannot set: ",
      toString(bad_reserved),
      ".",
      call. = FALSE
    )
  }
  unknown <- setdiff(nms, names(formals(pagerank)))
  if (length(unknown) > 0L) {
    stop(
      "`preset` contains argument(s) `pagerank()` does not have: ",
      toString(unknown),
      ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Apply a preset into `env` (a `pagerank()` evaluation frame), honoring the
# precedence rule explicit arg > preset > base default: only arguments absent
# from `mcall` (the matched call, dots expanded) are written. Returns the names
# actually applied, invisibly.
.pr_apply_preset <- function(preset, mcall, env) {
  values <- .pr_resolve_preset(preset)
  applied <- setdiff(names(values), names(mcall))
  if (length(applied) > 0L) {
    list2env(values[applied], envir = env)
  }
  invisible(applied)
}
