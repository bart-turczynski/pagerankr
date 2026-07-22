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
#'     *declares*: `rel=nofollow` evaporates, declared canonical/redirect
#'     targets are followed even when they were not crawled, robots-blocked
#'     pages keep the authority they collect, and self loops and isolates are
#'     dropped. This is a **pure pin of the package defaults** -- it changes
#'     nothing about how [pagerank()] behaves today. Its value is that it
#'     *states* the default view: a run made with `preset = "declared"` is a
#'     recorded, auditable claim about which view was intended, and it stays
#'     pinned to the bundle as documented even if a future default moves. Note
#'     that the declared **data** (`redirects_df`, `canonicals_df`,
#'     `indexability_df`) still has to be supplied by the caller -- a preset
#'     sets policy, never data.}
#'   \item{`"reversed"`}{The graph with every edge flipped (`reverse = TRUE`),
#'     yielding reverse / inverse PageRank: a page scores highly when it points
#'     *at* well-connected pages rather than when it is pointed at. The feeder
#'     view. Note that [topic_feeder_pagerank()] already reverses the graph
#'     itself, so this preset is a no-op there rather than an error.}
#'   \item{`"content"`}{Weights edges by the page region they were found in:
#'     links in the main content keep their full vote, while links in
#'     navigation, header, footer and aside are discounted to a tenth. Site
#'     chrome is typically the large majority of a crawl's edges, so left
#'     unweighted it *manufactures* the ranking. Edges are **downweighted, never
#'     dropped** -- placement is a heuristic classification, so a misclassified
#'     content link at 0.1 is a small error where a dropped one is a silent
#'     deletion, and dropping most of the graph would also manufacture isolates
#'     and dangling pages. All five placement terms are named explicitly, so
#'     this is a complete recipe rather than a partial adjustment.
#'
#'     This preset sets policy; the *data* is `placement_col`, which the caller
#'     must supply (it errors otherwise). [pagerank_screaming_frog()] supplies
#'     it from the bundle, so `preset = "content"` works there directly.}
#' }
#'
#' Presets are not composable with one another -- `preset` takes a single
#' bundle. `"content"` sets only placement weights and leaves every graph
#' hygiene knob at its default, which *is* the `"declared"` view, so the two do
#' not need to be combined.
#'
#' @section Provenance:
#'
#' The [transition_audit] attached to a [pagerank()] result records which
#' preset produced it, in `audit$config$preset`: the preset name for a
#' registered preset (whether passed by name or as a `pr_preset()` result),
#' `"custom"` for a hand-rolled bundle, and `NULL` when no preset was used. The
#' rest of `config` records the resulting configuration, so a result can be
#' both reconstructed and attributed to the named view it was asked for.
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
#' @seealso `vignette("presets")` for each preset's full expansion, worked
#'   examples, and the precedence rule.
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
#'
#' # "content" needs a placement column to read regions from. B is linked from
#' # the main content and C only from the footer, so B outranks C.
#' placed <- data.frame(
#'   from = c("A", "A", "B", "C"),
#'   to = c("B", "C", "A", "A"),
#'   region = c("content", "footer", "content", "content")
#' )
#' pagerank(placed, preset = "content", placement_col = "region")
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
  # Tag the bundle with its name so provenance survives
  # `pagerank(edges, preset = pr_preset("raw"))` -- the list form is still just
  # a plain named list, but the audit can tell it apart from a hand-rolled one.
  structure(registry[[name]], pr_preset_name = name)
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
    # "declared" is a PURE PIN of today's `pagerank()` defaults: it changes
    # nothing, it *states* the default view so a result can be frozen against
    # future default drift. When a default moves, this bundle is re-pinned to
    # whatever the defaults then are -- the pin tracks the defaults by
    # definition.
    declared = list(
      self_loops = "drop",
      drop_isolates_flag = TRUE,
      nofollow_action = "evaporate",
      robots_blocked_action = "show",
      out_of_scope_fold = "relabel"
    ),
    reversed = list(
      reverse = TRUE
    ),
    # All five placements are named on purpose. Placements absent from
    # `placement_weights` keep weight 1, so a partial recipe such as
    # `c(nav = 0.1)` would leave footer and aside outweighing nav tenfold. A
    # preset is a *complete* recipe, so it names every term.
    content = list(
      placement_weights = c(
        content = 1,
        nav = 0.1,
        header = 0.1,
        footer = 0.1,
        aside = 0.1
      )
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

# Provenance label for the `preset` argument, recorded in the transition audit
# so a result says which named view produced it. A registry preset is recorded
# by name; a hand-rolled bundle is recorded as "custom" (its expansion is
# already visible in the rest of the config); no preset is recorded as NULL.
.pr_preset_label <- function(preset) {
  if (is.null(preset)) {
    return(NULL)
  }
  if (is.character(preset)) {
    return(preset)
  }
  name <- attr(preset, "pr_preset_name", exact = TRUE)
  if (is.character(name) && length(name) == 1L) {
    return(name)
  }
  "custom"
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
