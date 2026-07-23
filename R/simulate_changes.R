#' @title Simulate the PageRank Impact of Link and Redirect Changes
#' @description Compares PageRank before and after proposed changes to the link
#'   graph, at both the edge level (adding/removing links) and the URL level
#'   (retiring a page behind a redirect, or repointing an existing redirect).
#'   The whole graph is recomputed and a before/after table is returned;
#'   interpretation is left to the caller. This is a faithful recompute
#'   primitive, not a ranking or target-optimization engine.
#'
#' @param edge_list_df A data frame representing the current link edge list.
#' @param add_links_df Optional data frame of links to add. Must have the same
#'   from/to column names as \code{edge_list_df}. Columns present in
#'   \code{edge_list_df} but absent here are padded with \code{NA} on the added
#'   rows, so weighted / annotated edge lists keep their schema.
#'   Default \code{NULL}.
#' @param remove_links_df Optional data frame of links to remove. Matching is
#'   by exact from+to pair. Must have the same from/to column names as
#'   \code{edge_list_df}. Default \code{NULL}.
#' @param redirect_urls_df Optional two-column \code{from}/\code{to} data frame
#'   of URL-level redirects to model. Each row retires the \code{from} URL and
#'   sends its inbound authority to \code{to} at 100\% pass-through. Retire
#'   semantics: the live source's own outbound links are \strong{stripped}
#'   before folding (an honest 301 has no body), so the target inherits the
#'   source's inbound authority only, never its outlinks. A row for source
#'   \code{A} \strong{overrides} any prior redirect for \code{A} -- whether from
#'   an earlier row or from the baseline crawl's real 3xx -- so
#'   "change A into a redirect to C" is a single override. A duplicate source
#'   mapping to two distinct targets in one changeset is an error (strict).
#'   Default \code{NULL}.
#' @param remove_urls Optional character vector of URLs to model as removed
#'   (turned into HTTP 404s). Each removed URL keeps its inbound links -- other
#'   pages still point at it -- but now they flow into a dead page: authority
#'   arrives and \strong{evaporates} to the shared waste sink rather than
#'   redistributing across the site (dangle) or self-amplifying (self-loop). The
#'   page's own outbound links are dropped. The node stays in the output holding
#'   the mass it absorbed once, flagged \code{"removed-dead"} in
#'   \code{node_status} so its residual score is never misread as earned
#'   authority. Under the hood this forces a \code{status_df} entry (HTTP
#'   \code{404}) into the proposed model only; 4xx and 5xx are one class (no
#'   split). A URL appearing in both \code{remove_urls} and
#'   \code{redirect_urls_df} is an error (a page cannot be both a 301 and a
#'   404). To also model cleaning up the inbound links, compose with
#'   \code{remove_links_df}. Default \code{NULL}.
#' @param redirects_df Optional data frame of existing redirects (baseline).
#'   Default \code{NULL}.
#' @param on_unknown_target How to treat a redirect or link \emph{target} that
#'   is not a node in the current graph (it may be a legitimate new page,
#'   modeled as a new node that carries inbound authority with no outlinks yet).
#'   One of \code{"warn"} (default, warn and proceed), \code{"error"}, or
#'   \code{"allow"} (proceed silently).
#' @param ... Additional arguments passed to both \code{pagerank()} calls
#'   (e.g., \code{clean_edge_urls}, \code{damping}, \code{nofollow_col},
#'   \code{indexability_df}, etc.).
#' @param edge_from_col Name of the from column in edge list data frames.
#'   Default \code{"from"}.
#' @param edge_to_col Name of the to column in edge list data frames.
#'   Default \code{"to"}.
#' @param redirect_from_col Name of the source column in \code{redirect_urls_df}
#'   and \code{redirects_df}. Default \code{"from"}.
#' @param redirect_to_col Name of the target column in \code{redirect_urls_df}
#'   and \code{redirects_df}. Default \code{"to"}.
#' @param label_baseline Label for the baseline model in the comparison output.
#'   Default \code{"baseline"}.
#' @param label_proposed Label for the proposed model in the comparison output.
#'   Default \code{"proposed"}.
#'
#' @return The output of \code{\link{compare_pagerank}} (per-node deltas,
#'   percentage changes, and rank changes between baseline and proposed) with an
#'   added \code{node_status} column: \code{"normal"} for a node present and
#'   live in both models, \code{"new-target"} for a node introduced by the
#'   changeset (present in the proposed model, absent from the baseline), or
#'   \code{"removed-dead"} for a node retired via \code{remove_urls} (its
#'   proposed score is residual absorbed mass on the way to the waste sink, not
#'   earned authority). Attributes:
#'   \describe{
#'     \item{summary}{Aggregate statistics from \code{compare_pagerank()}.}
#'     \item{proposed}{The full proposed \code{pagerank()} result, including its
#'       \code{transition_audit} attribute, so the evaporated-mass cost of a
#'       removal is surfaced by default.}
#'     \item{manifest}{A named list describing the changeset: redirects applied,
#'       which sources overrode a prior redirect, URLs removed, link add/remove
#'       counts, and any unknown targets.}
#'   }
#'
#' @seealso \code{\link{simulate_changes_screaming_frog}} for the Screaming Frog
#'   bundle entry point.
#' @export
#' @examples
#' # Current site links
#' edges <- data.frame(
#'   from = c("Home", "Home", "About", "Blog"),
#'   to = c("About", "Blog", "Home", "Home")
#' )
#'
#' # Propose adding a link from Blog to About
#' new_links <- data.frame(
#'   from = "Blog", to = "About"
#' )
#' result <- simulate_changes(edges,
#'   add_links_df = new_links,
#'   clean_edge_urls = FALSE
#' )
#' print(result)
#' attr(result, "summary")
#'
#' # Retire the About page behind a redirect to Home
#' retire <- data.frame(from = "About", to = "Home")
#' simulate_changes(edges, redirect_urls_df = retire, clean_edge_urls = FALSE)
#'
#' # Model the About page 404-ing: inbound authority flows in and evaporates
#' simulate_changes(edges, remove_urls = "About", clean_edge_urls = FALSE)
simulate_changes <- function(edge_list_df,
                             add_links_df = NULL,
                             remove_links_df = NULL,
                             redirect_urls_df = NULL,
                             remove_urls = NULL,
                             redirects_df = NULL,
                             on_unknown_target = c("warn", "error", "allow"),
                             ...,
                             edge_from_col = "from",
                             edge_to_col = "to",
                             redirect_from_col = "from",
                             redirect_to_col = "to",
                             label_baseline = "baseline",
                             label_proposed = "proposed") {
  on_unknown_target <- match.arg(on_unknown_target)

  if (!is.data.frame(edge_list_df)) {
    stop("`edge_list_df` must be a data frame.", call. = FALSE)
  }
  dots <- list(...)
  if ("add_redirects_df" %in% names(dots)) {
    stop("`add_redirects_df` has been removed. Use `redirect_urls_df` ",
      "(create-or-override, retire semantics) instead.",
      call. = FALSE
    )
  }

  baseline_args <- c(
    list(
      edge_list_df = edge_list_df,
      redirects_df = redirects_df,
      edge_from_col = edge_from_col,
      edge_to_col = edge_to_col,
      redirect_from_col = redirect_from_col,
      redirect_to_col = redirect_to_col
    ),
    dots
  )

  .simulate_changes_engine(
    baseline_args = baseline_args,
    add_links_df = add_links_df,
    remove_links_df = remove_links_df,
    redirect_urls_df = redirect_urls_df,
    remove_urls = remove_urls,
    on_unknown_target = on_unknown_target,
    edge_from_col = edge_from_col,
    edge_to_col = edge_to_col,
    redirect_from_col = redirect_from_col,
    redirect_to_col = redirect_to_col,
    label_baseline = label_baseline,
    label_proposed = label_proposed
  )
}


#' Shared changeset engine for both `simulate_changes()` entry points
#'
#' Takes a fully-built list of baseline `pagerank()` arguments (from the bare
#' edge-list path or the Screaming Frog bundle adapter), applies the URL- and
#' edge-level verbs to produce the proposed graph, recomputes both models, and
#' assembles the comparison output. `baseline_args` must contain at least
#' `edge_list_df` and `redirects_df`; everything else rides along unchanged into
#' both `pagerank()` calls so the two models differ only by the changeset.
#' @noRd
.simulate_changes_engine <- function(baseline_args,
                                     add_links_df, remove_links_df,
                                     redirect_urls_df, remove_urls = NULL,
                                     on_unknown_target,
                                     edge_from_col, edge_to_col,
                                     redirect_from_col, redirect_to_col,
                                     label_baseline, label_proposed) {
  edge_list_df <- baseline_args$edge_list_df
  baseline_redirects <- baseline_args$redirects_df

  # --- Validate the changeset ---
  required_cols <- c(edge_from_col, edge_to_col)
  .validate_link_df(
    add_links_df, "add_links_df", required_cols, edge_from_col, edge_to_col
  )
  .validate_link_df(
    remove_links_df, "remove_links_df", required_cols,
    edge_from_col, edge_to_col
  )
  .validate_redirect_urls_df(
    redirect_urls_df, redirect_from_col, redirect_to_col
  )
  remove_urls <- .validate_remove_urls(remove_urls)

  redirect_sources <- .redirect_urls_sources(
    redirect_urls_df, redirect_from_col
  )

  # A URL cannot be both a 301 (redirect_urls_df) and a 404 (remove_urls): a
  # genuine contradiction, not a precedence question -- error rather than guess.
  both <- intersect(remove_urls, redirect_sources)
  if (length(both) > 0) {
    stop("URL(s) in both `remove_urls` and `redirect_urls_df` ",
      "(a page cannot be both a 301 and a 404): ", toString(both), ".",
      call. = FALSE
    )
  }

  # --- Unknown-target handling (warn / error / allow) ---
  known_nodes <- unique(c(
    as.character(edge_list_df[[edge_from_col]]),
    as.character(edge_list_df[[edge_to_col]])
  ))
  unknown_targets <- .simulate_unknown_targets(
    known_nodes, redirect_urls_df, add_links_df,
    redirect_to_col, edge_to_col
  )
  .handle_unknown_targets(unknown_targets, on_unknown_target)

  # --- Build the proposed graph (order: redirect -> remove -> add links) ---
  proposed_edges <- .build_proposed_edges(
    edge_list_df, add_links_df, remove_links_df, redirect_sources,
    edge_from_col, edge_to_col
  )
  proposed_redirects <- .build_proposed_redirects(
    baseline_redirects, redirect_urls_df, redirect_from_col, redirect_to_col
  )

  # --- Recompute both models ---
  # The baseline keeps each page's real status; the proposed model forces the
  # removed URLs to 404 so pagerank()'s waste-sink mechanism strips their
  # outedges and evaporates their inbound authority (the first consumer of the
  # PAGE-qzskzcfd dead-class mechanism). Removal affects the proposed model.
  pr_baseline <- do.call(pagerank, baseline_args)
  proposed_args <- baseline_args
  proposed_args$edge_list_df <- proposed_edges
  proposed_args$redirects_df <- proposed_redirects
  proposed_args <- .apply_removed_status(proposed_args, remove_urls)
  pr_proposed <- do.call(pagerank, proposed_args)

  # --- Assemble output ---
  result <- compare_pagerank(
    pr_baseline, pr_proposed,
    label_a = label_baseline, label_b = label_proposed
  )
  result <- .attach_node_status(
    result, label_baseline, label_proposed, remove_urls
  )

  attr(result, "proposed") <- pr_proposed
  attr(result, "manifest") <- .build_change_manifest(
    redirect_urls_df, baseline_redirects, redirect_sources,
    remove_urls, add_links_df, remove_links_df, unknown_targets,
    redirect_from_col
  )
  result
}


#' Validate a link data frame argument for simulate_changes()
#' @noRd
.validate_link_df <- function(df, arg_name, required_cols, from_col, to_col) {
  if (is.null(df)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(df)) {
    stop("`", arg_name, "` must be a data frame or NULL.", call. = FALSE)
  }
  if (nrow(df) > 0 && !all(required_cols %in% names(df))) {
    stop("`", arg_name, "` must have '", from_col, "' and '",
      to_col, "' columns.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' Validate `redirect_urls_df` (shape + strict single-target-per-source)
#'
#' A two-column from/to redirect changeset. Enforces the strict rule that a
#' single changeset must send each URL to exactly one destination (A->B and
#' A->C is an error), mirroring `resolve_redirects()`'s default
#' `duplicate_from_policy = "strict"`. Exact-duplicate rows (A->B twice) are
#' allowed and deduplicated downstream.
#' @noRd
.validate_redirect_urls_df <- function(df, from_col, to_col) {
  if (is.null(df)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(df)) {
    stop("`redirect_urls_df` must be a data frame or NULL.", call. = FALSE)
  }
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }
  if (!all(c(from_col, to_col) %in% names(df))) {
    stop("`redirect_urls_df` must have '", from_col, "' and '",
      to_col, "' columns.",
      call. = FALSE
    )
  }

  sources <- as.character(df[[from_col]])
  targets <- as.character(df[[to_col]])
  distinct_pairs <- !duplicated(paste0(sources, "\t", targets))
  distinct_sources <- sources[distinct_pairs]
  conflicting <- unique(distinct_sources[duplicated(distinct_sources)])
  if (length(conflicting) > 0) {
    first <- conflicting[1]
    tgts <- unique(targets[sources == first])
    stop("Ambiguous redirect in `redirect_urls_df`: URL '", first,
      "' maps to multiple distinct targets: ", toString(tgts),
      ". A single changeset must send each URL to one destination.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' Unique redirect source URLs (the pages being retired)
#' @noRd
.redirect_urls_sources <- function(df, from_col) {
  if (is.null(df) || nrow(df) == 0) {
    return(character(0))
  }
  unique(as.character(df[[from_col]]))
}


#' Validate and normalize `remove_urls` to a unique character vector
#'
#' URLs are singletons, so the argument is a character vector (mirroring
#' `keep_domains` / `exclude_domains`), not a data frame. Returns
#' `character(0)` for `NULL` / empty input; otherwise a de-duplicated,
#' `NA`-dropped character vector.
#' @noRd
.validate_remove_urls <- function(remove_urls) {
  if (is.null(remove_urls) || length(remove_urls) == 0) {
    return(character(0))
  }
  if (!is.character(remove_urls) && !is.factor(remove_urls)) {
    stop("`remove_urls` must be a character vector of URLs or NULL.",
      call. = FALSE
    )
  }
  out <- as.character(remove_urls)
  unique(out[!is.na(out)])
}


#' Force removed URLs to HTTP 404 in the proposed pagerank() arguments
#'
#' Builds (or extends) the proposed model's `status_df` so every removed URL is
#' marked `404`, overriding any real crawled status it carried. Rows for
#' non-removed URLs are preserved. Uses the caller's resolved `status_url_col` /
#' `status_col` (defaulting to `"url"` / `"status_code"`, matching `pagerank()`)
#' so a supplied status table and the synthetic dead rows share a schema. This
#' mutates the proposed arguments only; the baseline keeps each page live.
#' @noRd
.apply_removed_status <- function(proposed_args, remove_urls) {
  if (length(remove_urls) == 0) {
    return(proposed_args)
  }
  url_col <- if (is.null(proposed_args$status_url_col)) {
    "url"
  } else {
    proposed_args$status_url_col
  }
  code_col <- if (is.null(proposed_args$status_col)) {
    "status_code"
  } else {
    proposed_args$status_col
  }

  synth <- stats::setNames(
    list(as.character(remove_urls), rep(404L, length(remove_urls))),
    c(url_col, code_col)
  )
  synth <- data.frame(synth, stringsAsFactors = FALSE, check.names = FALSE)

  existing <- proposed_args$status_df
  if (is.null(existing) || nrow(existing) == 0) {
    proposed_args$status_df <- synth
  } else {
    keep <- !(as.character(existing[[url_col]]) %in% remove_urls)
    proposed_args$status_df <- .rbind_aligned(
      existing[keep, , drop = FALSE], synth
    )
  }
  proposed_args$status_url_col <- url_col
  proposed_args$status_col <- code_col
  proposed_args
}


#' Redirect / link targets that are not yet nodes in the current graph
#' @noRd
.simulate_unknown_targets <- function(known_nodes, redirect_urls_df,
                                      add_links_df, redirect_to_col,
                                      edge_to_col) {
  targets <- character(0)
  if (!is.null(redirect_urls_df) && nrow(redirect_urls_df) > 0) {
    targets <- c(targets, as.character(redirect_urls_df[[redirect_to_col]]))
  }
  if (!is.null(add_links_df) && nrow(add_links_df) > 0) {
    targets <- c(targets, as.character(add_links_df[[edge_to_col]]))
  }
  targets <- unique(targets[!is.na(targets)])
  setdiff(targets, known_nodes)
}


#' Apply the `on_unknown_target` policy to unknown targets
#' @noRd
.handle_unknown_targets <- function(unknown_targets, on_unknown_target) {
  if (length(unknown_targets) == 0 || on_unknown_target == "allow") {
    return(invisible(NULL))
  }
  msg <- paste0(
    "Change targets not present in the current graph ",
    "(modeled as new nodes): ", toString(unknown_targets), "."
  )
  if (on_unknown_target == "error") {
    stop(msg, call. = FALSE)
  }
  warning(msg, call. = FALSE)
  invisible(NULL)
}


#' Build the proposed edge list
#'
#' Applies, in order: strip retired redirect sources' outedges (retire, not
#' move), remove exact from+to pairs, then append added links (aligned to the
#' edge list's full schema so weighted / annotated columns survive).
#' @noRd
.build_proposed_edges <- function(edge_list_df, add_links_df, remove_links_df,
                                  redirect_sources, from_col, to_col) {
  proposed_edges <- edge_list_df

  if (length(redirect_sources) > 0) {
    keep <- !(as.character(proposed_edges[[from_col]]) %in% redirect_sources)
    proposed_edges <- proposed_edges[keep, , drop = FALSE]
  }

  if (!is.null(remove_links_df) && nrow(remove_links_df) > 0) {
    remove_key <- paste0(
      as.character(remove_links_df[[from_col]]), "\t",
      as.character(remove_links_df[[to_col]])
    )
    current_key <- paste0(
      as.character(proposed_edges[[from_col]]), "\t",
      as.character(proposed_edges[[to_col]])
    )
    proposed_edges <- proposed_edges[!(current_key %in% remove_key), ,
      drop = FALSE
    ]
  }

  if (!is.null(add_links_df) && nrow(add_links_df) > 0) {
    proposed_edges <- .rbind_aligned(proposed_edges, add_links_df)
  }

  proposed_edges
}


#' Row-bind `add_df` onto `base_df`, keeping `base_df`'s columns
#'
#' Columns present in `base_df` but not in `add_df` are padded with `NA` on the
#' added rows; columns present only in `add_df` are dropped. This keeps a
#' weighted / Screaming Frog edge schema (nofollow, placement, weight, ...)
#' intact when a bare two-column set of links is added.
#' @noRd
.rbind_aligned <- function(base_df, add_df) {
  n <- nrow(add_df)
  cols <- names(base_df)
  filled <- stats::setNames(vector("list", length(cols)), cols)
  for (cn in cols) {
    filled[[cn]] <- if (cn %in% names(add_df)) add_df[[cn]] else rep(NA, n)
  }
  new_rows <- data.frame(filled, stringsAsFactors = FALSE, check.names = FALSE)
  rbind(base_df, new_rows)
}


#' Build the proposed redirect table (baseline overridden by the changeset)
#'
#' Any baseline redirect whose source appears in `redirect_urls_df` is dropped,
#' then the changeset rows are appended, so a changeset row for source A wins
#' over a prior redirect for A (create-or-override). Only the from/to columns
#' are carried forward.
#' @noRd
.build_proposed_redirects <- function(baseline_redirects, redirect_urls_df,
                                      from_col, to_col) {
  if (is.null(redirect_urls_df) || nrow(redirect_urls_df) == 0) {
    return(baseline_redirects)
  }

  ru <- redirect_urls_df[, c(from_col, to_col), drop = FALSE]
  ru <- ru[!duplicated(paste0(
    as.character(ru[[from_col]]), "\t", as.character(ru[[to_col]])
  )), , drop = FALSE]
  sources <- as.character(ru[[from_col]])

  if (is.null(baseline_redirects) || nrow(baseline_redirects) == 0) {
    return(ru)
  }

  base_keep <- !(as.character(baseline_redirects[[from_col]]) %in% sources)
  base_kept <- baseline_redirects[base_keep, c(from_col, to_col), drop = FALSE]
  rbind(base_kept, ru)
}


#' Add the per-row `node_status` column to the comparison table
#'
#' A node retired via `remove_urls` is `removed-dead` (its proposed score is
#' residual absorbed mass en route to the waste sink, not earned authority). A
#' node absent from the baseline but present in the proposed model is a
#' `new-target` (a page the changeset introduced, carrying inbound authority it
#' did not earn in the baseline); every other node is `normal`. `removed-dead`
#' is assigned last so it wins for a URL that is somehow both.
#' @noRd
.attach_node_status <- function(result, label_baseline, label_proposed,
                                remove_urls = character(0)) {
  base_col <- paste0("pagerank_", label_baseline)
  prop_col <- paste0("pagerank_", label_proposed)
  status <- rep("normal", nrow(result))
  is_new <- is.na(result[[base_col]]) & !is.na(result[[prop_col]])
  status[is_new] <- "new-target"
  if (length(remove_urls) > 0) {
    status[as.character(result[["node_name"]]) %in% remove_urls] <-
      "removed-dead"
  }
  result$node_status <- status
  result
}


#' Build the change manifest attached to the comparison output
#' @noRd
.build_change_manifest <- function(redirect_urls_df, baseline_redirects,
                                   redirect_sources, remove_urls, add_links_df,
                                   remove_links_df, unknown_targets,
                                   redirect_from_col) {
  overrode <- character(0)
  if (length(redirect_sources) > 0 && !is.null(baseline_redirects) &&
    nrow(baseline_redirects) > 0 &&
    redirect_from_col %in% names(baseline_redirects)) {
    base_src <- as.character(baseline_redirects[[redirect_from_col]])
    overrode <- intersect(redirect_sources, base_src)
  }
  list(
    redirects_applied = redirect_urls_df,
    redirects_overrode = overrode,
    urls_removed = remove_urls,
    links_added = if (is.null(add_links_df)) 0L else nrow(add_links_df),
    links_removed = if (is.null(remove_links_df)) 0L else nrow(remove_links_df),
    unknown_targets = unknown_targets
  )
}
