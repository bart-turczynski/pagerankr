# Build the two-crawl case-study fixture shipped in inst/extdata/.
#
# The source crawls are PRIVATE and are not part of this repository. This script
# reads them from paths supplied at run time, rewrites every identifying string
# through a deterministic dictionary, trims each export to the columns declared
# by sf_contract(), and writes the publishable result to inst/extdata/.
#
# Usage:
#   Rscript data-raw/build-fixture.R \
#     --before=<dir> --after=<dir> \
#     --primary-host=<crawled host> --secondary-host=<canonical-target host>
#
# Both directories must contain all_inlinks.csv and internal_all.csv exported by
# Screaming Frog. Nothing derived from the real hostnames or paths is written to
# disk outside inst/extdata/, and the dictionary itself is deliberately NOT
# persisted: it maps synthetic URLs back to real ones, so keeping it would undo
# the pseudonymization. Re-running against the same source reproduces the same
# output, because every label is assigned from a sorted traversal.
suppressPackageStartupMessages({
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(name, default = NULL) {
  prefix <- paste0("^--", name, "=")
  hit <- grep(prefix, args, value = TRUE)
  if (length(hit) == 0) {
    if (is.null(default)) {
      stop("missing required argument --", name, call. = FALSE)
    }
    return(default)
  }
  sub(prefix, "", hit[[1]])
}

dir_before    <- path.expand(arg_val("before"))
dir_after     <- path.expand(arg_val("after"))
host_primary  <- arg_val("primary-host")
host_secondary <- arg_val("secondary-host")

out_root <- file.path("inst", "extdata")
out_before <- file.path(out_root, "reviews-microsite-before")
out_after  <- file.path(out_root, "reviews-microsite-after")

# ---- source ---------------------------------------------------------------

read_crawl <- function(dir) {
  list(
    links    = fread(file.path(dir, "all_inlinks.csv"), showProgress = FALSE,
                     colClasses = "character"),
    internal = fread(file.path(dir, "internal_all.csv"), showProgress = FALSE,
                     colClasses = "character")
  )
}

crawls <- list(before = read_crawl(dir_before), after = read_crawl(dir_after))

url_cols_links    <- c("Source", "Destination")
url_cols_internal <- c("Address", "Canonical Link Element 1", "Redirect URL")

collect_urls <- function(crawls) {
  out <- character()
  for (cr in crawls) {
    for (col in intersect(url_cols_links, names(cr$links))) {
      out <- c(out, cr$links[[col]])
    }
    for (col in intersect(url_cols_internal, names(cr$internal))) {
      out <- c(out, cr$internal[[col]])
    }
  }
  sort(unique(out[nzchar(out)]))
}

urls <- collect_urls(crawls)

# ---- URL dictionary -------------------------------------------------------

parse_url <- function(u) {
  scheme <- sub("^([a-zA-Z][a-zA-Z0-9+.-]*)://.*$", "\\1", u)
  rest   <- sub("^[a-zA-Z][a-zA-Z0-9+.-]*://", "", u)
  host   <- sub("^([^/?#]*).*$", "\\1", rest)
  tail   <- substring(rest, nchar(host) + 1L)
  path   <- sub("[?#].*$", "", tail)
  query  <- NA_character_
  frag   <- NA_character_
  if (grepl("?", tail, fixed = TRUE)) {
    query <- sub("^[^?]*\\?([^#]*).*$", "\\1", tail)
  }
  if (grepl("#", tail, fixed = TRUE)) {
    frag <- sub("^[^#]*#(.*)$", "\\1", tail)
  }
  data.table(url = u, scheme = scheme, host = host, path = path,
             query = query, fragment = frag)
}

parts <- rbindlist(lapply(urls, parse_url))

# Hosts: the two site hosts keep the pseudonyms already used in notes/; every
# other host becomes a numbered example.org, assigned in sorted order so the
# labelling is stable across runs.
ext_hosts <- sort(setdiff(unique(parts$host), c(host_primary, host_secondary)))
host_map <- c(
  setNames("reviews-microsite.example.net", host_primary),
  setNames("reviews-microsite.example.com", host_secondary),
  setNames(
    sprintf("external-%02d.example.org", seq_along(ext_hosts)),
    ext_hosts
  )
)

# Paths: topology-preserving, semantics-free. Segments are relabelled per
# parent so depth and sibling grouping survive while meaning does not. A leaf
# carrying a file extension keeps it, since asset type is part of the graph.
split_path <- function(p) {
  segs <- strsplit(sub("^/", "", p), "/", fixed = TRUE)[[1]]
  segs[nzchar(segs)]
}

seg_label <- function(depth, index) {
  if (depth == 1L) sprintf("s%d", index) else sprintf("p%02d", index)
}

# Assign a label to every node of the path trie, per host, in sorted order.
label_env <- new.env(parent = emptyenv())
assign_labels <- function(host, paths) {
  nodes <- list()
  for (p in paths) {
    segs <- split_path(p)
    if (length(segs) == 0L) next
    for (i in seq_along(segs)) {
      parent <- paste(segs[seq_len(i - 1L)], collapse = "/")
      nodes[[length(nodes) + 1L]] <- list(
        parent = parent, seg = segs[[i]], depth = i
      )
    }
  }
  if (length(nodes) == 0L) return(invisible(NULL))
  nd <- unique(rbindlist(lapply(nodes, as.data.table)))
  setorder(nd, parent, seg)
  nd[, index := seq_len(.N), by = parent]
  for (i in seq_len(nrow(nd))) {
    key <- paste(host, nd$parent[i], nd$seg[i], sep = "\r")
    assign(key, seg_label(nd$depth[i], nd$index[i]), envir = label_env)
  }
  invisible(NULL)
}

for (h in unique(parts$host)) {
  assign_labels(h, unique(parts$path[parts$host == h]))
}

map_path <- function(host, p) {
  segs <- split_path(p)
  if (length(segs) == 0L) return("/")
  out <- character(length(segs))
  for (i in seq_along(segs)) {
    parent <- paste(segs[seq_len(i - 1L)], collapse = "/")
    key <- paste(host, parent, segs[[i]], sep = "\r")
    lab <- get(key, envir = label_env)
    ext <- regmatches(segs[[i]], regexpr("\\.[A-Za-z0-9]{1,5}$", segs[[i]]))
    if (length(ext) == 1L && i == length(segs)) lab <- paste0(lab, ext)
    out[i] <- lab
  }
  paste0("/", paste(out, collapse = "/"), if (grepl("/$", p)) "/" else "")
}

parts[, new_host := host_map[host]]
parts[, new_path := mapply(map_path, host, path)]
parts[, new_url := paste0(scheme, "://", new_host, new_path)]
parts[!is.na(query),    new_url := paste0(new_url, "?q=1")]
parts[!is.na(fragment), new_url := paste0(new_url, "#f1")]

# Collisions would silently merge two real nodes into one, changing the graph.
# Disambiguate deterministically and assert the map stays injective.
dup <- parts[, .N, by = new_url][N > 1L]
if (nrow(dup) > 0L) {
  for (u in dup$new_url) {
    idx <- which(parts$new_url == u)
    parts$new_url[idx] <- paste0(u, "-", seq_along(idx))
  }
}
stopifnot(!anyDuplicated(parts$new_url), nrow(parts) == length(urls))

url_map <- setNames(parts$new_url, parts$url)

# ---- text rewriting -------------------------------------------------------

leaf_of <- function(u) {
  p <- sub("^[a-z]+://[^/]+", "", sub("[?#].*$", "", u))
  segs <- split_path(p)
  if (length(segs) == 0L) "home" else segs[[length(segs)]]
}

# Anchor and alt text are regenerated rather than scrubbed: a scrub leaves
# whatever it failed to anticipate, a regeneration cannot. Only the empty /
# non-empty distinction is preserved, because an empty anchor is a real signal
# (image links, and the boilerplate detector reads it).
rewrite_text <- function(x, dest_new, prefix) {
  out <- rep("", length(x))
  keep <- !is.na(x) & nzchar(x)
  out[keep] <- paste0(prefix, vapply(dest_new[keep], leaf_of, character(1)))
  out
}

# ---- apply ----------------------------------------------------------------

map_col <- function(v) {
  out <- url_map[v]
  gap <- is.na(out)
  blank <- is.na(v[gap]) | !nzchar(v[gap])
  out[gap] <- ifelse(blank, "", NA_character_)
  unname(out)
}

segments_of <- function(u) {
  p <- sub("^[a-z]+://[^/]+", "", sub("[?#].*$", "", u))
  segs <- split_path(p)
  paste(segs, collapse = "/")
}

contract <- local({
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
  pagerankr:::sf_contract()
})

sf_names <- function(kind) {
  aliases <- contract[[kind]]$aliases
  vapply(contract[[kind]]$order, function(f) aliases[[f]][[1]], character(1))
}

transform_crawl <- function(cr) {
  lk <- copy(cr$links)
  for (col in intersect(url_cols_links, names(lk))) {
    set(lk, j = col, value = map_col(lk[[col]]))
  }
  if ("Anchor" %in% names(lk)) {
    set(lk, j = "Anchor", value = rewrite_text(
      lk[["Anchor"]], lk[["Destination"]], "link to "
    ))
  }
  if ("Alt Text" %in% names(lk)) {
    set(lk, j = "Alt Text", value = rewrite_text(
      lk[["Alt Text"]], lk[["Destination"]], "image of "
    ))
  }
  for (col in c("Source Segments", "Destination Segments")) {
    if (col %in% names(lk)) {
      src <- if (col == "Source Segments") {
        lk[["Source"]]
      } else {
        lk[["Destination"]]
      }
      set(lk, j = col, value = vapply(
        src, segments_of, character(1), USE.NAMES = FALSE
      ))
    }
  }
  lk <- lk[, intersect(sf_names("links"), names(lk)), with = FALSE]

  it <- copy(cr$internal)
  for (col in intersect(url_cols_internal, names(it))) {
    set(it, j = col, value = map_col(it[[col]]))
  }
  if ("Segments" %in% names(it)) {
    set(it, j = "Segments", value = vapply(
      it[["Address"]], segments_of, character(1), USE.NAMES = FALSE
    ))
  }
  # Free text carries the site's identity as directly as the URLs do.
  free_text <- c(
    "Title 1", "Meta Description 1", "H1-1", "H2-1",
    "Meta Keywords 1", "Author 1", "Category 1"
  )
  for (col in intersect(free_text, names(it))) {
    set(it, j = col, value = "")
  }
  it <- it[, intersect(sf_names("internal"), names(it)), with = FALSE]

  list(links = lk, internal = it)
}

out <- lapply(crawls, transform_crawl)

# ---- scrub gate -----------------------------------------------------------

# Fail loudly rather than write a fixture that leaks. Every real host token and
# every real path segment must be absent from every written cell.
real_tokens <- unique(c(
  unlist(strsplit(c(host_primary, host_secondary), ".", fixed = TRUE)),
  unlist(lapply(unique(parts$path), split_path))
))
real_tokens <- real_tokens[nchar(real_tokens) >= 4L]
real_tokens <- setdiff(real_tokens, c("html", "http", "https", "index"))
# Locale codes are shared vocabulary rather than identity. A site whose paths
# are locale-prefixed leaves the same string in the page's declared Language,
# which is true of much of the web and reveals nothing once the path itself is
# synthetic. Exempting the token, not the column, keeps the gate's reach.
is_locale <- grepl(
  "^[a-z]{2}(-[a-z]{2})?$", real_tokens,
  ignore.case = TRUE
)
real_tokens <- real_tokens[!is_locale]

# Match tokens at word boundaries rather than as bare substrings. A real path
# segment leaking into a URL is always delimited (by "/", "-", "?" or a quote),
# whereas DOM component names legitimately embed the same letters without one:
# "features" inside @class='featurestack' is structure, not identity. Substring
# matching cannot tell those apart and reported three false positives before
# this change. The residual gap — a leak concatenated into a longer token with
# no delimiter — is not reachable here, because the transform regenerates URLs,
# anchors and alt text from the dictionary instead of editing the originals.
token_hit <- function(t, hay) {
  grepl(paste0("(^|[^a-z0-9])\\Q", t, "\\E($|[^a-z0-9])"), hay, perl = TRUE)
}

check_leaks <- function(dt, label) {
  hay <- tolower(paste(unlist(lapply(dt, as.character)), collapse = "\n"))
  # Mask the vocabulary this script itself introduced before looking for leaks.
  # The pseudonym hosts are chosen for readability, so they can legitimately
  # contain a word that is also a real path segment ("reviews"); scanning them
  # reports the pseudonym as though it were the thing it replaced.
  for (h in tolower(host_map)) hay <- gsub(h, " ", hay, fixed = TRUE)
  hit <- vapply(tolower(real_tokens), token_hit, logical(1), hay = hay)
  hits <- real_tokens[hit]
  if (length(hits) > 0L) {
    # Report which column each token survived in: a hit in a URL-bearing column
    # is a transform bug, whereas a hit in page metadata is shared vocabulary
    # (a locale code, a MIME type) and needs a different judgement.
    where <- vapply(hits, function(t) {
      cols <- names(dt)[vapply(dt, function(v) {
        v <- tolower(as.character(v))
        for (h in tolower(host_map)) v <- gsub(h, " ", v, fixed = TRUE)
        any(token_hit(tolower(t), v))
      }, logical(1))]
      paste0(t, " [", toString(cols), "]")
    }, character(1))
    detail <- paste(utils::head(where, 20), collapse = "\n  ")
    stop("scrub gate failed for ", label, ":\n  ", detail, call. = FALSE)
  }
  invisible(NULL)
}

for (nm in names(out)) {
  check_leaks(out[[nm]]$links, paste(nm, "all_inlinks"))
  check_leaks(out[[nm]]$internal, paste(nm, "internal_all"))
}

# ---- write ----------------------------------------------------------------

dir.create(out_before, recursive = TRUE, showWarnings = FALSE)
dir.create(out_after,  recursive = TRUE, showWarnings = FALSE)

fwrite(out$before$links,    file.path(out_before, "all_inlinks.csv"))
fwrite(out$before$internal, file.path(out_before, "internal_all.csv"))
fwrite(out$after$links,     file.path(out_after,  "all_inlinks.csv"))
fwrite(out$after$internal,  file.path(out_after,  "internal_all.csv"))

cat("wrote fixture\n")
for (d in c(out_before, out_after)) {
  for (f in list.files(d, full.names = TRUE)) {
    cat(sprintf("  %-52s %8.1f KB\n", f, file.size(f) / 1024))
  }
}
cat("distinct URLs mapped: ", nrow(parts), "\n", sep = "")
cat("hosts mapped:         ", length(host_map), "\n", sep = "")
