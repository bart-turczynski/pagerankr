#!/usr/bin/env Rscript
# PAGE-xzxntstl probe 2: the axis only matters where chrome does not already
# dominate, i.e. among Content links on a source page. Ask three things:
#   1. Of content-link pairs on a page, what share are orderable by DOM path?
#   2. Is the order a total order, or does it fragment into incomparable blocks?
#   3. How many rank levels would a decay actually have to spread over?
suppressMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
d <- fread(args[[1]], select = c("Type", "Source", "Destination", "Link Path", "Link Position"),
           showProgress = FALSE)
setnames(d, c("type", "src", "dst", "lpath", "lpos"))
d <- d[type == "Hyperlink" & lpos == "Content" & lpath != ""]
cat("\n==== ", args[[2]], " (content links only) ====\n", sep = "")
cat("content hyperlinks :", nrow(d), "\n")
cat("source pages       :", uniqueN(d$src), "\n")

set.seed(1)
srcs <- unique(d$src)
samp <- if (length(srcs) > 400) sample(srcs, 400) else srcs
s <- d[src %chin% samp]

tag_of <- function(x) sub("\\[[0-9]+\\]$", "", x)
idx_of <- function(x) {
  m <- regexpr("\\[[0-9]+\\]$", x)
  out <- rep(1L, length(x)); hit <- m > 0
  out[hit] <- as.integer(gsub("[][]", "", regmatches(x, m)))
  out
}
steps <- strsplit(sub("^//", "", gsub("\\[@[^]]*\\]", "", s$lpath)), "/", fixed = TRUE)

out <- rbindlist(lapply(split(seq_len(nrow(s)), s$src), function(ix) {
  if (length(ix) > 80) ix <- ix[seq_len(80)]
  n <- length(ix)
  if (n < 2) return(NULL)
  st <- steps[ix]
  cmp <- 0L; tot <- 0L
  # also: the "branch key" = path up to the first step carrying an ordinal we
  # could rank on. Links sharing a branch key are mutually orderable.
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    a <- st[[i]]; b <- st[[j]]
    k <- min(length(a), length(b))
    diffs <- which(a[seq_len(k)] != b[seq_len(k)])
    tot <- tot + 1L
    if (!length(diffs)) { cmp <- cmp + 1L; next }
    if (tag_of(a[[diffs[[1]]]]) == tag_of(b[[diffs[[1]]]])) cmp <- cmp + 1L
  }
  # distinct ordinal levels available on this page (how much room a decay has)
  lev <- uniqueN(vapply(st, function(p) paste(idx_of(p), collapse = "."), ""))
  data.table(src = s$src[ix[1]], n_links = n, pairs = tot, comparable = cmp, levels = lev)
}))

cat("\n-- comparability of content-link pairs --\n")
cat("pages examined       :", nrow(out), "\n")
cat("pairs                :", sum(out$pairs), "\n")
cat("orderable by DOM path:", sprintf("%.1f%%", 100 * sum(out$comparable) / sum(out$pairs)), "\n")
cat("pages fully ordered  :", sprintf("%.1f%%", 100 * mean(out$comparable == out$pairs)), "\n")

cat("\n-- per-page: content links vs distinct ordinal levels --\n")
print(out[, .(links = round(quantile(n_links, c(.25, .5, .75, .9))),
              levels = round(quantile(levels, c(.25, .5, .75, .9))),
              q = c("25%", "50%", "75%", "90%"))])

# 3. Does the DEPTH-1 container ordinal (e.g. p[5] under the article) exist at all?
s[, leaf_par := sub("/[^/]+$", "", gsub("\\[@[^]]*\\]", "", lpath))]
s[, par_tail := sub("^.*/", "", leaf_par)]
cat("\n-- immediate parent of a content link, top 10 --\n")
print(s[, .N, by = par_tail][order(-N)][seq_len(min(10, .N))])
cat("\nparent carries an ordinal:",
    sprintf("%.1f%%", 100 * mean(grepl("[[][0-9]+[]]$", s$par_tail))), "\n\n")
