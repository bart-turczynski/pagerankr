#!/usr/bin/env Rscript
# Do the two exports describe the same graph? If they do, order can be taken
# from All Outlinks without changing any score. Aggregates only.
suppressMessages(library(data.table))
cols <- c("Type", "Source", "Destination", "Link Path", "Link Position")

rd <- function(p) {
  d <- fread(p, select = cols, showProgress = FALSE)
  setnames(d, c("type", "src", "dst", "lpath", "lpos"))
  d[type == "Hyperlink"]
}
i <- rd("_scratch/crawls/tidio/all_inlinks.csv")
o <- rd("_scratch/crawls/tidio/all_outlinks.csv")

cat("\n==== edge-set comparison: tidio inlinks vs outlinks ====\n")
cat("hyperlink rows  in :", nrow(i), " out:", nrow(o), "\n")
cat("distinct sources in:", uniqueN(i$src), " out:", uniqueN(o$src), "\n")
cat("distinct targets in:", uniqueN(i$dst), " out:", uniqueN(o$dst), "\n")

# Full row identity, ignoring order.
# NB: do NOT setkey() on i/o -- setkey sorts in place and would destroy the file
# order that the sibling test below depends on. This is exactly the "order is
# fragile in transit" hazard, and it silently corrupted an earlier run of this
# script. by= does not reorder the parent table.
ci <- i[, .N, by = .(src, dst, lpath, lpos)]
co <- o[, .N, by = .(src, dst, lpath, lpos)]
m <- merge(ci, co, by = c("src", "dst", "lpath", "lpos"), all = TRUE)
cat("\nrows only in inlinks :", sum(is.na(m$N.y)), "\n")
cat("rows only in outlinks:", sum(is.na(m$N.x)), "\n")
cat("multiplicity mismatch:", sum(!is.na(m$N.x) & !is.na(m$N.y) & m$N.x != m$N.y), "\n")

# the graph pagerank() would actually see: distinct src->dst pairs
ei <- unique(i[, .(src, dst)]); eo <- unique(o[, .(src, dst)])
cat("\ndistinct src->dst edges in :", nrow(ei), " out:", nrow(eo), "\n")
cat("identical edge sets        :",
    identical(nrow(fsetdiff(ei, eo)), 0L) && identical(nrow(fsetdiff(eo, ei)), 0L), "\n")

# ---- characterize the 1.7% of sibling groups that are NOT monotone ----------
tag_of <- function(x) sub("\\[[0-9]+\\]$", "", x)
idx_of <- function(x) {
  m <- regexpr("\\[[0-9]+\\]$", x)
  out <- rep(1L, length(x)); hit <- m > 0
  out[hit] <- as.integer(gsub("[][]", "", regmatches(x, m)))
  out
}
set.seed(1)
srcs <- unique(o$src); samp <- sample(srcs, min(300, length(srcs)))
s <- o[src %chin% samp & lpath != ""]
s[, ord := seq_len(.N)]
s[, clean := gsub("\\[@[^]]*\\]", "", lpath)]
s[, parent := sub("/[^/]+$", "", clean)]
s[, leaf := sub("^.*/", "", clean)]
s[, leaf_tag := tag_of(leaf)][, leaf_idx := idx_of(leaf)]

g <- s[, .(n = .N, ok = all(diff(leaf_idx) >= 0), pos = paste(sort(unique(lpos)), collapse = "+")),
       by = .(src, parent, leaf_tag)][n > 1]
cat("\n-- non-monotone sibling groups, by link position --\n")
print(g[, .(groups = .N, monotone = round(100 * mean(ok), 1)), by = pos][order(-groups)])

# are the offenders duplicated targets (same link emitted twice) rather than reorder?
bad <- g[ok == FALSE]
cat("\nnon-monotone groups:", nrow(bad), "of", nrow(g), "\n")
if (nrow(bad)) {
  b <- merge(s, bad[, .(src, parent, leaf_tag)], by = c("src", "parent", "leaf_tag"))
  cat("of those rows, share whose leaf carries NO ordinal (implicit 1):",
      sprintf("%.1f%%", 100 * mean(!grepl("[[][0-9]+[]]$", b$leaf))), "\n")
  cat("share that are repeat targets within the group:",
      sprintf("%.1f%%", 100 * mean(duplicated(b[, .(src, parent, leaf_tag, dst)]))), "\n")
}
cat("\n")
