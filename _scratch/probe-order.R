#!/usr/bin/env Rscript
# PAGE-xzxntstl probe 1: is link order recoverable?
# Two candidate carriers:
#   (A) CSV row order within a source page
#   (B) numeric predicates in the DOM path (//body/div/main/article/p[5]/a[1])
# Prints aggregates only. Never returns row data.

suppressMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
path <- args[[1]]
label <- args[[2]]

cols <- c("Type", "Source", "Destination", "Link Path", "Link Position")
d <- fread(path, select = cols, showProgress = FALSE)
setnames(d, c("type", "src", "dst", "lpath", "lpos"))
d <- d[type == "Hyperlink"]
d[, row_ord := seq_len(.N)]           # CSV row order, as ingested
d[, row_in_src := rowid(src)]         # position within source, by CSV order

cat("\n================ ", label, " ================\n", sep = "")
cat("hyperlink rows      :", nrow(d), "\n")
cat("distinct sources    :", uniqueN(d$src), "\n")
cat("empty link_path     :", sprintf("%.3f%%", 100 * mean(d$lpath == "")), "\n")
cat("path has numeric [n]:", sprintf("%.1f%%", 100 * mean(grepl("[[][0-9]+[]]", d$lpath))), "\n")

# ---- (B) how far does the path ordinal get us? ------------------------------
# A path is a sequence of steps. Compare two paths by walking steps in parallel:
# at the first differing step they are comparable ONLY if the tag names match
# (XPath indices are per-tag-name; p[5] vs ul[1] has no defined order).
# Measure: over sibling link pairs within a source, what share are comparable?

# strip class predicates, keep numeric ones; missing index means 1
norm_steps <- function(p) {
  p <- gsub("\\[@[^]]*\\]", "", p)
  strsplit(sub("^//", "", p), "/", fixed = TRUE)
}

# Sample sources to keep this cheap on the big crawls.
set.seed(1)
srcs <- unique(d$src)
samp <- if (length(srcs) > 300) sample(srcs, 300) else srcs
s <- d[src %chin% samp & lpath != ""]

steps <- norm_steps(s$lpath)
s[, nsteps := lengths(steps)]

tag_of <- function(x) sub("\\[[0-9]+\\]$", "", x)
idx_of <- function(x) {
  m <- regexpr("\\[[0-9]+\\]$", x)
  out <- rep(1L, length(x))
  hit <- m > 0
  out[hit] <- as.integer(gsub("[][]", "", regmatches(x, m)))
  out
}

# pairwise comparability, per source, capped at 60 links per source
res <- rbindlist(lapply(split(seq_len(nrow(s)), s$src), function(ix) {
  if (length(ix) > 60) ix <- ix[seq_len(60)]
  if (length(ix) < 2) return(NULL)
  st <- steps[ix]
  n <- length(ix)
  cmp <- 0L; tot <- 0L; agree <- 0L
  for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    a <- st[[i]]; b <- st[[j]]
    k <- min(length(a), length(b))
    eq <- which(a[seq_len(k)] != b[seq_len(k)])
    tot <- tot + 1L
    if (!length(eq)) { cmp <- cmp + 1L; agree <- agree + 1L; next }  # prefix: comparable
    p <- eq[[1]]
    if (tag_of(a[[p]]) == tag_of(b[[p]])) {
      cmp <- cmp + 1L
      # does DOM ordinal order agree with CSV row order?
      dom_lt <- idx_of(a[[p]]) < idx_of(b[[p]])
      csv_lt <- i < j
      if (dom_lt == csv_lt) agree <- agree + 1L
    }
  }
  data.table(src = s$src[ix[1]], pairs = tot, comparable = cmp, agree = agree)
}))

cat("\n-- (B) DOM-path ordinal as order carrier (", nrow(res), " sampled sources) --\n", sep = "")
cat("link pairs examined     :", sum(res$pairs), "\n")
cat("pairs comparable by path:", sprintf("%.1f%%", 100 * sum(res$comparable) / sum(res$pairs)), "\n")
cat("of comparable, DOM order agrees with CSV order:",
    sprintf("%.1f%%", 100 * sum(res$agree) / sum(res$comparable)), "\n")

# ---- (A) is CSV row order document order? -----------------------------------
# Cleanest test: same source, same parent path, same tag, differing only in the
# numeric index. Row order should be monotone in the index if CSV = document order.
s[, parent := sub("/[^/]+$", "", gsub("\\[@[^]]*\\]", "", lpath))]
s[, leaf := sub("^.*/", "", gsub("\\[@[^]]*\\]", "", lpath))]
s[, leaf_tag := tag_of(leaf)]
s[, leaf_idx := idx_of(leaf)]

mono <- s[, .(n = .N, ok = all(diff(leaf_idx) >= 0)),
          by = .(src, parent, leaf_tag)][n > 1]
cat("\n-- (A) CSV row order vs sibling DOM index --\n")
cat("sibling groups (n>1)    :", nrow(mono), "\n")
cat("groups in monotone order:", sprintf("%.1f%%", 100 * mean(mono$ok)), "\n")

# ---- distribution of links per source: what decay shape has room to act? ----
per_src <- d[, .N, by = src]$N
cat("\n-- links per source page --\n")
print(round(quantile(per_src, c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99))))

cnt <- d[lpos != "", .N, by = .(lpos)][order(-N)]
cat("\n-- link positions --\n"); print(cnt)

# how deep are content links specifically?
cd <- d[lpos == "Content", .(n = .N), by = src]$n
if (length(cd)) {
  cat("\n-- content links per source --\n")
  print(round(quantile(cd, c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99))))
}
cat("\n")
