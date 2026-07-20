#!/usr/bin/env Rscript
# Independent check that All Outlinks row order is DOCUMENT order: if it is,
# a link's normalized position within its source page should order the page
# regions the way a reader meets them -- header/nav first, footer last.
suppressMessages(library(data.table))
d <- fread("_scratch/crawls/tidio/all_outlinks.csv",
           select = c("Type", "Source", "Destination", "Link Position"),
           showProgress = FALSE)
setnames(d, c("type", "src", "dst", "lpos"))
d <- d[type == "Hyperlink"]
d[, pos := seq_len(.N), by = src]        # rank within source, file order
d[, n := .N, by = src]
d <- d[n > 5]
d[, rel := (pos - 1) / (n - 1)]          # 0 = first link on page, 1 = last

cat("\n-- mean normalized position by region (0 = page top) --\n")
print(d[lpos != "", .(links = .N, mean_rel = round(mean(rel), 3),
                      median_rel = round(median(rel), 3)), by = lpos][order(mean_rel)])

cat("\n-- where content links sit, decile of page --\n")
print(d[lpos == "Content", .N, by = .(decile = pmin(9L, floor(rel * 10)))][order(decile)])
cat("\n")
