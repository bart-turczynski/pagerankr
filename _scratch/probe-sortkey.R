#!/usr/bin/env Rscript
# What is the All Inlinks export actually sorted by? If row order is a
# destination grouping, it carries no per-source document order at all.
suppressMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
d <- fread(args[[1]], select = c("Type", "Source", "Destination", "Link Path"),
           showProgress = FALSE)
setnames(d, c("type", "src", "dst", "lpath"))
cat("\n==== ", args[[2]], " ====\n", sep = "")
cat("all rows            :", nrow(d), "\n")
mono <- function(x) mean(head(x, -1) <= tail(x, -1))
cat("rows sorted by dst  :", sprintf("%.1f%%", 100 * mono(d$dst)), "\n")
cat("rows sorted by src  :", sprintf("%.1f%%", 100 * mono(d$src)), "\n")
cat("rows sorted by type :", sprintf("%.1f%%", 100 * mono(d$type)), "\n")
# runs: if grouped by destination, each dst is one contiguous block
cat("distinct dst        :", uniqueN(d$dst), "\n")
cat("dst runs (blocks)   :", sum(d$dst != shift(d$dst, fill = "")), "\n")
cat("distinct src        :", uniqueN(d$src), "\n")
cat("src runs (blocks)   :", sum(d$src != shift(d$src, fill = "")), "\n")
cat("\n")
