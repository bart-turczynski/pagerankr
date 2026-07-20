suppressMessages(devtools::load_all("/Users/bartturczynski/Projects/pagerankr", quiet = TRUE))

# A small "real" site: a hub plus 20 real pages, interlinked in a ring.
real <- sprintf("https://ex.com/p%02d", 1:20)
hub <- "https://ex.com/"
base_edges <- rbind(
  data.frame(from = hub, to = real, stringsAsFactors = FALSE),
  data.frame(from = real, to = c(real[-1], real[1]), stringsAsFactors = FALSE),
  data.frame(from = real, to = hub, stringsAsFactors = FALSE)
)

# K fake dead URLs, each discovered via exactly one link from the hub.
# This is the realistic shape: a crawler only finds a URL because something
# links to it. The dead pages have NO outgoing links (a 404 has none).
make_edges <- function(k) {
  if (k == 0) {
    return(base_edges)
  }
  dead <- sprintf("https://ex.com/dead%04d", seq_len(k))
  rbind(base_edges, data.frame(from = hub, to = dead, stringsAsFactors = FALSE))
}
dead_urls <- function(k) {
  if (k == 0) character(0) else sprintf("https://ex.com/dead%04d", seq_len(k))
}

run <- function(k, exclude_teleport = FALSE) {
  e <- make_edges(k)
  d <- dead_urls(k)
  args <- list(edge_list_df = e, clean_edge_urls = FALSE)
  if (exclude_teleport && k > 0) {
    # Teleport only to live pages: weight 1 for real, 0 for dead.
    live <- setdiff(unique(c(e$from, e$to)), d)
    args$prior_df <- data.frame(
      url = c(live, d),
      weight = c(rep(1, length(live)), rep(0, length(d))),
      stringsAsFactors = FALSE
    )
    args$prior_alpha <- 0 # 0 = pure prior teleport; 1 would be uniform
    args$prior_verbose <- FALSE
  }
  res <- do.call(pagerank, args)
  list(res = res, dead = d)
}

summarize <- function(k, exclude_teleport = FALSE) {
  out <- run(k, exclude_teleport)
  res <- out$res
  is_dead <- res[[1]] %in% out$dead
  data.frame(
    k = k,
    teleport = if (exclude_teleport) "excluded" else "uniform",
    n_nodes = nrow(res),
    dead_total = sum(res[[2]][is_dead]),
    hub = res[[2]][res[[1]] == hub],
    real_p01 = res[[2]][res[[1]] == real[1]],
    real_total = sum(res[[2]][res[[1]] %in% real]),
    stringsAsFactors = FALSE
  )
}

ks <- c(0, 10, 100, 1000)
cat("=== UNIFORM TELEPORT (current behavior) ===\n")
u <- do.call(rbind, lapply(ks, summarize, exclude_teleport = FALSE))
print(u, row.names = FALSE, digits = 4)

cat("\n=== TELEPORT EXCLUDING DEAD PAGES ===\n")
x <- do.call(rbind, lapply(ks, summarize, exclude_teleport = TRUE))
print(x, row.names = FALSE, digits = 4)

cat("\n=== PER-DEAD-PAGE SCORE, and teleport's share of it ===\n")
for (k in ks[-1]) {
  ru <- run(k, FALSE)
  rx <- run(k, TRUE)
  du <- ru$res[[2]][ru$res[[1]] %in% ru$dead][1]
  dx <- rx$res[[2]][rx$res[[1]] %in% rx$dead][1]
  cat(sprintf(
    "k=%4d  uniform=%.3e  excluded=%.3e  teleport share of dead score=%5.1f%%\n",
    k, du, dx, 100 * (du - dx) / du
  ))
}

cat("\n=== CONVERGENCE CHECK with zeroed teleport entries ===\n")
for (k in ks[-1]) {
  rx <- run(k, TRUE)
  cat(sprintf(
    "k=%4d  sum=%.10f  any NA=%s  any negative=%s  min=%.3e\n",
    k, sum(rx$res[[2]]), any(is.na(rx$res[[2]])),
    any(rx$res[[2]] < 0), min(rx$res[[2]])
  ))
}
