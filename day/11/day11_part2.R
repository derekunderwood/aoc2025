rm(list = ls())
input <- readLines("day/11/input.txt")

# lines like: "node: a b c" or "node:" (robust to extra spaces)
parse_line <- function(line) {
  parts <- strsplit(line, ":\\s*")[[1]]
  node <- parts[1]
  nbrs <- character(0)
  if (length(parts) > 1 && nzchar(parts[2])) {
    nbrs <- strsplit(parts[2], "\\s+")[[1]]
  }
  list(node = node, neighbors = nbrs)
}

entries <- lapply(input, parse_line)
adj <- setNames(lapply(entries, `[[`, "neighbors"),
                sapply(entries, `[[`, "node"))

# Ensure all mentioned nodes exist in adj
all_nodes <- unique(c(names(adj), unlist(adj, use.names = FALSE)))
for (n in setdiff(all_nodes, names(adj))) adj[[n]] <- character(0)

# ---------- Name <-> ID mapping ----------
nodes <- names(adj)
id_of   <- setNames(seq_along(nodes), nodes)
name_of <- setNames(nodes, seq_along(nodes))

adj_id <- lapply(adj, function(nbrs) {
  if (length(nbrs) == 0) integer(0) else as.integer(id_of[nbrs])
})
names(adj_id) <- NULL  # index by integer position

# ---------- Helpers ----------
# Reverse reachability: which nodes can reach 'target'?
compute_reachable_to <- function(adj_id, target_id) {
  N <- length(adj_id)
  rev_adj <- vector("list", N)
  for (u in seq_len(N)) for (v in adj_id[[u]]) rev_adj[[v]] <- c(rev_adj[[v]], u)
  can <- rep(FALSE, N)
  q <- c(target_id); can[target_id] <- TRUE; head <- 1L
  while (head <= length(q)) {
    curr <- q[head]; head <- head + 1L
    for (p in rev_adj[[curr]]) if (!can[p]) { can[p] <- TRUE; q <- c(q, p) }
  }
  can
}

# Kahn's algorithm: topological order and DAG check
dag_topo <- function(adj_id) {
  N <- length(adj_id)
  indeg <- integer(N)
  for (u in seq_len(N)) for (v in adj_id[[u]]) indeg[v] <- indeg[v] + 1L
  q <- which(indeg == 0L)
  topo <- integer(0)
  head <- 1L
  while (head <= length(q)) {
    u <- q[head]; head <- head + 1L
    topo <- c(topo, u)
    for (v in adj_id[[u]]) {
      indeg[v] <- indeg[v] - 1L
      if (indeg[v] == 0L) q <- c(q, v)
    }
  }
  list(is_dag = (length(topo) == N), topo = topo)
}

# ---------- Counting in DAG (fast DP) ----------
# States: bit 1 => seen 'dac', bit 2 => seen 'fft' (0..3)
count_paths_dag <- function(start_id, goal_id, dac_id, fft_id, adj_id, topo) {
  N <- length(adj_id)
  dp <- matrix(0.0, nrow = N, ncol = 4)
  s0 <- 0L
  if (start_id == dac_id) s0 <- bitwOr(s0, 1L)
  if (start_id == fft_id) s0 <- bitwOr(s0, 2L)
  dp[start_id, s0 + 1L] <- 1.0  # +1 for R's 1-based indexing
  
  for (u in topo) {
    for (state in 0:3) {
      ways <- dp[u, state + 1L]
      if (ways == 0) next
      for (v in adj_id[[u]]) {
        ns <- state
        if (v == dac_id) ns <- bitwOr(ns, 1L)
        if (v == fft_id) ns <- bitwOr(ns, 2L)
        dp[v, ns + 1L] <- dp[v, ns + 1L] + ways
      }
    }
  }
  dp[goal_id, 3 + 1L]  # state==3 means both dac & fft seen
}

# ---------- Counting in general graph (pruned DFS, simple paths only) ----------
count_paths_general <- function(start_id, goal_id, dac_id, fft_id, adj_id,
                                can_out, can_dac, can_fft) {
  total <- 0L
  stack <- list(list(path = c(start_id),
                     curr = start_id,
                     has_a = (start_id == dac_id),
                     has_b = (start_id == fft_id)))
  while (length(stack) > 0) {
    node <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    path <- node$path
    curr <- node$curr
    has_a <- node$has_a
    has_b <- node$has_b
    
    if (curr == goal_id) {
      if (has_a && has_b) total <- total + 1L
      next
    }
    
    # Expand neighbors with pruning
    nbrs <- adj_id[[curr]]
    if (length(nbrs) == 0) next
    # Must be able to reach goal, and still able to include missing dac/fft
    keep <- can_out[nbrs] & (has_a | can_dac[nbrs]) & (has_b | can_fft[nbrs])
    nbrs <- nbrs[keep]
    if (length(nbrs) == 0) next
    
    for (nbr in nbrs) {
      # Simple path constraint: avoid revisiting nodes already in the path
      if (nbr %in% path) next
      stack[[length(stack) + 1]] <- list(
        path = c(path, nbr),
        curr = nbr,
        has_a = (has_a || (nbr == dac_id)),
        has_b = (has_b || (nbr == fft_id))
      )
    }
  }
  total
}

# ---------- Configure your query ----------
start_device <- "svr"
goal_device  <- "out"
must_a <- "dac"
must_b <- "fft"

# IDs
start_id <- id_of[start_device]
goal_id  <- id_of[goal_device]
dac_id   <- id_of[must_a]
fft_id   <- id_of[must_b]

# Precompute reachability for pruning (used in general graph case)
can_out <- compute_reachable_to(adj_id, goal_id)
can_dac <- compute_reachable_to(adj_id, dac_id)
can_fft <- compute_reachable_to(adj_id, fft_id)

# ---------- Run ----------
topo_info <- dag_topo(adj_id)
if (topo_info$is_dag) {
  cat("Graph is a DAG. Using fast DP…\n")
  count <- count_paths_dag(start_id, goal_id, dac_id, fft_id, adj_id, topo_info$topo)
} else {
  cat("Graph has cycles. Using pruned DFS (may be expensive)…\n")
  count <- count_paths_general(start_id, goal_id, dac_id, fft_id, adj_id,
                               can_out, can_dac, can_fft)
}
cat(sprintf("Count of simple paths from '%s' to '%s' that include '%s' and '%s': %s\n",
            start_device, goal_device, must_a, must_b, format(count, scientific = FALSE)))
