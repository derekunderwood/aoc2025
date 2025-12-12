rm(list = ls())
input <- readLines("day/11/input.txt")

list <- list()
for (i in 1:length(input)) {
  parts <- unlist(strsplit(input[i], " "))
  
  device <- gsub(":", "", parts[1])
  connected <- parts[2:length(parts)]
  
  parts_list <- list(device)
  parts_list <- c(parts_list[[1]], list(connected))
  list <- c(list, list(parts_list))
}

# --- Build adjacency map from your 2-element list structure ---
devices      <- vapply(list, function(x) x[[1]], character(1))
connections  <- lapply(list, function(x) x[[2]])
adj <- setNames(connections, devices)

# Make sure all mentioned nodes appear in adj (even those with no outgoing edges)
all_nodes <- unique(c(names(adj), unlist(adj, use.names = FALSE)))
for (n in setdiff(all_nodes, names(adj))) adj[[n]] <- character(0)

# --- DFS to enumerate all simple paths from a start node ---
dfs_paths <- function(start, adj, max_depth = Inf, simple = TRUE, is_goal = NULL) {
  paths <- list()
  stack <- list(c(start))  # each element is a vector representing a path
  
  while (length(stack) > 0) {
    path <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    current <- tail(path, 1)
    
    # If we have a goal predicate, capture when met
    if (!is.null(is_goal) && is_goal(current, path)) {
      paths[[length(paths) + 1]] <- path
      # continue exploring further only if outcome should allow longer paths
      # If outcome should stop, add 'next' here
    }
    
    # Stop expansion if max depth reached
    if (length(path) >= max_depth) next
    
    # Expand neighbors
    for (nbr in adj[[current]]) {
      if (simple && nbr %in% path) next  # avoid cycles
      stack[[length(stack) + 1]] <- c(path, nbr)
    }
  }
  
  paths
}

# --- Example usage ---
start_device <- "you"

# Define outcome as reaching device "F"
is_goal <- function(node, path) node == "out"

all_paths_to_F <- dfs_paths(start_device, adj, max_depth = Inf, simple = TRUE, is_goal = is_goal)

all_paths_to_F
length(all_paths_to_F)
