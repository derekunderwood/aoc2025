# Build universe of nodes
nodes <- unique(c(sorted_distances$c1, sorted_distances$c2))
nodes <- as.character(nodes)
N <- length(nodes)

# Parent and rank for union-find
parent <- setNames(nodes, nodes)  # parent[x] = x initially
rank   <- setNames(integer(N), nodes)

find <- function(x) {
  # Path compression
  if (parent[[x]] != x) {
    parent[[x]] <<- find(parent[[x]])
  }
  parent[[x]]
}

union <- function(x, y) {
  rx <- find(x)
  ry <- find(y)
  if (rx == ry) return(FALSE)  # no merge happened
  
  # Union by rank
  if (rank[[rx]] < rank[[ry]]) {
    parent[[rx]] <<- ry
  } else if (rank[[rx]] > rank[[ry]]) {
    parent[[ry]] <<- rx
  } else {
    parent[[ry]] <<- rx
    rank[[rx]] <<- rank[[rx]] + 1
  }
  TRUE  # merge happened
}

# Count distinct sets (components)
n_components <- function() {
  length(unique(vapply(nodes, find, character(1))))
}

last_c1_c2 <- NULL

for (j in seq_len(nrow(sorted_distances))) {
  x <- as.character(sorted_distances$c1[j])
  y <- as.character(sorted_distances$c2[j])
  
  merged <- union(x, y)
  if (merged) {
    last_c1_c2 <- c(c1 = x, c2 = y)
    if (n_components() == 1) {
      # All connected; stop early
      break
    }
  }
}

# last_c1_c2 now holds the last pair that connected two previously separate components.
last_c1_c2

x1 <- unlist(strsplit(last_c1_c2[1], ","))[1]
x2 <- unlist(strsplit(last_c1_c2[2], ","))[1]

as.numeric(x1)*as.numeric(x2)
