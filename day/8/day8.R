rm(list=ls())
# Read as a matrix of numeric coordinates
coords <- do.call(rbind, lapply(readLines("day/8/input.txt"), function(s) as.numeric(strsplit(s, ",")[[1]])))

# Compute pairwise Euclidean distances (condensed form)
dcond <- dist(coords)

# Convert to full matrix for easy indexing (optional)
D <- as.matrix(dcond)
N <- nrow(coords)

# Get indices for upper triangle pairs (i < j)
idx <- which(upper.tri(D), arr.ind = TRUE)  # matrix with columns (row=i, col=j)

# Assemble result efficiently
sort_df <- data.frame(
  distance = D[idx],
  c1 = apply(coords[idx[,1], , drop = FALSE], 1, function(v) paste(v, collapse = ",")),
  c2 = apply(coords[idx[,2], , drop = FALSE], 1, function(v) paste(v, collapse = ","))
)

# If you need numeric distance type:
sort_df$distance <- as.numeric(sort_df$distance)

idx <- order(sort_df[, 1])   # indices of sorted order
sorted_distances <- sort_df[idx, ]  # reorder rows

list_id <- 1
circuits <- character()          # flat vector of all members seen so far
circuit_list <- list()           # list of circuits (each is a character vector of IDs)

# Helper: find which circuit index contains a given id (or NA if none)
which_circuit <- function(id, circuit_list) {
  hits <- which(vapply(circuit_list, function(v) id %in% v, logical(1)))
  if (length(hits) == 0) return(NA_integer_)
  hits[1]
}

df <- head(sorted_distances, 1000)

for (j in seq_len(nrow(df))) {
  x <- sorted_distances$c1[j]
  y <- sorted_distances$c2[j]
  vals <- c(x, y)
  
  # Are they already seen?
  add <- vals[!vals %in% circuits]
  
  if (length(add) == 2) {
    # Both new: start a new circuit with these two
    circuit_list[[list_id]] <- add
    circuits <- c(circuits, add)
    list_id <- list_id + 1
    
  } else if (length(add) == 1) {
    # One new, one existing: add the new one to the existing circuit
    existing <- vals[vals %in% circuits]
    ci <- which_circuit(existing, circuit_list)
    circuit_list[[ci]] <- unique(c(circuit_list[[ci]], add))
    circuits <- c(circuits, add)
    
  } else {
    # Both exist: either same circuit (skip) or different circuits (merge)
    ci_x <- which_circuit(x, circuit_list)
    ci_y <- which_circuit(y, circuit_list)
    
    if (is.na(ci_x) || is.na(ci_y)) {
      # Defensive: if either isn’t found, treat as new (shouldn't happen if 'circuits' is maintained)
      next
    }
    
    if (ci_x == ci_y) {
      # Already in same circuit -> skip
      next
    } else {
      # Merge circuits: append ci_y into ci_x and remove ci_y
      merged <- unique(c(circuit_list[[ci_x]], circuit_list[[ci_y]]))
      circuit_list[[ci_x]] <- merged
      circuit_list <- circuit_list[-ci_y]
      
      # Note: when you remove an element, indices shift; no need to adjust list_id separately
      circuits <- unique(circuits)  # optional, keeps flat vector tidy
    }
  }
}

str(circuit_list)
circuit_sizes <- sapply(circuit_list, length)
top_values <- sort(circuit_sizes, decreasing = TRUE)[1:3]
prod(top_values)
