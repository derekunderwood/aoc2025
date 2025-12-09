M <- z

count_paths_numeric <- function(M) {
  stopifnot(is.matrix(M))
  n <- nrow(M); p <- ncol(M)
  
  # Start column (assumes single 'S' in the top row)
  sc <- which(M[1, ] == "S")
  if (length(sc) != 1) stop("Need exactly one 'S' in the top row.")
  
  # Use doubles (numeric) to avoid integer overflow
  counts <- numeric(p)
  counts[sc] <- 1.0
  
  r <- 1L
  while (r < n) {
    next_counts <- numeric(p)
    
    # Straight down moves: add counts where row r+1 has '|'
    stream_cols <- which(M[r + 1L, ] == "|")
    if (length(stream_cols)) {
      next_counts[stream_cols] <- next_counts[stream_cols] + counts[stream_cols]
    }
    
    # Caret splits: move to row r+2, c-1 and/or c+1 if those are '|'
    caret_cols <- which(M[r + 1L, ] == "^")
    if (length(caret_cols) && (r + 1L < n)) {
      for (c in caret_cols) {
        # LEFT
        if (c - 1L >= 1L && M[r + 2L, c - 1L] == "|")
          next_counts[c - 1L] <- next_counts[c - 1L] + counts[c]
        # RIGHT
        if (c + 1L <= p && M[r + 2L, c + 1L] == "|")
          next_counts[c + 1L] <- next_counts[c + 1L] + counts[c]
      }
      # Skip the caret row (movement materializes one row below it)
      r <- r + 2L
    } else {
      # Normal straight-down step
      r <- r + 1L
    }
    counts <- next_counts
  }
  
  # Total paths reaching the bottom
  sum(counts)
}

count_paths_numeric(M)
