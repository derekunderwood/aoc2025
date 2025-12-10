rm(list=ls())
file_path <- "day/9/input.txt"
lines <- readLines(file_path, warn = FALSE)

parts <- strsplit(lines, ",", fixed = TRUE)
coords <- do.call(
  rbind,
  lapply(parts, function(x) as.numeric(trimws(x)))
)

# Validate
if (!is.matrix(coords)) stop("Failed to parse coordinates into a matrix.")
if (ncol(coords) != 2L) stop("Input must have exactly 2 numeric columns (x,y).")
if (anyNA(coords)) stop("Found NA in coordinates; check for malformed lines or non-numeric tokens.")

n <- nrow(coords)
x <- coords[, 1]
y <- coords[, 2]

# Create upper-tri index pairs once
upper_pairs <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
# Compute per-pair rectangle counts
dx <- abs(x[upper_pairs[,1]] - x[upper_pairs[,2]])
dy <- abs(y[upper_pairs[,1]] - y[upper_pairs[,2]])
rect_counts <- (dx + 1L) * (dy + 1L)

# Find the maximum rectangle grid-count and the corresponding pair
idx_max_rect <- which.max(rect_counts)
i_r <- upper_pairs[idx_max_rect, 1]
j_r <- upper_pairs[idx_max_rect, 2]
p1_r <- coords[i_r, ]; p2_r <- coords[j_r, ]
max_rect_grid_count <- rect_counts[idx_max_rect]
