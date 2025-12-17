rm(list=ls())
input <- readLines("day/12/input.txt")

# shapes in fixed position, regions start after line 31 of input

getShape <- function(y) {
  if (y == 1) {
    index <- c(2:4)
  } else if (y == 2) {
    index <- c(7:9)
  } else if (y == 3) {
    index <- c(12:14)
  } else if (y == 4) {
    index <- c(17:19)
  } else if (y == 5) {
    index <- c(22:24)
  } else {
    index <- c(27:29)
  }
  
  shape <- matrix(unlist(strsplit(input[index], "")), nrow = 3, byrow = T)
  shape <- ifelse(shape == "#", 1, 0)
  shape
  
}

getRegion <- function(x) {
  regions <- input[31:length(input)]
  parts <- strsplit(regions[x], ": ")
  qty <- as.numeric(unlist(strsplit(parts[[1]][2], " ")))
  
  region_shape <- unlist(strsplit(parts[[1]][1], "x"))
  
  wide <- as.numeric(region_shape[1])
  long <- as.numeric(region_shape[2])
  list(wide, long, qty)
}


# ---------- Orientation helpers ----------
trim_shape <- function(m) {
  ones <- which(m != 0, arr.ind = TRUE)
  if (nrow(ones) == 0) stop("A shape has no '1' cells.")
  m[min(ones[,1]):max(ones[,1]), min(ones[,2]):max(ones[,2]), drop = FALSE]
}
rotate90  <- function(m) t(m[nrow(m):1, , drop = FALSE])
rotate180 <- function(m) m[nrow(m):1, ncol(m):1, drop = FALSE]
rotate270 <- function(m) t(m)[, nrow(m):1, drop = FALSE]
flip_h    <- function(m) m[, ncol(m):1, drop = FALSE]
flip_v    <- function(m) m[nrow(m):1, , drop = FALSE]

generate_orientations <- function(m, rotations = TRUE, reflections = TRUE) {
  base <- list(trim_shape(m))
  mats <- if (rotations) list(base[[1]], rotate90(base[[1]]), rotate180(base[[1]]), rotate270(base[[1]])) else base
  if (reflections) {
    refls <- unlist(lapply(mats, function(x) list(flip_h(x), flip_v(x), flip_h(flip_v(x)))), recursive = FALSE)
    mats <- c(mats, refls)
  }
  # Deduplicate by (dims + flattened values, column-major)
  keys <- vapply(mats, function(x) paste(nrow(x), ncol(x), paste(as.integer(as.vector(x)), collapse = ""), sep = "|"),
                 FUN.VALUE = character(1))
  mats[!duplicated(keys)]
}

shape_coords <- function(m) {
  idx <- which(m != 0, arr.ind = TRUE)
  list(rows = as.integer(idx[,1]),
       cols = as.integer(idx[,2]),
       h = nrow(m),
       w = ncol(m),
       area = nrow(idx))
}

# ---------- STRICT boolean feasibility (DFS with pruning, no side-effects) ----------
can_satisfy_exact_fast <- function(
    shapes_list,            # list of 0/1 matrices
    grid_cols, grid_rows,   # wide × long (cols × rows)
    demand,                 # integer vector length S (non-negative)
    rotations = TRUE, reflections = TRUE,
    max_iters = 200000L,    # iteration safety cap
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  S <- length(shapes_list)
  if (length(demand) != S) stop("demand length must equal number of shapes.")
  if (any(demand < 0))     stop("demand cannot have negative entries.")
  
  # Precompute orientations + coordinates
  shape_orients <- vector("list", S)
  shape_area    <- numeric(S)
  for (i in seq_len(S)) {
    mats <- generate_orientations(shapes_list[[i]], rotations, reflections)
    cs   <- lapply(mats, shape_coords)
    shape_orients[[i]] <- cs
    shape_area[i]      <- cs[[1]]$area  # area is constant across orientations
  }
  
  # Fail-fast checks
  total_area <- sum(demand * shape_area)
  grid_area  <- grid_cols * grid_rows
  if (total_area > grid_area) return(FALSE)
  
  for (i in seq_len(S)) {
    if (demand[i] > 0) {
      fits <- any(vapply(shape_orients[[i]], function(o) o$h <= grid_rows && o$w <= grid_cols, logical(1)))
      if (!fits) return(FALSE)
    }
  }
  
  # Order: higher demand first, then larger area (reduces branching)
  order_idx <- order(-demand, -shape_area)
  
  # DFS with aggressive area pruning; no side effects
  iters <- 0L
  dfs <- function(idx_pos, rem, occ) {
    iters <<- iters + 1L
    if (iters > max_iters) return(FALSE)
    
    # Area prune
    free_cells <- sum(!occ)
    need_area  <- sum(rem * shape_area)
    if (need_area > free_cells) return(FALSE)
    
    # Success: all shapes placed
    if (sum(rem) == 0L) return(TRUE)
    if (idx_pos > length(order_idx)) return(FALSE)
    
    sid <- order_idx[idx_pos]
    if (rem[sid] == 0L) return(dfs(idx_pos + 1L, rem, occ))
    
    # Try all placements (all orientations × all origins)
    for (coords in shape_orients[[sid]]) {
      if (coords$h > grid_rows || coords$w > grid_cols) next
      rmax <- grid_rows - coords$h + 1L
      cmax <- grid_cols - coords$w + 1L
      for (r0 in seq_len(rmax)) {
        for (c0 in seq_len(cmax)) {
          rr <- coords$rows + (r0 - 1L)
          cc <- coords$cols + (c0 - 1L)
          if (any(occ[cbind(rr, cc)])) next
          occ2 <- occ; occ2[cbind(rr, cc)] <- TRUE
          rem2 <- rem; rem2[sid] <- rem2[sid] - 1L
          # Place same shape again if demand[sid] > 1; otherwise next idx happens via rem==0 above
          if (dfs(idx_pos, rem2, occ2)) return(TRUE)
        }
      }
    }
    FALSE
  }
  
  occ0 <- matrix(FALSE, nrow = grid_rows, ncol = grid_cols)
  dfs(1L, as.integer(demand), occ0)
}

# --- Example usage & sanity checks -------------------------------------------

region_len <- 1:length(input[31:length(input)])

shapes_list <- list()

for (j in 1:6) {
  out <- getShape(j)
  shapes_list <- c(shapes_list, list(out))
}


getSol <- function(x) {
  reg_info <- getRegion(region_len[x])
  wide <- reg_info[[1]]
  long <- reg_info[[2]]
  shapes <- reg_info[[3]]
  
  # shapes_list_in <- shapes_list[shapes > 0]
  # counts <- shapes[shapes>0]
  
  shapes_list_in <- shapes_list
  counts <- shapes

  sol_out <- can_satisfy_exact_fast(shapes_list, wide, long, counts)
  sol_out
}

getSol(1)

out <- sapply(1:1000, function(x) {
  x <- getSol(x)
  x
})
table(out)
