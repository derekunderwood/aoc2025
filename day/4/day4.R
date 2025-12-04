rm(list=ls())
data <- read.table("day/4/input.txt", header = F)

temp_list <- list()
for (i in 1:nrow(data)) {
  temp <- strsplit(data[i,], split = "")
  temp_list <- append(temp_list, temp)
}

tp_matrix <- do.call("rbind", temp_list)
tp_matrix <- ifelse(tp_matrix == "@", 1L, 0L)

apply_count_neighbors <- function(mat) {
  out <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      rows <- max(1, i-1):min(nrow(mat), i+1)
      cols <- max(1, j-1):min(ncol(mat), j+1)
      out[i, j] <- if (mat[i, j] == 1) sum(mat[rows, cols]) - mat[i, j] else NA
    }
  }
  out
}

adj_matrix <- apply_count_neighbors(tp_matrix)
adj_matrix_out <- ifelse(adj_matrix < 4 , 1L, 0L)
adj_matrix_out <- ifelse(is.na(adj_matrix_out), 0, adj_matrix_out)

sum(adj_matrix_out == 1)

# Part Two

reduceFunc <- function(m){
  z_tp_matrix <- m
  
  zz_adj_matrix <- apply_count_neighbors(z_tp_matrix)
  zz_adj_matrix_out <- ifelse(zz_adj_matrix < 4 , 1L, 0L)
  zz_adj_matrix_out <- ifelse(is.na(zz_adj_matrix_out), 0, zz_adj_matrix_out)
  
  sum_out <- sum(zz_adj_matrix_out == 1)
  
  mask <- zz_adj_matrix_out == 1 # flag/mask for removal (0) from new matrix
  z_tp_matrix[mask] <- 0
  
  list(sum_out, z_tp_matrix)
  
}

iterate_n_steps <- function(m) {
  current <- m
  success <- FALSE
  steps <- c()
  while (!success) {
    res <- reduceFunc(current)
    print(res[[1]])
    steps <- c(steps, res[[1]])
    current <- res[[2]]
    success <- res[[1]] == 0
  }
  
  list(steps = steps, final_matrix = current)
  
}

z <- iterate_n_steps(tp_matrix)
sum(z[[1]])
