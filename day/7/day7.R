rm(list=ls())
input <- read.table("day/7/input.txt")

temp_list <- list()
for (i in 1:nrow(input)) {
  temp <- strsplit(input[i,], split = "")
  temp_list <- append(temp_list, temp)
}

laser_matrix <- do.call("rbind", temp_list)

recode_row_by_above <- function(M, i, map) {
  stopifnot(is.matrix(M), i >= 2, i <= nrow(M))
  above <- M[i - 1, ]

  keys <- as.character(above)
  # Lookup: map[keys] returns a vector with NAs if key absent
  new_vals <- unname(map[keys])
  new_vals[is.na(new_vals)] <- M[i, is.na(new_vals)]
  M[i, ] <- new_vals
  M
}

map <- c(S = "|", `|` = "|")

copy_from_above_except <- function(M, i, block_char = "^") {
  # Build mask: copy only where current row != block_char AND above row != block_char
  mask <- (M[i, ] != block_char) & (M[i - 1, ] != block_char)
  # Copy from row above where allowed
  M[i, mask] <- M[i - 1, mask]
  M[i,]
}

z <- recode_row_by_above(laser_matrix, 2, map)

for (i in 3:nrow(z)) {
  row <- copy_from_above_except(z, i)
  caret_positions <- which(row == "^")
  for (pos in caret_positions) {
    if (pos > 1) row[pos - 1] <- "|"
    if (pos < length(row)) row[pos + 1] <- "|"
  }
  z[i,] <- row
}

new_streams <- function(M, i) {
  mask <- (M[i, ] == "^") & (M[i - 1, ] == "|")
  sum(mask)
}

apply_new_streams <- function(M) {
  sapply(3:nrow(M), function(i) new_streams(M, i))
}

sum(apply_new_streams(z))
