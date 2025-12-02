rm(list=ls())
input <- read.table("day/1/input.txt")

startPos <- 50

out <- list()
for (i in 1:nrow(input)) {
  move <- strsplit(input[i,], split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
  out <- append(out, move)
}

moves <- c()
for (i in 1:length(out)) {
  sign <- ifelse(out[[i]][1] == "R", 1, -1)
  clicks <- as.numeric(out[[i]][2]) %% 100
  # clicks <- ifelse(sign > 0,
                   # clicks %% 100,
                   # (clicks %% 100) - 1)
  
  dialMove <- sign*clicks
  moves <- append(moves, dialMove)
}

startMoves <- append(startPos, moves)


rolling_lock_positions <- function(net_clicks, start_pos = 50) {
  # Compute cumulative positions
  positions <- numeric(length(net_clicks))
  current_pos <- start_pos
  
  for (i in seq_along(net_clicks)) {
    current_pos <- (current_pos + net_clicks[i]) %% 100
    current_pos <- (current_pos + 100) %% 100  # handle negatives
    positions[i] <- current_pos
  }
  
  return(positions)
}

out_count <- rolling_lock_positions(moves, start_pos = 50)
table(out_count)

# Part Two
moves2 <- c()
for (i in 1:length(out)) {
  sign <- ifelse(out[[i]][1] == "R", 1, -1)
  clicks <- as.numeric(out[[i]][2])
  
  dialMove <- sign*clicks
  moves2 <- append(moves2, dialMove)
}


count_crossings <- function(clicks, start_pos = 50) {
  current_pos <- start_pos
  cross_count <- 0
  
  for (click in clicks) {
    prev_total <- current_pos
    new_total <- current_pos + click
    
    if (click > 0) {
      # Moving forward: count how many multiples of 100 we pass
      crosses <- floor(new_total / 100) - floor(prev_total / 100)
    } else {
      # Moving backward: count how many multiples of 100 we pass
      crosses <- ceiling(new_total / 100) - ceiling(prev_total / 100)
      crosses <- abs(crosses)
    }
    
    cross_count <- cross_count + crosses
    
    # Update current position modulo 100
    current_pos <- (new_total %% 100 + 100) %% 100
  }
  
  return(cross_count)
}

count_crossings(moves2, start_pos = 50)
