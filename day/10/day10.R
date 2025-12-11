rm(list=ls())
input <- readLines("day/10/input.txt")

onPosition <- function(x) {
  text <- input[x]
  m <- gregexpr("\\[[^\\]]*\\]", text, perl = TRUE)
  matches <- regmatches(text, m)[[1]]
  inner <- gsub("^\\[|\\]$", "", matches)
  chars <- strsplit(inner, "")[[1]]
  on <- ifelse(chars == "#", 1, 0)
  on
}

clean_to_numeric <- function(x) {
  # Remove '(' and ')'
  inner <- gsub("^\\(|\\)$", "", x)
  parts <- strsplit(inner, ",", fixed = TRUE)[[1]]
  parts <- parts[parts != ""]
  nums <- as.numeric(parts)
  
  nums
}

to_list <- function(s) {
  text <- input[s]
  m <- gregexpr("\\([^\\)]*\\)", text, perl = TRUE)
  paren_strings <- regmatches(text, m)[[1]]
  
  # x <- c("(3)", "(1,3)", "(2)", "(2,3)", "(0,2)", "(0,1)")
  
  result <- lapply(paren_strings, clean_to_numeric)
  result
  
}

xor_vec <- function(a, b) (a + b) %% 2L

# Build 0/1 masks from click index lists (assumes 0-based indices in clicks)
make_masks <- function(clicks, n) {
  lapply(clicks, function(idx) {
    m <- integer(n)
    if (length(idx) > 0) m[idx + 1L] <- 1L  # +1 to map 0-based to R's 1-based
    m
  })
}

# BFS to find minimal number of clicks and one optimal sequence
min_clicks <- function(on, start, clicks) {
  stopifnot(length(on) == length(start))
  n <- length(start)
  masks <- make_masks(clicks, n)
  
  start_key  <- paste(start, collapse = "")
  target_key <- paste(on,    collapse = "")
  
  # Queue of states and the path (sequence of click indices) used to reach them
  queue_states <- list(start)
  queue_paths  <- list(integer(0))
  visited <- new.env(parent = emptyenv())
  visited[[start_key]] <- TRUE
  
  head <- 1L
  while (head <= length(queue_states)) {
    state <- queue_states[[head]]
    path  <- queue_paths[[head]]
    
    # Found the target
    if (identical(state, on)) {
      return(list(steps = length(path), path = path))
    }
    
    # Try each click
    for (j in seq_along(masks)) {
      next_state <- xor_vec(state, masks[[j]])
      key <- paste(next_state, collapse = "")
      if (!isTRUE(visited[[key]])) {
        visited[[key]] <- TRUE
        queue_states[[length(queue_states) + 1L]] <- next_state
        queue_paths [[length(queue_paths)  + 1L]] <- c(path, j)
      }
    }
    head <- head + 1L
  }
  
  # If unreachable (shouldn't happen if clicks span the space)
  return(NULL)
}

i <- 2
on <- onPosition(i)
start <- c(rep(0, length(on)))
buttonClicks <- to_list(i)
min_clicks(on, start, buttonClicks)

clickCollect <- c()

for (j in 1:length(input)) {
  on <- onPosition(j)
  start <- c(rep(0, length(on)))
  buttonClicks <- to_list(j)
  
  out <- min_clicks(on, start, buttonClicks)[[1]]
  clickCollect <- c(clickCollect, out)
}

sum(clickCollect)
