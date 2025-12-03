rm(list=ls())
# options(scipen = 999)

# Using data.table::fread, read.table and read.csv misreads?
input <- data.table::fread("day/3/input.txt", header = F)

# Function to find the largest two-digit number
max_two_digit <- function(num_str) {
  digits <- as.numeric(unlist(strsplit(as.character(num_str), "")))
  max_num <- -Inf
  for (i in 1:(length(digits) - 1)) {
    for (j in (i + 1):length(digits)) {
      two_digit <- digits[i] * 10 + digits[j]
      if (!is.na(two_digit) && two_digit > max_num) {
        max_num <- two_digit
      }
    }
  }
  return(max_num)
}

out <- c()
for (i in 1:nrow(input)) {
  val <- max_two_digit(input[i])
  out <- c(out, val)
}

sum(out)

# Part Two

# Find the largest k-digit subsequence (as a string)
max_k_digit <- function(num_str, k = 12) {
  s <- as.character(num_str)
  chars <- strsplit(s, "")[[1]]
  # Keep only digits 0-9
  digits <- as.integer(chars[grepl("[0-9]", chars)])
  n <- length(digits)
  
  if (n < k) return(NA_character_)  # Not enough digits
  
  res <- integer(k)
  start <- 1
  
  for (p in seq_len(k)) {
    # The furthest we can look while still leaving (k - p) digits for later picks
    end <- n - (k - p)
    window <- digits[start:end]
    
    # Choose the largest digit; take the first occurrence if there are ties
    idx_rel <- which.max(window)       # index within window
    res[p] <- window[idx_rel]
    start <- start + idx_rel           # advance start past the chosen digit
  }
  
  paste0(res, collapse = "")
}

# Convenience wrapper for 12 digits
max_twelve_digit <- function(num_str) max_k_digit(num_str, k = 12)

# Vectorized usage over multiple inputs
max_twelve_vector <- function(x) {
  vapply(x, max_twelve_digit, FUN.VALUE = character(1))
}

out2 <- sapply(input, max_twelve_vector)
sum(as.numeric(out2))
