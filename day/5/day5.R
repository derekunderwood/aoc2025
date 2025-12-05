rm(list=ls())

input <- read.table("day/5/input.txt", header = F)

ranges <- input[grepl("-", input[,1]) == T,]
ranges <- strsplit(ranges, "-")

rangeFinder <- function(x) {
  lower <- as.numeric(ranges[[x]][1])
  upper <- as.numeric(ranges[[x]][2])
  list(lower = lower, upper = upper)
}

ranges_out <- lapply(seq_along(ranges), rangeFinder)

ids <- input[grepl("-", input[,1]) == F,]
ids <- as.numeric(ids)

collect <- c()

for (h in 1:length(ranges_out)) {
  out <- c()
  for (i in 1:length(ids)){
    x <- ids[i]
    result <- ifelse(x >= ranges_out[[h]]$lower & x <= ranges_out[[h]]$upper,
                     TRUE,
                     FALSE)
    out <- c(out, result)
  }
  collect <- c(collect, ids[out == T])
}

collect <- collect[!duplicated(collect)]
length(collect)

# Part Two

z <- data.frame("lower" = c(),
                "upper" = c())

for (i in 1:length(ranges_out)) {
  x <- ranges_out[[i]]$lower
  y <- ranges_out[[i]]$upper
  z <- rbind(z, c(lower = x, upper = y))
}                
colnames(z) <- c("start", "end")

union_integer_ranges <- function(df) {
  stopifnot(all(c("start", "end") %in% names(df)))
  # clean
  df <- df[!is.na(df$start) & !is.na(df$end), , drop = FALSE]
  if (nrow(df) == 0) return(df[0, , drop = FALSE])
  if (any(df$end < df$start)) stop("Found end < start in input.")
  
  # sort by start, then end
  o <- order(df$start, df$end)
  s <- df$start[o]; e <- df$end[o]
  
  out_s <- integer(0)
  out_e <- integer(0)
  
  cur_s <- s[1]
  cur_e <- e[1]
  
  for (i in 2:length(s)) {
    # For integers, treat adjacency as mergeable: [a,b] and [c,d] merge if c <= cur_e + 1
    if (s[i] <= cur_e + 1) {
      # overlap or touch; extend
      if (e[i] > cur_e) cur_e <- e[i]
    } else {
      # flush current
      out_s <- c(out_s, cur_s)
      out_e <- c(out_e, cur_e)
      # start new
      cur_s <- s[i]; cur_e <- e[i]
    }
  }
  # flush last
  out_s <- c(out_s, cur_s)
  out_e <- c(out_e, cur_e)
  
  data.frame(start = out_s, end = out_e)
}

zz <- union_integer_ranges(z)
sum(zz$end-zz$start+1)
