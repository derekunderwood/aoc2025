rm(list=ls())

raw_data <- read.csv("day/2/input.txt", header = F)
raw_data <- as.vector(raw_data[1,])
# x <- "1188511880-1188511890"
# parts <- strsplit(x, "-")
# idRange <- as.numeric(parts[[1]][1]):as.numeric(parts[[1]][2])

parts <- lapply(raw_data, strsplit, split = "-")

range_list <- list()
for (i in 1:length(raw_data)) {
  
  out <- as.numeric(parts[[i]][[1]][1]):as.numeric(parts[[i]][[1]][2])
  range_list <- append(range_list, list(out))
}

pattern <- "^([0-9]+)\\1$"
grepl(pattern, out)

out <- lapply(range_list, function(x){
  x[grepl(pattern, x)]
})

sum(sapply(out, sum))

# Part Two

pattern_gte_two <- "^([0-9]+)\\1{1,}$"
out2 <- lapply(range_list, function(x){
  x[grepl(pattern_gte_two, x)]
})

sum(sapply(out2, sum))
