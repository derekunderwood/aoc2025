rm(list=ls())
options(scipen = 999)
input <- read.table("day/6/input.txt")

vertOp <- function(x) {
  num_vec <- as.numeric(input[,x][1:nrow(input)-1])
  op <- input[,x][nrow(input)]
  
  if (op == "*") {
    prod(num_vec)
  } else {
    sum(num_vec)
  }
}

x <- sapply(1:ncol(input), vertOp)
sum(x)
