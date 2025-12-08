rm(list=ls())
input <- readLines("day/6/input.txt", warn = FALSE)
char_length <- regmatches(input[5], gregexpr("[*+]\\s*", input[5], perl = TRUE))[[1]]
widths <- nchar(char_length)

input <- read.fwf(
  file = "day/6/input.txt",
  widths = widths,
  strip.white = FALSE,   # critical: do not trim leading/trailing spaces
  colClasses = "character"  # keep content as text so spaces are preserved
)

revFunc <- function(x) {
  num_vec <- input[,x][1:nrow(input)-1]
  op <- trimws(input[,x][nrow(input)])
  
  val_out <- c()
  for (h in 1:max(nchar(num_vec))) {
    to_cat <- c()
    for (i in 1:length(num_vec)) {
      test <- unlist(strsplit(num_vec[i], split = ""))
      rev_vec <- ifelse(!is.na(rev(test)[h]), rev(test)[h], "") 
      to_cat <- c(to_cat, rev_vec)
    }
    
    val <- as.numeric(paste0(to_cat, collapse = ""))
    val_out <- c(val_out, val)
  }
  
  if (op == "*") {
    prod(val_out, na.rm = T)
  } else {
    sum(val_out, na.rm = T)
  }
}

out <- sapply(1:ncol(input), revFunc)
sum(out)
