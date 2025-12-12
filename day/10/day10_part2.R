rm(list = ls())

# ---------- Dependencies ----------
if (!requireNamespace("lpSolve", quietly = TRUE)) {
  install.packages("lpSolve")
}
library(lpSolve)

# ---------- Input ----------
input <- readLines("day/10/input.txt")

# ---------- Strict parsing (no silent zeros) ----------
parse_on <- function(text) {
  m <- regmatches(text, gregexpr("\\{[^\\}]*\\}", text, perl = TRUE))[[1]]
  if (length(m) == 0L) stop("Parse error: no {…} group found in line: ", text)
  # If there are multiple {…}, use the first (matches your original function)
  v <- gsub("[{}\\s]", "", m[1L])
  parts <- strsplit(v, ",", fixed = TRUE)[[1]]
  parts <- parts[parts != ""]
  out <- suppressWarnings(as.integer(parts))
  if (anyNA(out)) stop("Parse error: non-integer token inside {…}. Line: ", text)
  out
}

parse_clicks_idxs <- function(text) {
  paren <- regmatches(text, gregexpr("\\([^\\)]*\\)", text, perl = TRUE))[[1]]
  if (length(paren) == 0L) return(list())
  res <- lapply(paren, function(s) {
    inner <- gsub("[\\(\\)\\s]", "", s)
    if (inner == "") return(integer(0))
    parts <- strsplit(inner, ",", fixed = TRUE)[[1]]
    parts <- parts[parts != ""]
    idx <- suppressWarnings(as.integer(parts))
    if (anyNA(idx)) stop("Parse error: non-integer token inside (…). Line: ", text)
    idx
  })
  res
}

# ---------- Build constraint matrix ----------
# A: n x m logical matrix where A[i,j] = 1 if button j covers position i.
build_A <- function(on_vec, clicks_idxs) {
  n <- length(on_vec)
  # Clean clicks: keep indices in 0..n-1 and drop empty masks
  clicks_clean <- lapply(clicks_idxs, function(idx0) idx0[idx0 >= 0L & idx0 < n])
  clicks_clean <- clicks_clean[lengths(clicks_clean) > 0L]
  if (!length(clicks_clean)) return(list(A = matrix(0, n, 0), clicks = list()))
  # Dedupe identical masks (saves solver effort)
  keys <- vapply(clicks_clean, function(idx) paste(sort(idx), collapse = ","), character(1))
  clicks_clean <- clicks_clean[!duplicated(keys)]
  
  m <- length(clicks_clean)
  A <- matrix(0L, nrow = n, ncol = m)
  for (j in seq_len(m)) {
    for (i0 in clicks_clean[[j]]) {
      A[i0 + 1L, j] <- 1L  # 0-based -> 1-based row index
    }
  }
  list(A = A, clicks = clicks_clean)
}

# ---------- ILP solve per line ----------
# Set allow_reuse = TRUE if a button can be clicked multiple times; FALSE for single-use.
solve_line_ilp <- function(on_vec, clicks_idxs, allow_reuse = TRUE, timeout_sec = 10L) {
  n <- length(on_vec)
  if (n == 0L) return(list(steps = 0L, status = "ok"))
  
  BA <- build_A(on_vec, clicks_idxs)
  A <- BA$A; m <- ncol(A)
  
  # Coverage check: if any on[i] > 0 and row i of A is all zeros -> infeasible
  uncovered <- which(on_vec > 0L & rowSums(A) == 0L)
  if (length(uncovered)) {
    return(list(steps = NA_integer_, status = "infeasible_uncovered"))
  }
  
  if (m == 0L) {
    # If on has any positive, cannot satisfy
    if (any(on_vec > 0L)) return(list(steps = NA_integer_, status = "infeasible_no_buttons"))
    return(list(steps = 0L, status = "ok"))
  }
  
  # Objective: minimize sum(x_j)
  obj <- rep(1, m)
  
  # Equality constraints: A %*% x == on
  const.mat <- A
  const.dir <- rep("=", n)
  const.rhs <- as.numeric(on_vec)
  
  # Variable bounds & integrality
  lower <- rep(0, m)
  upper <- rep(Inf, m)  # Inf allowed only when allow_reuse == TRUE; otherwise 1.
  all.bin <- FALSE
  if (!allow_reuse) {
    upper <- rep(1, m)
    all.bin <- TRUE
  }
  
  # Solve
  # lpSolve uses lp() for both LP and ILP; integrality via all.bin or integer.vec
  sol <- lp(
    direction = "min",
    objective.in = obj,
    const.mat = const.mat,
    const.dir = const.dir,
    const.rhs = const.rhs,
    all.bin = all.bin,
    # If reuse is allowed, we need integer variables instead of binary
    # (lpSolve defaults to continuous; enforce integrality)
    int.vec = if (allow_reuse) seq_len(m) else NULL,
    # Bounds
    compute.sens = FALSE
  )
  
  # Apply timeout (lp has control options, but many builds ignore timeout; we still check status)
  # lpSolve status codes: 0=optimal, 2=infeasible, 3=timeout/undefined, 5=presolved infeasible
  status <- sol$status
  if (status == 0) {
    clicks <- as.integer(round(sum(sol$solution)))
    return(list(steps = clicks, status = "ok"))
  } else if (status %in% c(2, 5)) {
    return(list(steps = NA_integer_, status = "infeasible"))
  } else if (status == 3) {
    return(list(steps = NA_integer_, status = "timeout"))
  } else {
    return(list(steps = NA_integer_, status = paste0("status_", status)))
  }
}

# ---------- Orchestrate all lines ----------
parsed <- lapply(input, function(txt) {
  list(on = parse_on(txt), clicks = parse_clicks_idxs(txt))
})

# If your buttons are reusable (can click same button multiple times), set TRUE.
allow_reuse_buttons <- TRUE   # <-- change to FALSE if single-use per line

timeout_per_line <- 10L       # increase if your lines are large

results <- lapply(parsed, function(x) {
  solve_line_ilp(x$on, x$clicks, allow_reuse = allow_reuse_buttons, timeout_sec = timeout_per_line)
})

click_counts <- vapply(results, function(r) r$steps, integer(1))
statuses     <- vapply(results, function(r) r$status, character(1))

status_table <- sort(table(statuses), decreasing = TRUE)
print(status_table)

total_clicks <- sum(click_counts, na.rm = TRUE)
cat("Total clicks (excluding NA lines):", total_clicks, "\n")
