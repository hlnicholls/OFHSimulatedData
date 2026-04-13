# Shared function to generate consistent PIDs across all generator scripts
# This ensures all datasets use the same participant IDs

generate_pids <- function(n) {
  set.seed(42)  # CRITICAL: Same seed ensures identical PIDs across all files
  letter1 <- sample(LETTERS, n, replace = TRUE)
  letter2 <- sample(LETTERS, n, replace = TRUE)
  numbers <- sprintf("%06d", sample(100000:999999, n, replace = FALSE))
  paste0(letter1, letter2, numbers)
}
