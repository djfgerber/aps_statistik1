
matrix_to_tibble <- function(mat, col_names) {
  if (length(col_names) != ncol(mat)) {
    stop("Number of column names must match the number of columns in the matrix.")
  }
  # Convert each column of the matrix to a list of vectors
  cols <- lapply(seq_len(ncol(mat)), function(i) mat[, i])
  names(cols) <- col_names
  tibble::tibble(!!!cols)
}

rcorrelated <- function(n, mu, sds, Omega, colnames, seed = 10283){
  set.seed(seed)
  MASS::mvrnorm(n, mu, diag(sds) %*% Omega %*% diag(sds)) %>% 
    matrix_to_tibble(colnames) %>% 
    return()
}



