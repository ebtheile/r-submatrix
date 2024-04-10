library(gsignal)

max_5_5_submatrix <- function(m) {
  kernel = matrix(1, 5, 5) 
  conv = conv2(m, kernel, shape = "valid")
  max_index <- which.max(conv)
  col_index <- (max_index - 1) %/% nrow(conv) + 1
  row_index <- (max_index - 1) %% nrow(conv) + 1
  result = m[(row_index):(row_index+4), (col_index):(col_index+4)]
  return (result)
}

m <- matrix(rnorm(10000),nrow=100)
print(system.time(result <-max_5_5_submatrix(m)))


