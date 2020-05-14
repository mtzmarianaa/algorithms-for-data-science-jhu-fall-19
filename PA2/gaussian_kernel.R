# The gaussian_kernel function creates the Gaussian kernel
# for a given data matrix. It calculates the kernel function
# with an input for a sigma which is set to 1 as default.
gaussian_kernel <- function(data_matrix,
                            sigma_squared = 1) {
  # x_i'x_i - 2x_i'x_j + x_j'x_j
  data_matrix <- as.matrix(data_matrix)
  one_vector <- matrix(1, nrow = nrow(data_matrix), ncol = 1)
  row_matrix <- as.matrix(apply(data_matrix, 1,  # x_i'x_i
                          function(index) sum(index^2))) %*% t(one_vector)
  norm_matrix <- row_matrix - 2 *
    (data_matrix  %*% t(data_matrix)) + t(row_matrix)
  
  return(exp((-norm_matrix) / (2 * sigma_squared)))
}
