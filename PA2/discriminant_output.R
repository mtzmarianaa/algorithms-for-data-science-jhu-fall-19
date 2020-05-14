# The discriminant_output function is used will output the discriminant
# choice for each observation from a kernel matrix. The result is the
# y vector from Bishop's textbook. The observations can be later classified
# as beloning to class 1 or -1 depending on if the observation is > 0 or not.
discriminant_output <- function(alphas,
                                label_vector,
                                kernel_matrix,
                                b_solve) {
  discriminant_result <- as.matrix(kernel_matrix %*%
                            (alphas * label_vector)) + as.numeric(b_solve)
  # return(ifelse(discriminant_result > 0, 1, -1))
  return(discriminant_result)
}
