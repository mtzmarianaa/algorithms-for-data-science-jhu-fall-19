# This R script contains the two_class_svm() and multi_class_svm() functions.
# It requires the use of gaussian_kernel.R and discriminant_output.R.

# The two_class_svm function is a helper function to be used with
# the multi_class_svm() function. It is used to help calculate the
# one vs. all class comparison that SVM can utilize in multi-class
# classification. The function will go through the process of
# calculating the Gram matrix and solving the optimization problem
# for the quadratic equation W(alpha). It returns the output of
# the discriminant function afterwards.
two_class_svm <- function(two_data_matrix,  # data matrix of 'two' classes
                          two_Y_labels,  # binary labels for 'two' classes
                          gaussian_sigma_initial = 1,  # Gaussian kernel parameter
                          C_constraint = 100  # constraint for alpha vector
) {
  # Calculate the Gram matrix
  gram_matrix <- gaussian_kernel(data_matrix = two_data_matrix,
                                 sigma_squared = gaussian_sigma_initial)
  
  # Helper variables
  y_label_vector <- two_Y_labels
  sample_size <- nrow(two_data_matrix)
  diagonal_label_matrix <- diag(y_label_vector)
  
  # Set up the variables for the quadratic equation solver
  D_matrix <- diagonal_label_matrix %*% gram_matrix %*% diagonal_label_matrix
  d_vector <- as.matrix(rep(1, sample_size))
  A_matrix <- t(rbind(y_label_vector,
                      diag(x = 1, nrow = sample_size),
                      diag(x = -1, nrow = sample_size)))
  constraint_vector <- c(0, rep(0, sample_size), rep(-C_constraint, sample_size))
  
  # Optimize the quadratic equation
  alpha_solve <- solve.QP(Dmat = D_matrix,
                          dvec = d_vector,
                          Amat = A_matrix,
                          bvec = constraint_vector)
  alpha_vectors <- alpha_solve$solution
  indicator_vector <- as.numeric(alpha_vectors > 0)
  
  # Solve for the bias
  b_result <- (t(indicator_vector) %*% y_label_vector -
                 sum(alpha_vectors * indicator_vector * y_label_vector *
                       apply(gram_matrix, 1, sum))) /
    sum(indicator_vector)
  
  # Calculate the output for the discriminant function
  discriminant_result <- discriminant_output(alphas = alpha_vectors,
                                             label_vector = y_label_vector,
                                             kernel_matrix = gram_matrix,
                                             b_solve = b_result)
  
  # Return the discriminant output
  return(discriminant_result)
}

# The multi_class_svm function is used to calculate the discriminant
# matrix for a multi-class dataset. It performs the one vs. all
# classification method for multi-class SVM. It uses the helper function
# two_class_svm() to do one vs. all for each of the classes. The function
# will return the discriminant matrix for the multi-class case.
multi_class_svm <- function(X_matrix,  # data matrix with no labels
                            Y_labels,  # labels for the data matrix X_matrix
                            gaussian_sigma = 1,  # parameter for Gaussian kernel
                            alpha_constraint = 100  # constraint for alpha vectors
) {
  # Helper variables
  number_labels <- length(unique(Y_labels))
  discriminant_matrix <- matrix(0,
                                nrow = nrow(X_matrix),
                                ncol = number_labels)
  
  # Calculate the corresponding one vs. all classification for each label
  for(index in 1:number_labels) {
    discriminant_matrix[,index] <- two_class_svm(
      two_data_matrix = X_matrix,
      two_Y_labels = ifelse(Y_labels == index, 1, (-1 / (number_labels - 1))),
      gaussian_sigma_initial = gaussian_sigma,
      C_constraint = alpha_constraint)
  }
  
  # Return the multi-class SVM discriminant matrix
  return(discriminant_matrix)
}
