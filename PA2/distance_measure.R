# The distance_measure function is used to help calculate the Fisher's Classification
# Procedure Based on Sample Discriminants, from  p.631 in Applied Multivariate Statistical
# Analysis, 6th Ed. by Johnson and Wichern.
distance_measure <- function(data_set = class_1_2_3,
                             class_mean = mean_class_1,
                             eigenvector_matrix = weight_matrix_1_2_3,
                             dimension_reduction = 3) {
  # Create an nx1 dimensional vector of ones
  one_vector <- rep(1, nrow(data_set))
  
  # Calculate the scaled difference of an observation from each of the class means
  observation_mean_deviation <- as.matrix(data_set - (one_vector %*% t(class_mean))) %*%
    eigenvector_matrix[,1:dimension_reduction]
  
  # Return the sum of squares for the scaled deviations
  return(apply(observation_mean_deviation^2, 1, sum))
}
