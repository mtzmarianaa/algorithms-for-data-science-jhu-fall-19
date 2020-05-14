# The synthesize_iris_data function is a function to be used on each class of the
# iris data. It will generate 50 new observations for each class that are random.
# It will rotate them using a covariance matrix and normalize them with the assistance
# of a helper function, normalize_column().
synthesize_iris_data <- function(list_element_number = 1,
                                 iris_list_data = iris_list,
                                 covariance_list_data = covariance_list,
                                 minimum_list = iris_minimum_list,
                                 maximum_list = iris_maximum_list) {
  # Random matrix of 50 observations
  set.seed(100)  # Set seed so that random numbers are constant
  random_matrix <- matrix(rep(runif(n = 50*2)), ncol = 2)
  
  # Rotate the random matrix according to the covariance matrix
  rotated_data <- as.data.frame(random_matrix %*%
                                  covariance_list_data[[list_element_number]])
  
  # Normalize each column of the rotated data
  normalized_data <- sapply(seq_along(rotated_data), function(index)
    normalize_column(list_element = index, random_data = rotated_data,
                     iris_class = list_element_number))
  
  # Return the normalized data
  return(normalized_data)
}
