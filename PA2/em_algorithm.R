### Expectation Maximization
# The g_function function is used to calculate the kth output of the
# g function in the EM-algorithm (based on lecture slides).
# The function determines the multivariate normal distribution
# for the data matrix when given a set of means and standard deviations.
g_function <- function(X_matrix,
                       column_means_k,
                       sigma_k,
                       number_of_columns) {
  # Calculate the g() for a certain value of k
  return((1 / ((sqrt(2 * pi) * sigma_k))^number_of_columns) *
           exp((-0.5)*(colSums((t(X_matrix - column_means_k))^2) / sigma_k^2)))
}

# The conditional_probablity_kn function is used to calculate the conditional
# probability for some k, where k = 1,...,K, given the dataset. This function
# is looking for the probabilities of the latent variables that are determined
# a priori by the user. The latent variable in this case is K, the number of
# subpopulations in the dataset. The results are known as the responsibilities.
conditional_probability_kn <- function(mixing_probability,
                                       g_output,
                                       total_K) {
  # Calculate the numerator
  numerator <- t(sapply(seq_along(1:total_K), function(index_k) {
    mixing_probability[index_k] * g_output[index_k, ]
  }))
  
  # Calculate the denominator
  denominator <- matrix(rep(colSums(numerator), each = total_K), nrow = total_K)
  
  # Return the responsibility that component k takes for explaining
  # the observations of the dataset.
  return(numerator / denominator)
}

# The mixing_probabilities function determines the mixing coefficients for
# each of the subpopulations. The rule is that the mixing coefficients
# should sum to one. The mixing coefficients help to better understand
# the superposition of the K Gaussian desnsities. 
mixing_probabilities <- function(number_observations_N,
                                 responsibilities) {
  # Return the mixing coefficients
  return((1 / number_observations_N) * rowSums(responsibilities))
}

# The means_function function calculates the column means of the dataset
# for each possible value of k, k = 1,...,K. It does so using the responsibilities
# that are found using the conditional_probability_kn function.
means_function <- function(X_matrix,
                           responsibilities,
                           total_K) {
  # Return the matrix of means
  return(sapply(seq_along(1:total_K), function(index_k) {
    t(responsibilities[index_k,] %*% as.matrix(X_matrix)) /
      sum(responsibilities[index_k,])
  }))
}

# The standard_deviation_function function is used to calculate the sigma_k
# function for each iteration of the EM algorithm. It calculates the standard
# deviation for each column of the k subpopulations.
standard_deviations_function <- function(X_matrix,
                                         responsibilities,
                                         weights,
                                         total_K,
                                         total_N,
                                         number_of_dimensions) {
  return(sapply(1:total_K, function(index_k) {
    sqrt(sum(responsibilities[index_k,] %*% ((X_matrix - matrix(rep(weights[, index_k],
           each = total_N), ncol = number_of_dimensions))^2)) /
           (number_of_dimensions * sum(responsibilities[index_k,])))
  }))
}

EM_algorithm <- function(data_matrix,
                         number_of_subpopulations_K,
                         max_iterations = 100,
                         convergence_cutoff = 0.001,
                         sample_data = FALSE) {
  # Helper variables
  data_matrix <- as.matrix(data_matrix)
  dimensions_D <- ncol(data_matrix)  # Number of dimensions in the dataset
  number_observations <- nrow(data_matrix)  # Number of observations in the dataset
  one_vector <- matrix(rep(1, dimensions_D), ncol = 1)  # Vector of ones
  convergence <- FALSE  # Boolean variable for convergence
  iteration <- 0  # Counter for number of iterations
  set.seed(685)  # Set seed and randomizing vector for means
  new_list <- list("conditional_probability" = NULL,  # A list to save output
                   "weighted_means" = NULL, "sigma_k" = NULL,
                   "mixing_coefficient" = NULL)
  
  # Adjust for sample code
  if (sample_data == TRUE) {
    random_vector <- matrix(c(-0.18671, 0.72579), ncol = 2)
  } else {
    random_vector <- matrix(rnorm(dimensions_D), ncol = dimensions_D)
  }
  
  # Initialize parameters
  sample_column_mean_vector <- colMeans(data_matrix)  # xbar_col
  sample_std_dev_vector <- sqrt(diag(var(data_matrix)))  # sigma_col
  weighted_means_iteration_i <- round(sample_column_mean_vector %*%
                                        t(one_vector) + sample_std_dev_vector %*%  random_vector, 4)  # m_k
  sigma_k_iteration_iteration_i <- round(mean(sample_std_dev_vector) %*%
                                           t(one_vector), 4)  # sigma_k
  mixing_coefficient_iteration_i <- t(rep(1, number_of_subpopulations_K)) /
    number_of_subpopulations_K  # p_k
  
  output <- list()  # Helper list to save output
  # Go through iterations until convergence or limit
  while ((convergence == FALSE) & (iteration < max_iterations)) {
    old_list <- new_list  # Update old_list
    # E-step
    # Calculate the (i + 1)th g function
    g_function_iteration_i <- t(sapply(seq_along(1:number_of_subpopulations_K),
                                       function(index_k) {
                                         g_function(X_matrix = data_matrix,
                                                    column_means_k = matrix(rep(weighted_means_iteration_i[, index_k],
                                                                                each = number_observations), ncol = dimensions_D),
                                                    sigma_k = sigma_k_iteration_iteration_i[index_k],
                                                    number_of_columns = dimensions_D)
                                       }))
    
    # Calculate the responsibilities of each component k
    conditional_probability_iteration_i <- conditional_probability_kn(
      mixing_probability = mixing_coefficient_iteration_i,
      g_output = g_function_iteration_i,
      total_K = number_of_subpopulations_K)
    new_list$conditional_probability <- round(conditional_probability_iteration_i, 4)
    
    # M-step
    # Calculate the (i + 1)th value for the means function
    weighted_means_iteration_i <- means_function(
      X_matrix = data_matrix,
      responsibilities = conditional_probability_iteration_i,
      total_K = number_of_subpopulations_K)
    new_list$weighted_means <- round(weighted_means_iteration_i, 4)
    
    # Calculate the column standard deviation of each k for the (i + 1)th iteration
    sigma_k_iteration_iteration_i <- standard_deviations_function(
      X_matrix = data_matrix,
      responsibilities = conditional_probability_iteration_i,
      weights = weighted_means_iteration_i,
      total_K = number_of_subpopulations_K,
      total_N = number_observations,
      number_of_dimensions = dimensions_D)
    new_list$sigma_k <- round(sigma_k_iteration_iteration_i, 4)
    
    # Calculate the mixing coefficient for each k group for the (i + 1th) iteration
    mixing_coefficient_iteration_i <- mixing_probabilities(
      number_observations_N = number_observations,
      responsibilities = conditional_probability_iteration_i)
    new_list$mixing_coefficient <- round(mixing_coefficient_iteration_i, 4)
    
    # Update counter for number of iterations
    iteration <- iteration + 1
    
    # Update list with new list of elements
    output <- c(output, list(new_list))
    
    # Check for convergence
    if (iteration > 1) {
      list_difference <- Map("-", old_list, new_list)
      convergence_score <- sum(sapply(list_difference,
                                      function(list_index) sum(abs(list_index))))
      if (convergence_score < convergence_cutoff)
        convergence <- TRUE
      print(c(iteration, convergence_score))
    }
  }
  
  # Name each iteration
  names(output) <- paste("Iteration", 1:length(output), sep = '')
  
  # Return output list
  return(output)
}
