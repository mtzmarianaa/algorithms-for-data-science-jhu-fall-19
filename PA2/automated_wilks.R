# Reference: https://stackoverflow.com/questions/4357827/do-while-loop-in-r
# The automated_wilks function is used to loop through a data matrix, removing
# a single observation one at a time until the Wilk's Outlier Removal method
# no longer considers it such that an observation needs to be removed.
automatated_wilks <- function(X_matrix,
                              alpha_level = 0.1) {
  # Helper variables
  boolean_flag <- TRUE
  removed_indices <- c()
  
  # Loop through the data matrix until no more outliers remain
  while (boolean_flag) {
    outlier_index <- as.numeric(wilks_outlier_removal(data_matrix = X_matrix,
                                                      alpha = alpha_level)[1])
    if (outlier_index > 0) {
      X_matrix <- X_matrix[-outlier_index,]
    } else {
      boolean_flag <- FALSE
    }
  }
  
  # Return the outlier-free matrix
  return(X_matrix)
}
