# Reference: https://stackoverflow.com/questions/9317830/r-do-i-need-to-add-explicit-new-line-character-with-print
# The wilks_outlier_removal function will calculate the single outlier
# from a given data matrix using the Wilk's Outlier Removal method. It
# accepts a data matrix and a significance level alpha and looks for
# a single outlier from the data. If there are no outliers -1 is returned,
# otherwise the index of the outlier is returned.
wilks_outlier_removal <- function(data_matrix,
                                  alpha = 0.1) {
  # Check that X matrix is sufficiently large to analyze
  number_of_columns <- ncol(data_matrix)
  number_of_rows <- nrow(data_matrix)
  if (number_of_columns + 1 >= number_of_rows)
    return(-1)
  
  # Helper variables
  column_means <- as.matrix(colMeans(data_matrix))
  ones_vector <- as.matrix(rep(1, nrow(data_matrix)))
  means_matrix <- t(column_means %*% t(ones_vector))
  mean_subtracted_matrix <- as.matrix(data_matrix - means_matrix)
  variance_covariance_matrix <- population_covariance(data_matrix)
  
  # Calculate squared Mahalanobis distance for each value
  squared_mahalanobis_values <- apply(mean_subtracted_matrix, 1,
                                      function(index) {
                                        t(index) %*%
                                          solve(variance_covariance_matrix) %*%
                                          index
                                      })
  
  # Find the outlier from the data
  indexed_mahalanobis <- data.frame(squared_mahalanobis_values,
                                    1:number_of_rows)
  sorted_values <- indexed_mahalanobis[order(indexed_mahalanobis$squared_mahalanobis_values),]
  rownames(sorted_values) <- NULL
  
  critical_value <- qf(1 - alpha,  # Calculate the critical value for F-distribution
                       df1 = number_of_columns,
                       df2 = (number_of_rows - number_of_columns - 1))
  
  final_critical_value <- (number_of_columns * ((number_of_rows - 1)^2) *
                             critical_value) / (number_of_rows * (number_of_rows - number_of_columns - 1) +
                                                  (number_of_rows * number_of_columns * critical_value))
  
  outlier_data <- sorted_values[sorted_values[,1] > final_critical_value, ]
  
  # Check if outlier exists, return -1 or the index of the outlier
  if (nrow(outlier_data) > 0) {
    single_outlier <- tail(outlier_data, 1)
    writeLines(c(paste("Observation: ", single_outlier[2]),
                 paste("Squared Mahalanobis distance:", round(single_outlier[1], 4))))
    return(single_outlier[2])
  } else {
    paste("No outliers")
    return(-1)
  }
}
