# The replace_missing_data function is used to calculate a generated
# value to fill in an NA element from a vector. The function works
# by first finding the mean and standard deviation from a vector. Then
# it will randomly a select a value between the interval of the
# mean +/- 2 * standard deviation. It will then return this value.
replace_missing_data <- function(data_column_j) {
  # Remova NA's for calculations
  na_removed_data <- data_column_j[!is.na(data_column_j)]
  
  # Calculate mean and standard deviation
  data_mean <- mean(na_removed_data)
  data_size <- length(na_removed_data)
  data_standard_deviation <- sqrt(var(na_removed_data) *
                                    ((data_size - 1) / data_size))
  
  # Randomly select a value between +/- 2*sigma from the mean
  set.seed(621)
  generated_value <- round(runif(1, data_mean - 2 * data_standard_deviation,
                                 data_mean + 2 * data_standard_deviation), 1)
  
  # Return the generated value
  return(generated_value)
}
