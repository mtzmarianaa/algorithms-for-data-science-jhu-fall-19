# The feature_na_fill function is used to update each of the NA values
# in a vector with new values generated with the help of the
# replace_missing_data() function. It loops through all of the NA's
# in a vector and then replaces them with an imputed value. It returns
# the new vector.
feature_na_fill <- function(data_column) {
  number_of_na <- sum(is.na(data_column))  # Count the NA's
  na_indices <- which(is.na(data_column))  # Locate indices of NA's
  
  # Loop through and replace all NA's with generated data
  while(number_of_na > 0) {
    # Replace the next NA value with generated data
    data_column[na_indices[1]] <- replace_missing_data(data_column_j = data_column)
    
    # Update the helper variables
    na_indices <- which(is.na(data_column))
    number_of_na <- number_of_na - 1
  }
  
  return(data_column)
}
