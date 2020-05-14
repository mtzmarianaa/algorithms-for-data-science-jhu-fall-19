# The normalize_column function is a helper function that is used within
# the synthesize_iris_data() function. It is repeated for each column
# of a given class, where it will normalize the random data.
normalize_column <- function(list_element,
                             random_data,
                             iris_class,
                             minimum_list = iris_minimum_list,
                             maximum_list = iris_maximum_list) {
  # Select the current column from the rotated data
  rotated_data_column <- random_data[, list_element]
  
  # Create a Pmin and Pmax for the min and max of the rotated data
  random_min <- min(rotated_data_column)
  random_max <- max(rotated_data_column)
  
  # Create a min and max from the actual iris data
  actual_min <- iris_minimum_list[[iris_class]][list_element]
  actual_max <- iris_maximum_list[[iris_class]][list_element]
  
  # Normalize the rotated data
  rotated_data_column <- ((rotated_data_column - random_min) /
                            (random_max - random_min)) *
    (actual_max - actual_min) +
    actual_min
  
  # Return the normalized data
  return(rotated_data_column)
}