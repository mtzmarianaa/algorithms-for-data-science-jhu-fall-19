# Jared Yu
# Programming Assignment 1
# Algorithms for Data Science

# Load libraries
library("readxl"); library("Hmisc"); library("ggplot2"); library("gtools")

### 1. Implement an algorithm to read in the Iris dataset.

# References: http://www.sthda.com/english/wiki/reading-data-from-excel-files-xls-xlsx-into-r
# Load iris_data sheet
# NOTE: Uses readxl library
iris_dataframe <- read_xlsx(path = "C://Users//qizhe//Desktop//JHU//alg_for_ds//PA1//iris.xlsx",
                sheet = "iris_data", col_names = FALSE)

# Load class sheet
dataframe_class <- read_xlsx(path = "C://Users//qizhe//Desktop//JHU//alg_for_ds//PA1//iris.xlsx",
                      sheet = "class", col_names = FALSE)

# Load header sheet
dataframe_labels <- read_xlsx(path = "C://Users//qizhe//Desktop//JHU//alg_for_ds//PA1//iris.xlsx",
                      sheet = "header", col_names = FALSE)

# Refernces: https://stackoverflow.com/questions/21080967/how-to-get-the-first-10-words-in-a-string-in-r
# Save the feature names to a variable for the dataframe header
features <- dataframe_labels[4:7,]
feature_names <- c()  # Variable for feature/column header names
for (index in 1:nrow(features)) {
  # Loop through and save the feature names to a variable
  feature_names <- c(feature_names, paste(strsplit(x = as.character(features[index,]),
                 split = "\\s+")[[1]][1:2], collapse = "_"))
}
feature_names <- c(feature_names, 'iris_class')

# Create iris dataset
iris_dataframe <- cbind(iris_dataframe, dataframe_class)
colnames(iris_dataframe) <- feature_names

# Save the class names names to a variable for use during plotting
classes <- dataframe_labels[10:12,]
class_names <- c()
for (index in 1:nrow(classes)) {
  class_names <- c(class_names, paste(strsplit(x = as.character(classes[index,]),
                                split = "\\s+")[[1]][3:4], collapse = " "))
}

### 2. Implement an algorithm to visually see two sets of features
# and the class they belong to.

# The function plot_2_features will accept as arguments 2 features from
# the iris dataset. It will also accept as the dataset given the iris
# dataset, along with a variable for the class name. It will create a
# scatterplot of the two features along with color and symbol codes
# for their respective classes.
# NOTE: Uses ggplot2 and Hmisc library
plot_2_features <- function(feature1 = 'sepal_length', feature2 = 'sepal_width',
                   data_set = iris_dataframe, class_name = 'iris_class') {
  # Create x and y labels
  graph_label = NULL
  for (index in c(feature1, feature2)) {
  graph_label <- c(graph_label, paste(capitalize(strsplit(x = index,
                        split = "_")[[1]]), collapse = " "))
  }

  # Create vector of classes
  iris_class <- factor(data_set[,class_name])

  # Create the scatter plot of two features symbol and color coded by class
  # utilizing ggplot2
  # References: http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
  # References: https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
  # References: https://stackoverflow.com/questions/35458230/how-to-increase-the-font-size-of-ggtitle-in-ggplot2/35458749
  # References: https://stackoverflow.com/questions/14942681/change-size-of-axes-title-and-labels-in-ggplot2
  feature_plot <- ggplot(data = data_set, aes(x = data_set[,feature1], 
                              y = data_set[,feature2], 
                              color = factor(data_set$iris_class), 
                              shape = factor(data_set$iris_class)))
  feature_plot + geom_point() +
    xlab(graph_label[1]) +
    ylab(graph_label[2]) +
    labs(shape = "Iris Class", col = "Iris Class") +
    ggtitle(label = paste(graph_label[1], " vs. ", graph_label[2])) +
    theme(plot.title = element_text(size = 30, hjust = 0.5),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15))
}

# Visualize all 6 possible combinations of 2 of the 4 features
# NOTE: Uses gtools library
feature_combinations <- combinations(n = 4, r = 2,
                                     v = colnames(iris_dataframe)[1:4])

# Indicate which features are being plotted
# sink('plot_names.txt')  # Used to save the output to a file
for (index in 1:nrow(feature_combinations)) {
  print(paste("Plot ", index))
  print(feature_combinations[index,])
}
# sink()

# Plot 1
plot_2_features(feature1 = feature_combinations[1,][1],
                feature2 = feature_combinations[1,][2])
# Plot 2
plot_2_features(feature1 = feature_combinations[2,][1],
                feature2 = feature_combinations[2,][2])
# Plot 3
plot_2_features(feature1 = feature_combinations[3,][1],
                feature2 = feature_combinations[3,][2])
# Plot 4
plot_2_features(feature1 = feature_combinations[4,][1],
                feature2 = feature_combinations[4,][2])
# Plot 5
plot_2_features(feature1 = feature_combinations[5,][1],
                feature2 = feature_combinations[5,][2])
# Plot 6
plot_2_features(feature1 = feature_combinations[6,][1],
                feature2 = feature_combinations[6,][2])

### 3. Implement your sorting algorithm from Homework 1 Problem 5.

# Selection sort modified to return the sorted indices
# The selection_sort function will accept an array and then sort the
# elements of the array using the selection sort method. At the same time,
# the function will sort the indices of the array using the same method
# of selection sort. This process will allow for the function to return the
# sorted indices which represent the elements having been sorted in order.
selection_sort <- function(feature_array) {
  # Create a vector representing element indices
  index_vector <- 1:length(feature_array)

  # Loops through 1-n
  for (index_j in 1:(length(feature_array) - 1)) {
    # Creates a default minimum key
    minimum_key <- feature_array[index_j]
    # Creates a minimume key index
    minimum_key_index <- index_vector[index_j]

    # Loops through remaining array elements
    for (index_i in (index_j+1):length(feature_array)) {
      # Select minimum value from remaining array
      if (feature_array[index_i] < minimum_key) {
        # Swap minimum key and feature_array[index_i]
        temporary_variable <- minimum_key
        minimum_key <- feature_array[index_i]
        feature_array[index_i] <- temporary_variable
        
        # Swap minimum key index and index_vector[index_i]
        temporary_index <- minimum_key_index
        minimum_key_index <- index_vector[index_i]
        index_vector[index_i] <- temporary_index
      }
    }
    # Save minimum key and its index to next index of array
    feature_array[index_j] <- minimum_key
    index_vector[index_j] <- minimum_key_index
  }
  return(index_vector)  # Return sorted indices
}

# Copy over iris data
iris_data <- iris_dataframe

# Save row index to separate column
iris_data['index'] <- rownames(iris_data)

# Sort dataframe by each feature
feature_1_sort <- iris_data[selection_sort(iris_data$sepal_length),]
feature_2_sort <- iris_data[selection_sort(iris_data$sepal_width),]
feature_3_sort <- iris_data[selection_sort(iris_data$petal_length),]
feature_4_sort <- iris_data[selection_sort(iris_data$petal_width),]

# Reset row indices
rownames(feature_1_sort) <- NULL
rownames(feature_2_sort) <- NULL
rownames(feature_3_sort) <- NULL
rownames(feature_4_sort) <- NULL

# Plot the data sorted by each of the features.
# Class 1, 2, and 3 represent Setosa, Versicolor, and Virginica respectively
plot((1:nrow(feature_1_sort)), feature_1_sort$iris_class,
     xlab = "Dataframe Index", ylab = "Class",
     main = "Sorted by Sepal Length")
plot((1:nrow(feature_2_sort)), feature_2_sort$iris_class,
     xlab = "Dataframe Index", ylab = "Class",
     main = "Sorted by Sepal Width")
plot((1:nrow(feature_3_sort)), feature_3_sort$iris_class,
     xlab = "Dataframe Index", ylab = "Class",
     main = "Sorted by Petal Length")
plot((1:nrow(feature_4_sort)), feature_4_sort$iris_class,
     xlab = "Dataframe Index", ylab = "Class",
     main = "Sorted by Petal Width")