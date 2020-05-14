# Jared Yu
# Algorithms for Data Science
# Programming Assignment 2

# Libraries
library(gridExtra); library(tidyverse); library(tools); library(quadprog)
library(e1071); library(nnet)

### 1. Data Cleansing
# Load data
iris_cleanse <- read.csv(file = 'iris_data_for_cleansing.csv')

# Display the NA count
cat("The number of NA's in iris_data_for_cleansing.csv before cleaning is:",
    sum(is.na(iris_cleanse)))

# Fix column names
colnames(iris_cleanse)[1] <- c("sepal.length")

# Load functions
source("replace_missing_data.R")
source("feature_na_fill.R")

# Find columns with NA's
na_vector <- sapply(seq_along(iris_cleanse), function(index) {
  sum(is.na(iris_cleanse[,index]))
})
columns_with_nas <- which(na_vector > 0)

# Impute the generated features into each of the columns with NA's
for (index in seq_along(columns_with_nas)) {
  iris_cleanse[, index] <- feature_na_fill(iris_cleanse[, index])
}

# Re-count NA's in dataframe
# Display the NA count
cat("The number of NA's in iris_data_for_cleansing.csv after cleaning is:",
    sum(is.na(iris_cleanse)))

# View cleaned dataframe
print("Show the first 5 rows of the cleaned dataset.")
head(iris_cleanse)

# Save cleansed dataset
write.table(x = iris_cleanse, file = "iris_cleanse.txt")

### 2. Generate two sets of features from the original 4 features to
### end up with a total of 8 features.
# Load data
iris_update <- read.csv('updated_iris.csv')

# Fix column names
colnames(iris_update)[1] <- c("sepal.length")

# Create new variable for problem 2
iris2 <- iris_update
iris_list <- split(iris2[,c(3,4)], f = iris2$class)
iris_minimum_list <- lapply(iris_list, function(x) apply(x, 2, min))
iris_maximum_list <- lapply(iris_list, function(x) apply(x, 2, max))

# Generate means per class/feature
mean_list <- lapply(iris_list, function(index) apply(index, 2, mean))

# Generate variance-covariance matrix per class
covariance_list <- lapply(iris_list, function(index) (49/50)*cov(index))

# Load functions
source("synthesize_iris_data.R")
source("normalize_column.R")
# Generate 50 observations per class, rotated and normalized
set.seed(100)
synthetic_data_list <- lapply(seq_along(iris_list), function(index)
  synthesize_iris_data(list_element_number = index))

# Mean shift the data
synthetic_mean <- lapply(synthetic_data_list, function(index) apply(index, 2, mean))
synthetic_difference_list <- lapply(seq_along(synthetic_mean),
                                    function(index) {
  synthetic_mean[[index]] - mean_list[[index]]
})

one_vector_synthetic <- rep(1, 50)  # Use ones vector to generate mean vector

synthetic_mean_50 <- lapply(seq_along(synthetic_difference_list), function(index_i) 
  sapply(seq_along(iris_list[[1]]), function(index_j)
    as.matrix(one_vector_synthetic) %*% synthetic_difference_list[[index_i]][index_j]))

synthetic_iris_list <- lapply(seq_along(synthetic_data_list), function(index)
  synthetic_data_list[[index]] - synthetic_mean_50[[index]])

# Combine synthesized data with updated iris dataset
iris2_final <- cbind(iris2, do.call(rbind, synthetic_iris_list))
colnames(iris2_final)[8] <- c("New.Feature.3")
colnames(iris2_final)[9] <- c("New.Feature.4")

# View the new dataset with added features
print("Show the first 5 rows of the dataset with new features.")
head(iris2_final)

# Save dataset with generated columns
write.table(x = iris2_final, file = "iris2_final.txt")

### 3. Perform Feature Preprocessing: Use an outlier removal method to remove any outliers.
# Wilk's Outlier Removal
# Load functions
source("population_covariance.R")
source("wilks_outlier_removal.R")
source("automated_wilks.R")

# Use Wilk's Outlier Removal on the iris updated dataset
cat("Use Wilk's Outlier Removal with an alpha level of 0.05.")
outlier_free_iris <- automatated_wilks(X_matrix = iris_update[,1:6], alpha_level = 0.05)

# Show the number of rows retained
cat(nrow(outlier_free_iris),
    "rows retained out of",
    nrow(iris_update), "rows.")  # 136 rows retained

# Create dataframe without outliers, including indices
iris3_index <- iris_update
iris3_index$index <- 1:nrow(iris3_index)
removed_rows <- anti_join(iris3_index, outlier_free_iris, by = "New.Feature.2")

removed_indices <- removed_rows$index
full_indices <- 1:nrow(iris3_index)
full_indices <- full_indices[!(full_indices %in% removed_indices)]

outlier_free_iris$class <- iris_update[full_indices,7]
outlier_free_iris$index <- full_indices

# Preview the dataset with outliers removed
print("Show the first 5 rows of the dataset with outliers removed.")
head(outlier_free_iris)

# Save dataset with removed outliers
write.table(x = outlier_free_iris, file = "outlier_free_iris.txt")

# Do pairs scatterplot of all iris pairs, then plot on top the removed observations
outlier_scatter_plot <- function(full_data,  # Dataset with labels in last column
                                 outlier_indices  # Vector of indices for outliers
                                 ) {
  # Helper variables
  number_of_features <- ncol(full_data) - 1
  scatter_plot_combinations <- t(combn(x = 1:number_of_features, m = 2))
  feature_names <- toTitleCase(gsub(".", " ",
                    colnames(full_data[,-ncol(full_data)]), fixed = TRUE))
  
  apply(scatter_plot_combinations, 1, function(index) {
    plot(full_data[,as.numeric(index[1])], full_data[,as.numeric(index[2])],
         col = c("gray"),
         # pch = c(10),
         main = paste(feature_names[as.numeric(index[2])], " vs. ",
                      feature_names[as.numeric(index[1])]),
         xlab = feature_names[as.numeric(index[1])],
         ylab = feature_names[as.numeric(index[2])])
    
    # Highlight the removed observations 
    points(full_data[outlier_indices, as.numeric(index[1])],
           full_data[outlier_indices, as.numeric(index[2])],
           col = 'red', pch = c(10))
  })
}

# Plot observations and removed observations
par(mfrow = c(4,4))
outlier_scatter_plot(full_data = iris_update,
                     outlier_indices = removed_indices)
dev.off()  # Close plot

### 4. Rank the 6 set of features to determine which are the two top features
# References: https://stackoverflow.com/questions/12019461/rbind-error-names-do-not-match-previous-names
# View each feature's class-wise separation
gg_class_density_plots <- lapply(colnames(iris_update)[1:6], function(index)
  ggplot(iris_update,
         aes(x = iris_update[,index],
             group = class,
             color = factor(class))) +
    geom_density() +
    labs(color = "Iris Class") +
    xlab(toTitleCase(gsub(".", " ",
                          index, fixed = TRUE))) +
    ggtitle(toTitleCase(gsub(".", " ",
                          index, fixed = TRUE))))
do.call(grid.arrange, gg_class_density_plots)
dev.off()
cat("The features with best class-wise separation are:",
      "New Feature 2, Petal Length, and Petal Width.")

# Check the feature accuracy using nobis distance
# Helper variables
iris_data4 <- iris_update[,-7]  # data matrix
ones_vector <- matrix(rep(1, nrow(iris_data4)))  # vector of ones
class_list <- split(iris_update[,-7], iris_update[,7])  # list of iris classes
mean_list <- lapply(class_list,  # list of feature means by class
  function(index) apply(index, 2, mean))
mean_vector_list <- lapply(mean_list,  # vectorized version of mean_list
  function (index) ones_vector %*% index)
mean_subtracted_data_list <- lapply(mean_vector_list,  # subtract means from features
  function (index) iris_data4 - index)
class_size <- nrow(class_list[[1]])  # class size
variance_covariance_list <- lapply(class_list,  # variance covariance matrix per class
  function(index) ((class_size - 1) / class_size) * cov(index))
diagonal_inverse_variance_covariance_list <- lapply(variance_covariance_list,
  function (index) diag(solve(index)))  # inverse and diagonal only of variance covariance

# Predict class labels of each observation by feature
predicted_labels <- sapply(1:nrow(iris_data4), function (index_i) {
  # Distances per feature by class
  mahalanobis_distances <- sapply(seq_along(mean_subtracted_data_list),
    function (index_j) {
      (mean_subtracted_data_list[[index_j]][index_i,]^2) *
        diagonal_inverse_variance_covariance_list[[index_j]]
  })

  # Choose minimum distance by class
  apply(mahalanobis_distances, 1, which.min)
})

# Test accuracy of features
predicted_labels <- as.data.frame(t(predicted_labels))
true_label_vector <- iris_update$class
feature_accuracy <- apply(predicted_labels, 2,
                          function (index) mean(index == true_label_vector))
top_features <- order(feature_accuracy, decreasing = TRUE)

cat("The accuracy of the features are as follows: \n", feature_accuracy)

cat("The accuracy of the features are in the following order:",
    top_features, "\n",
    "Therefore, the top two features are Petal Length then Petal Width.")

### 5. Reduce the dimensionality to two features using PCA or Kernel PCAs
# Kernel PCA
# Helper functions and variables
source("gaussian_kernel.R")
data_X_matrix <- iris_update[,1:6]

# Calculate the kernel matrix
K_matrix <- gaussian_kernel(data_matrix = data_X_matrix)
size_N <- nrow(data_X_matrix)
one_over_N_matrix <- matrix((1 / size_N), nrow = size_N, ncol = size_N)

# Calculate Gram matrix
gram_matrix <- K_matrix -
  one_over_N_matrix %*% K_matrix -
  K_matrix %*% one_over_N_matrix +
  one_over_N_matrix %*% K_matrix %*% one_over_N_matrix

# Calculate eigenvectors of the Gram matrix
eigenvector_a_matrix <- eigen(gram_matrix)$vector

# Create the kernel PC matrix
kernel_pca_matrix <- gram_matrix %*% eigenvector_a_matrix

# Subset the top two features
final_kernel_pca_matrix <- kernel_pca_matrix[,1:2]

# Preview the dimensionally reduced matrix
head(final_kernel_pca_matrix)

# Save the first two features
write.table(x = final_kernel_pca_matrix, file = "final_kernel_pca_matrix.txt")

# Plot the biplot for the top 2 features
plot(final_kernel_pca_matrix[,1], final_kernel_pca_matrix[,2], 
     col = c("red", "blue", "green")[iris_update[,7]],
     main = "Kernel PCA Biplot of the Updated Iris Dataset",
     xlab = "First Kernel Principal Component",
     ylab = "Second Kernel Principal Component")

### 6. Using the following Machine Learning techniques, classify the three class Iris data
source("em_algorithm.R")
# Apply EM to iris update
iris_em <- iris_update[,1:6]
# Reference: https://campus.datacamp.com/courses/machine-learning-toolbox/regression-models-fitting-them-and-evaluating-their-performance?ex=6
# Use EM on the updated iris dataset
iris_output <- EM_algorithm(data_matrix = iris_em,
                            number_of_subpopulations_K = 3,
                            max_iterations = 100)

sink("em.txt")  # Save final EM values
iris_output$Iteration20$weighted_means
iris_output$Iteration20$sigma_k
iris_output$Iteration20$mixing_coefficient
sink()

# Determine the predicted labels per observation
predicted_labels <- apply(iris_output$Iteration20$conditional_probability,
                          2, function(index) c(1:3)[which.max(index)])

# Create confusion matrix
confusion_matrix_em <- table(predicted_labels, iris_update[,7])
cat("The confusion matrix for the EM algorithm is as follows: \n")
confusion_matrix_em

# Calculate accuracy
accuracy_em <- sum(diag(confusion_matrix_em)) / sum(confusion_matrix_em)
cat("The accuracy of the EM algorithm is as follows: \n", accuracy_em)

### Either Fisher Linear Discriminant (Linear Discriminant Analysis), Kernel Fisher’s Discriminant or Parzen Window
# LDA
# List version
iris_list <- split(iris_update[,1:6], f = iris_update$class)

# List of class mean vectors
mean_list <- lapply(iris_list, function(index) apply(index, 2, mean))

# List of covariance matrices for classes 1, 2, and 3
covariance_list <- lapply(iris_list, function(index) (49)*cov(index))

# Global dataframe
iris_data <- iris_update
class_1_2_3 <- iris_data[, -ncol(iris_data)]

# Global mean vector
global_mean_vector <- colMeans(class_1_2_3)

# Within-class scatter matrix
scatter_matrix_within_class <- Reduce("+", covariance_list)

# Between-class scatter matrix
class_size_n <- nrow(iris_list[[1]])
scatter_matrix_between_class <- Reduce("+",
  lapply(mean_list, function(index) (index - global_mean_vector) %*%
  t(index - global_mean_vector))) * class_size_n

# Inverse within-class scatter matrix
inverse_scatter_matrix_within_class <- solve(scatter_matrix_within_class)

# Weight matrix for all 3 classes
weight_matrix_1_2_3 <- eigen(inverse_scatter_matrix_within_class %*%
                               scatter_matrix_between_class)$vectors[,1:2]

source("distance_measure.R")
# Matrix for the scaled deviations of each class from each of the class means
distance_measurement_matrix <- sapply(mean_list,
  function(index) distance_measure(data_set = class_1_2_3,
                                   class_mean = index,
                                   eigenvector_matrix = weight_matrix_1_2_3,
                                   dimension_reduction = 2))

# Create a confusion matrix for the Fisher classification
predicted_label <- apply(distance_measurement_matrix, 1, which.min)
confusion_matrix_lda <- table(iris_update[,7], predicted_label)
cat("The confusion matrix for LDA is as follows: \n")
confusion_matrix_lda

accuracy_lda <- sum(diag(confusion_matrix_lda)) / sum(confusion_matrix_lda)
cat("The accuracy of LDA is as follows: \n", accuracy_lda)

### Neural Network Method (Probabilistic NN, Radial Basis Function or Feed Forward)
# Feed Forward
# References: https://cran.r-project.org/web/packages/nnet/nnet.pdf
iris_nn <- iris_update[,-7]
targets <- class.ind(c(rep("s", 50),  # One-hot-encode classes
                       rep("c", 50), rep("v", 50)))
set.seed(101)
train_indices <- c(sample(1:50,25),  # Randomly sample training set
                   sample(51:100,25), sample(101:150,25))
feedforward_model <- nnet(iris_nn[train_indices,], targets[train_indices,],  # Fit model
  size = 2, rang = 0.1, decay = 5e-4, maxit = 1e3, softmax = TRUE)

# Test predictions
confusion_matrix_nn <- table(max.col(targets[-train_indices,]),
                          max.col(predict(feedforward_model, iris_nn[-train_indices,])))
cat("The confusion matrix for the Feed Forward Neural Network is as follows: \n")
confusion_matrix_nn

accuracy_neural_network <- sum(diag(confusion_matrix_nn)) / sum(confusion_matrix_nn)
cat("The accuracy of the Feed Forward Neural Network is as follows: \n", accuracy_neural_network)

### Support Vector Machine
# Below the one vs. all technique in multi-class SVM is implemented on
# the updated iris dataset.
source("gaussian_kernel.R")
source("discriminant_output.R")
source("svm.r")

# Calculate the Y matrix of discriminants
discriminant_Y_matrix <- multi_class_svm(X_matrix = iris_update[,-7],
                Y_labels = iris_update[,7],
                gaussian_sigma = 1,
                alpha_constraint = 100)

# Calculate the confusion matrix
predicted_labels_svm <- max.col(discriminant_Y_matrix)
confusion_matrix_svm <- table(predicted_labels_svm, iris_update[,7])
cat("The confusion matrix for SVM is as follows: \n")
confusion_matrix_svm

accuracy_svm <- sum(diag(confusion_matrix_svm)) / sum(confusion_matrix_svm)
cat("The accuracy of SVM is as follows: \n", accuracy_svm)
