Jared Yu
Programming Assignment 2
Algorithms for Data Science

Included project files:
# data files
iris_data_for_cleansing.csv
updated_iris.csv

The assignment was completed using RStudio.

# Main file that loads all other files
pa2_code.R

# helper functions
automated_wilks.R
discrminant_output.R
distance_measure.R
em_algorithm.R
feature_na_fill.R
gaussian_kernel.R
normalize_column.R
population_covariance.R
replace_missing_data.R
svm.R
synthesize_iris_data.R
wilks_outlier_removal.R

# output files from program
iris_cleanse.txt
iris2_final.txt
outlier_free_iris.txt
final_kernel_pca_matrix.txt
em.txt

# output images from program
wilks_plot.jpg
feature_comparison.jpg
kernel_pca.jpg

The pa2_code.R (main file with code) file should run in RStudio without any issues. The code
however does utilize certain libraries that are required for the code to run. Also, it is best
to run the code from the first line to the last line. Each problem is mostly independent, but
many of them use a variable called iris_update which is created earlier on in the program. This
is the dataset with observations and labels.

Libraries required:
gridExtra, tidyverse, tools, quadprog, e1071, nnet

They can all be obtained by using the command install.packages("Package Name"). For example,
to install the "readxl" package, type into the console install.packages("gridExtra").
