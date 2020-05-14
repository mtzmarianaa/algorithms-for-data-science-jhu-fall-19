Jared Yu
Programming Assignment 1
Algorithms for Data Science

Included project files:
PA1.R
iris.xlsx

The PA1.R file should run in RStudio without any issues. The code however does utilize certain
libraries that are required for the code to run.

Libraries required:
readxl - Used to read in the excel files.
Hmisc - Used to capitalize certain strings.
ggplot2 - Used for detailed scatter plots.
gtools - Used for enumerating combinations of strings.

They can all be obtained by using the command install.packages("Package Name"). For example,
to install the "readxl" package, type into the console install.packages("readxl").

The following output can be saved to a text file using the 'sink()' function, however it has
been commented out within PA1.R. The purpose of the 'sink()' function is merely to save the
following text output so that it can be repeated in the ReadMe.txt file. However, there is no
single output file/results as indicated in the Programming Assignment Guidelines. This was
brought up during an office hours with the professor, where it was stated that this is not
an issue for a language such as R or Python where the style is that code is executed in a
line-by-line manner. The important output of the different plots have been saved as .png
files and included in the zip folder.

Program output:
[1] "Plot  1"
[1] "petal_length" "petal_width" 
[1] "Plot  2"
[1] "petal_length" "sepal_length"
[1] "Plot  3"
[1] "petal_length" "sepal_width" 
[1] "Plot  4"
[1] "petal_width"  "sepal_length"
[1] "Plot  5"
[1] "petal_width" "sepal_width"
[1] "Plot  6"
[1] "sepal_length" "sepal_width"

NOTE: There are 6 scatter plots for each combination of the two features for Problem 2 included
in the zip file. Additionally, there are 4 more plots included for Problem 3 showing the sorted
features and their corresponding classes. The names of the first 6 plots are named as follows,
'plot_#_2_features.png', where # is indexed from 1-6. The names of the enxt 4 plots are as follows,
'plot_#_sorted_features.png', where # is index from 1-4. The titles and subtitles of the plots help
to indicate which features are being plotted.

The .txt file 'sample_tests.txt' contains the various inputs and outputs for Problem 1-3.