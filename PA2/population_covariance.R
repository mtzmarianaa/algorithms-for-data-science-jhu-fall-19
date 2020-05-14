# Reference: https://stackoverflow.com/questions/30695648/how-to-redefine-cov-to-calculate-population-covariance-matrix
# The population_covariance function is designed to help calculate the population
# variance-covariance matrix for a given X matrix. It returns the pxp variance
# -covariance matrix using the population degree of freedom.
population_covariance <- function(covariance_data) {
  cov(covariance_data) *
    (nrow(covariance_data) - 1) /
    nrow(covariance_data)
}
