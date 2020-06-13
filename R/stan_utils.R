
# Extract variables over time

# Extract stock over time

# Get stock inits

# get transformed data block


#' Stan's transformed data block for ODE models
#'
#' @return a string
#' @export
#'
#' @examples
#' td <- stan_transformed_data()
stan_transformed_data <- function() {
  stan_td <- paste(
    "transformed data {",
    "  real x_r[0];",
    "  int  x_i[0];",
    "}", sep = "\n")
}
