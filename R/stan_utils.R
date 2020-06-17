
#' Extract the values over time of a variable from a Stan fit
#'
#' @param var_name A string that indicates the variable's name for which the
#' function will construct the timeseries.
#' @param posterior_df A Stan fit object converted into a data frame
#'
#' @return A data frame
#' @export
#'
#' @examples
#' posterior_df <- data.frame(`var[1]` = rep(0, 2), `var[2]` = rep(1, 2),
#'                             check.names = FALSE)
#' extract_timeseries_var("var", posterior_df)
extract_timeseries_var <- function(var_name, posterior_df) {
  posterior_cols <- colnames(posterior_df)
  pattern        <- stringr::str_glue("{var_name}\\[.+\\]")
  pos_search     <- grep(pattern, posterior_cols)
  search_cols    <- posterior_cols[pos_search]
  search_df      <- posterior_df[, search_cols]

  var_ts         <- purrr::imap_dfr(search_df, function(col, label) {
    pattern      <- "\\[(\\d+)\\]"
    match_output <- stringr::str_match(label, pattern)
    time_var     <- as.numeric(match_output[[2]])

    data.frame(stringsAsFactors = FALSE,
               iter  = seq_along(col),
               time  = time_var,
               value = col)
  })

  var_ts$variable <- var_name
  var_ts          <- var_ts[ , c(1:2,4, 3)]

  var_ts
}

# Extract stock over time

# Get stock inits

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
