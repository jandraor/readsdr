#' Estimate discrete change for a stock
#'
#' @param sim_df A data frame with the simulation output
#' @param cumulative_var A string that indicates to which variable the discrete
#'   change will be estimated
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' test_output <- data.frame(time = seq(0, 2, by = 0.25),
#'                           C    = c(0, rep(5,4), rep(20, 4)))
#' discrete_net_change(test_output, "C")

discrete_net_change <- function(sim_df, cumulative_var) {
  message("This function is still under development")

  temp_df       <- sim_df[, c("time", cumulative_var)]
  temp_df       <- dplyr::filter(temp_df, time - trunc(time) == 0)
  cml_vals      <- temp_df[ , cumulative_var]
  temp_df$value <- round(cml_vals  - dplyr::lag(cml_vals ), 0)
  temp_df       <- dplyr::slice(temp_df, -1)
  temp_df$var   <- paste0("delta_", cumulative_var)

  temp_df[, c("time", "value", "var")]
}
