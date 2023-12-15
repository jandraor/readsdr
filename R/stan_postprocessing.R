
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


#' Extract the values over time of a stock from a Stan fit
#'
#' @param stock_name A string that indicates the stock's name for which the
#' function will construct the timeseries.
#' @param all_stocks A vector of strings that contains the names of all the
#' stocks in the model. This vector must have the same order as the differential
#' equations in the Stan code.
#' @param ODE_output A string that indicates the name of the variable where
#' model's output in stored in Stan.
#'
#' @inheritParams extract_timeseries_var
#'
#' @return A data frame
#' @export
#'
#' @examples
#' posterior_df <- data.frame(`yhat[1,2]` = rep(0, 2), `yhat[2,2]` = rep(1, 2),
#'                             check.names = FALSE)
#' stocks       <- c("S1", "S2")
#' extract_timeseries_stock("S2", posterior_df, stocks, "yhat")
extract_timeseries_stock <- function(stock_name, posterior_df, all_stocks,
                                     ODE_output) {

  posterior_cols <- colnames(posterior_df)
  pos_stock      <- which(stock_name == all_stocks)
  pattern        <- stringr::str_glue("{ODE_output}\\[\\d+,{pos_stock}\\]")
  pos_search     <- grep(pattern, posterior_cols)
  search_cols    <- posterior_cols[pos_search]
  search_df      <- posterior_df[, search_cols]

  stock_ts       <- purrr::imap_dfr(search_df, function(col, label) {

    pattern      <- stringr::str_glue("{ODE_output}\\[(\\d+),\\d+\\]")
    match_output <- stringr::str_match(label, pattern)
    time_var     <- as.numeric(match_output[[2]])

    data.frame(stringsAsFactors = FALSE,
               iter  = seq_along(col),
               time = time_var,
               value = col)
  })

  stock_ts$stock <- stock_name
  stock_ts       <- stock_ts[, c(1:2, 4, 3)]
  stock_ts
}
