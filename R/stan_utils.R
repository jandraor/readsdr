
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


#' Stan's data block for ODE models
#'
#' @param vars_vector a string vector. Each element corresponds to a vector's
#' name for which users will supply data.
#' @param inits a boolean. Indicates whether the block includes the declaration
#' for stocks' init values.
#' @param type a string vector. It must have the same length as vars_vector .
#'  This parameter indicates the type of the variables declared by
#'  vars_vector.
#'
#'
#' @return a string that contains the Stan code for the data block.
#' @export
#'
#' @examples
#' stan_data("y", "int")
#' stan_data("y", "real", FALSE)
stan_data <- function(vars_vector, type, inits = TRUE) {

  if(length(vars_vector) != length(type)) {
    stop("Different length sizes between 'vars_vector' & 'type' pars",
         call. = FALSE)
  }

  obj_list <- Map(c, type, vars_vector)

  data_declaration_list <- lapply(obj_list, function(var_obj) {

    stringr::str_glue("  {var_obj[[1]]} {var_obj[[2]]}[n_obs];")
  })

  data_declaration <- paste(data_declaration_list, collapse = "\n")

  stan_d <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  int<lower = 1> n_params;",
    "  int<lower = 1> n_difeq;",
    data_declaration,
    "  real t0;",
    "  real ts[n_obs];", sep = "\n")

  if(inits) {
    stan_d <- paste(stan_d, "  vector[n_difeq] y0;", sep = "\n")
  }

  paste(stan_d, "}", sep = "\n")
}
