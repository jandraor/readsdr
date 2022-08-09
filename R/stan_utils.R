
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

# Stan's data block for ODE models
stan_data <- function(meas_mdl, unk_inits) {

  decl <- paste(
    "  int<lower = 1> n_obs;",
    "  int<lower = 1> n_params;",
    "  int<lower = 1> n_difeq;", sep = "\n")

  data_decl <- lapply(meas_mdl, construct_data_decl) %>%
    paste(collapse = "\n")

  final_decl <- paste("  real t0;",
                      "  array[n_obs] real ts;", sep = "\n")

  body_block <- paste(decl, data_decl, final_decl, sep = "\n")

  if(!unk_inits) {
    body_block <- paste(body_block,
                        "  vector[n_difeq] x0;", sep = "\n")
  }

  paste("data {", body_block, "}", sep = "\n")
}

construct_data_decl <- function(meas_obj) {

  split_strings <- strsplit(meas_obj, "~")[[1]]

  lhs <- split_strings[[1]] %>% stringr::str_trim()

  rhs  <- split_strings[[2]] %>% stringr::str_trim()
  type <- get_dist_type(rhs)

  stringr::str_glue("  array[n_obs] {type} {lhs};")
}

get_dist_type <- function(rhs) {

  pattern       <- "(.+?)\\(.+\\)"
  string_match  <- stringr::str_match(rhs, pattern)
  dist_name     <- string_match[[2]]

  dist_db <- data.frame(id = c("neg_binomial",
                               "neg_binomial_2",
                               "poisson",
                               "normal",
                               "lognormal"),
                        type = c("int",
                                 "int",
                                 "int",
                                 "real",
                                 "real"))

  index <- match(dist_name, dist_db$id)

  dist_db[index, "type"]
}

get_dist_obj <- function(rhs) {

  pattern       <- "(.+?)(\\(.+\\))"
  string_match  <- stringr::str_match(rhs, pattern)
  dist_name     <- string_match[[2]]

  args_text <- string_match[[3]] %>%
    stringr::str_remove("^\\(") %>%
    stringr::str_remove("\\)$")

  args_list <- strsplit(args_text, ",")[[1]] %>% stringr::str_trim() |>
    as.list()

  args_names <- get_dist_args(dist_name)

  names(args_list) <- args_names

  c(list(dist_name = dist_name), args_list)

}

get_dist_args <- function(dist) {

  if(dist == "beta") return (c("alpha", "beta"))
  if(dist == "exponential") return (c("beta"))
  if(dist == "lognormal") return (c("mu", "sigma"))
  if(dist == "neg_binomial_2") return (c("mu", "phi"))

  msg <- stringr::str_glue("Distribution '{dist}' not supported")
  stop(msg, call. = FALSE)
}

decompose_meas <- function(meas_obj) {

  split_strings <- strsplit(meas_obj, "~")[[1]]
  lhs           <- split_strings[[1]] %>% stringr::str_trim()
  rhs           <- split_strings[[2]] %>% stringr::str_trim()

  list(lhs = lhs,
       rhs = rhs)
}

