#' Generate log-likelihood function for an SD model
#'
#' @param pars_df A data frame
#' @param deSolve_components A list
#' @param sim_controls A list
#' @param fit_options A list
#' @param extra_stocks An optional list
#'
#' @return A function
#' @export
#'
#' @examples
sd_loglik_fun <- function(pars_df, deSolve_components, sim_controls, fit_options,
                          extra_stocks = NULL) {

  pars_trans_text  <- transform_pars(pars_df)
  pars_assign_text <- assign_pars_text(pars_df, extra_stocks)
  model_exe_text   <- get_model_run_text(sim_controls)
  meas_model_text  <- get_meas_model_text(fit_options)

  body_func <- paste("{",
                     pars_trans_text,
                     pars_assign_text,
                     model_exe_text,
                     meas_model_text,
                     "}", sep = "\n")

  model_func <- rlang::new_function(
    args = rlang::exprs(pars = ),
    body = rlang::parse_expr(body_func)
  )

  data       <- fit_options$data

  rlang::fn_env(model_func)$consts      <- deSolve_components$consts
  rlang::fn_env(model_func)$init_stocks <- deSolve_components$stocks
  rlang::fn_env(model_func)$data        <- fit_options$data

  model_func
}


transform_pars <- function(pars_df) {
  pars_lines <- vector(mode = "character", length = nrow(pars_df))

  if(!"par_trans" %in% colnames(pars_df)) {
    pars_df[["par_trans"]] <- ""
  }

  par_names <- pars_df$name
  par_trans <- pars_df$par_trans

  for(i in seq_along(par_names)) {
    lhs             <- stringr::str_glue("pars[[{i}]]")
    rhs             <- stringr::str_glue("pars[[{i}]]")

    if(par_trans[i] == "log") {
      rhs <- stringr::str_glue("exp(pars[[{i}]])")
    }

    if(par_trans[i] == "logit") {
      rhs <- stringr::str_glue("expit(pars[[{i}]])")
    }

    par_line        <- paste(lhs, rhs, sep = " <- ")
    pars_lines[[i]] <- par_line
  }

  paste(pars_lines, collapse = "\n")
}

assign_pars_text <- function(pars_df, extra_stocks = NULL) {

  n_rows  <- nrow(pars_df)
  n_extra <- 0

  if(!is.null(extra_stocks)) {
    n_extra <- length(extra_stocks)
  }

  assign_lines <- vector(mode = "character", length = n_rows + n_extra)

  for(i in seq_len(n_rows)) {

    par_name <- pars_df[i, "name"]

    if(pars_df[i, "type"] == "constant") {
      lhs <- stringr::str_glue('consts["{par_name}"]')
    }

    if(pars_df[i, "type"] == "stock") {
      lhs <- stringr::str_glue('init_stocks["{par_name}"]')
    }

    rhs <- stringr::str_glue('pars[[{i}]]')

    assign_line       <- paste(lhs, rhs, sep = " <- ")
    assign_lines[[i]] <- assign_line
  }

  pars_df2  <- dplyr::mutate(pars_df, pos = dplyr::row_number())
  stocks_df <- dplyr::filter(pars_df2, type == "stock")
  n_stocks  <- nrow(stocks_df)

  for(i in seq_len(n_extra)) {
    stock_obj <- extra_stocks[[i]]
    sn        <- stock_obj$name

    lhs <- stringr::str_glue('init_stocks["{sn}"]')

    rhs <- stock_obj$init

    for(i in seq_len(n_stocks)) {
      pattern     <- paste0("\\b", stocks_df[[i, "name"]], "\\b")
      idx         <- stocks_df[[i, "pos"]]
      replacement <- stringr::str_glue("pars[[{idx}]]")
      rhs         <- stringr::str_replace_all(rhs, pattern, replacement)
    }

    assign_line                <- paste(lhs, rhs, sep = " <- ")
    assign_lines[[i + n_rows]] <- assign_line
  }

  paste(assign_lines, collapse = "\n")
}

get_model_run_text <- function(sim_controls) {
  start_time <- sim_controls$start
  stop_time  <- sim_controls$stop
  step       <- sim_controls$step
  im         <- sim_controls$integ_method

  simtime_line <- stringr::str_glue(
    "simtime <- seq({start_time}, {stop_time}, {step})")

  call_ode_line <- paste("o <- deSolve::ode(",
                         "  y      = init_stocks,",
                         "  times  = simtime,",
                         "  func   = deSolve_components$func,",
                         "  parms  = consts,",
                         stringr::str_glue('  method = "{im}")'),
                         sep = "\n")

  paste(simtime_line,
        call_ode_line,
        "o_df <- data.frame(o)",
        sep = "\n")
}

get_meas_model_text <- function(fit_options) {

  stock_name <- fit_options$stock_name
  fit_type   <- fit_options$stock_fit_type
  distr      <- fit_options$dist

  if(fit_type == "actual") {
    sim_data_line <- "sim_data <- dplyr::filter(o_df, time - trunc(time) == 0)"
    sim_data_text <- stringr::str_glue("sim_data[, '{stock_name}']")
  }

  if(fit_type == "net_change") {
    sim_data_line <- stringr::str_glue('sim_data <- sd_net_change(o_df, "{stock_name}")')
    sim_data_text <- "sim_data[, 'value']"
  }


  if(!is.null(fit_options$dist_offset)) {
    sim_data_text <- paste(sim_data_text, fit_options$dist_offset,
                           sep = " + ")
  }

  log_lik_line <- stringr::str_glue(
    "sum({distr}(data, {sim_data_text}, log = TRUE))")

  paste(sim_data_line, log_lik_line, sep = "\n")
}
