#' Generate a log-likelihood function for an SD model
#'
#' @param pars_df A data frame
#' @param deSolve_components A list
#' @param sim_controls A list
#' @param fit_options A list
#' @param extra_stocks An optional list
#' @param extra_constraints An optional list
#' @param neg_log A boolean that indicates whether the log-likelihood function
#'        returns a positive or negative value. If \code{TRUE}, the function
#'        returns a positive value (for minimisation optimisers). If
#'        \code{FALSE}, the function returns the original log-likelihood.
#'
#' @return A function
#' @export
#'
#' @examples
sd_loglik_fun <- function(pars_df, deSolve_components, sim_controls, fit_options,
                          extra_stocks = NULL, extra_constraints = NULL,
                          neg_log = FALSE) {

  n_unk_proc <- nrow(pars_df)

  pars_df <- arrange_pars(pars_df, fit_options)

  pars_trans_text  <- transform_pars(pars_df)
  pars_assign_text <- assign_pars_text(pars_df, extra_stocks)
  model_exe_text   <- get_model_run_text(sim_controls)

  par_measure_df <- dplyr::filter(pars_df, type == "par_measure")

  if(nrow(par_measure_df) > 1) {
    stop("Only one unknown in the measurement model is supported", call. = FALSE)
  }


  meas_model_text  <- get_meas_model_text(fit_options, neg_log, n_unk_proc)

  body_list <- list(pars_trans_text,
                    pars_assign_text,
                    model_exe_text,
                    meas_model_text)

  if(!is.null(extra_constraints)) {
    extra_constraints_text <- get_constraint_text(extra_constraints, pars_df)
    body_list              <- append(body_list, extra_constraints_text, 1)
  }

  body_text <- paste(body_list, collapse = "\n")

  body_func <- paste("{", body_text, "}", sep = "\n")

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

  pars_df <- dplyr::filter(pars_df, type != "par_measure")

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

  stocks_df <- dplyr::filter(pars_df, type == "stock")
  n_stocks  <- nrow(stocks_df)

  for(i in seq_len(n_extra)) {

    stock_obj <- extra_stocks[[i]]
    sn        <- stock_obj$name

    lhs <- stringr::str_glue('init_stocks["{sn}"]')

    rhs <- stock_obj$init

    for(j in seq_len(n_stocks)) {
      pattern     <- paste0("\\b", stocks_df[[j, "name"]], "\\b")
      idx         <- stocks_df[[j, "pos"]]
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

get_meas_model_text <- function(fit_options, neg_log, n_unk_proc) {

  stock_name <- fit_options$stock_name
  fit_type   <- fit_options$stock_fit_type

  if(fit_type == "actual") {
    sim_data_line <- "sim_data <- dplyr::filter(o_df, time - trunc(time) == 0)"
    sim_data_text <- stringr::str_glue("sim_data[, '{stock_name}']")
  }

  if(fit_type == "net_change") {
    sim_data_line <- stringr::str_glue('sim_data <- sd_net_change(o_df, "{stock_name}")')
    sim_data_text <- "sim_data[, 'value']"
  }


  if(!is.null(fit_options$dist$dist_offset)) {
    sim_data_text <- paste(sim_data_text, fit_options$dist$dist_offset,
                           sep = " + ")
  }

  distr        <- fit_options$dist$name
  par_sim_data <- fit_options$dist$sim_data

  par_assignment <- stringr::str_glue("{par_sim_data} = {sim_data_text}")

  if(!is.null(fit_options$dist$unknown)) {
    unknown_obj <- fit_options$dist$unknown
    pos         <- n_unk_proc + 1
    par_name    <- unknown_obj$name
    unknown_par <- stringr::str_glue("{par_name} = pars[[{pos}]]")
    par_assignment <- paste(par_assignment, unknown_par, sep = ", ")
  }

  if(!is.null(fit_options$dist$known_par)) {
    par_name  <- names(fit_options$dist$known_par)
    known_par <- stringr::str_glue("{par_name} = {fit_options$dist$known_par}")
    par_assignment <- paste(par_assignment, known_par, sep = ", ")
  }

  log_lik_line <- stringr::str_glue(
    "loglik   <- sum({distr}(data, {par_assignment}, log = TRUE))")

  return_line <- "loglik"

  if(neg_log) return_line <- paste0("-", return_line)

  paste(sim_data_line, log_lik_line, return_line, sep = "\n")
}

get_constraint_text <- function(extra_constraints, pars_df) {
  pars_df2  <- dplyr::mutate(pars_df,
                             pos = dplyr::row_number())

  sapply(extra_constraints, function(constraint) {

    condition <- identify_pars(constraint, pars_df2)
    stringr::str_glue("if({condition}) return(-Inf)")
  }) -> constraints_vector

  paste(constraints_vector, collapse = "\n")
}

identify_pars <- function(equation, pars_df) {

  n_rows <- nrow(pars_df)

  for(i in seq_len(n_rows)) {
    pattern     <- paste0("\\b", pars_df[[i, "name"]], "\\b")
    idx         <- pars_df[[i, "pos"]]
    replacement <- stringr::str_glue("pars[[{idx}]]")
    equation    <- stringr::str_replace_all(equation, pattern, replacement)
  }

  equation
}

arrange_pars <- function(pars_df, fit_options) {

  if(!is.null(fit_options$dist$unknown)) {
    unknown_obj <- fit_options$dist$unknown
    par_name    <- unknown_obj$name
    par_trans   <- ""

    if(!is.null(unknown_obj$par_trans)) {
      par_trans <- unknown_obj$par_trans
    }

    meas_row <- data.frame(name      = par_name,
                           type      = "par_measure",
                           par_trans = par_trans)

    pars_df <- rbind(pars_df, meas_row)

  }

  pars_df <- dplyr::mutate(pars_df,
                           type_num = dplyr::case_when(
                             type == "constant" ~ 1,
                             type == "stock" ~ 2,
                             type == "par_measure" ~ 3,
                             TRUE ~ NA_real_))

  pars_df <- dplyr::arrange(pars_df, type_num)
  pars_df <- dplyr::select(pars_df, - type_num)

  dplyr::mutate(pars_df, pos = dplyr::row_number())
}
