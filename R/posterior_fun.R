#' Posterior function
#'
#' @inheritParams read_xmile
#' @inheritParams sd_Bayes
#' @inheritParams sd_loglik_fun
#'
#' @return A function
#' @export
#'
#' @examples
#' filepath         <- system.file("models/", "SEIR.stmx", package = "readsdr")
#' meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
#'                             measurements = 1:10))
#' estimated_params <- list(
#'   sd_prior("par_beta", "lognormal", c(0, 1)),
#'   sd_prior("par_rho", "beta", c(2, 2)),
#'   sd_prior("I0", "lognormal", c(0, 1), "init"))
#' fun <- sd_posterior_fun(filepath, meas_data_mdl, estimated_params)
sd_posterior_fun <- function(filepath, meas_data_mdl, estimated_params,
                             start_time = NULL, stop_time = NULL,
                             timestep = NULL, integ_method = "euler",
                             const_list = NULL) {


  ll_fun_obj <- sd_loglik_fun(filepath      = filepath,
                              unknown_pars  = estimated_params,
                              meas_data_mdl = meas_data_mdl,
                              start_time    = start_time,
                              stop_time     = stop_time,
                              timestep      = timestep,
                              integ_method  = integ_method,
                              const_list    = const_list)

  log_lik_fun <- ll_fun_obj$fun

  log_prior_fun <- log_prior_fun_generator(estimated_params, meas_data_mdl)

  fun <- function(pars) log_lik_fun(pars) + log_prior_fun(pars)

  list(fun = fun, par_list = ll_fun_obj$par_list)
}

log_prior_fun_generator <- function(estimated_params, meas_data_mdl) {

  meas_pars <- extract_meas_pars(meas_data_mdl)

  if(length(meas_pars) > 0) estimated_params <- c(estimated_params, meas_pars)

  par_list <- get_par_list(estimated_params)

  idx <- lapply(seq_len(length(estimated_params)), function(i) list(index = i))

  par_specs <- Map(function(x, y, z) {
    x[['par_trans']] <- NULL
    c(x, y[2], z)
  }, estimated_params, par_list, idx)

  expr_list <- lapply(par_specs, build_prior_expr)

  body_text <- paste(expr_list, collapse = " + \n")

  body_func <- paste("{", body_text, "}", sep = "\n")

  rlang::new_function(args = rlang::exprs(pars = ),
                      body = rlang::parse_expr(body_func))
}

build_prior_expr <- function(par_obj) {

  dist_name <- par_obj$dist

  arg_names_Stan <- get_dist_args(dist_name)
  arg_list       <- par_obj[arg_names_Stan]

  arg_names_R    <- get_dist_args(dist_name, "R")
  dist_args_txt  <- paste(arg_names_R, arg_list, sep = " = ")

  fun_name  <- Stan_to_R(dist_name, "d")

  par_expr <- constrain_pars(par_obj)
  log_arg  <- "log = TRUE"

  args_txt <- paste(c(par_expr, dist_args_txt, log_arg), collapse = ", ")


  stringr::str_glue("  {fun_name}({args_txt})")

}

constrain_pars <- function(par_obj) {

  trans_vector <- par_obj$par_trans
  idx          <- par_obj$index

  par_expr <- stringr::str_glue("pars[[{idx}]]") # unconstrained par

  for(cons in trans_vector) par_expr <- stringr::str_glue("{cons}({par_expr})")

  par_expr
}
