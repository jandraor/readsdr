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
#'  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
#'                             measurements = 1:10))
#' estimated_params <- list(
#'   sd_prior("par_beta", "lognormal", c(0, 1)),
#'   sd_prior("par_rho", "beta", c(2, 2)),
#'   sd_prior("I0", "lognormal", c(0, 1), "init"))
#' fun <- sd_posterior_fun(filepath, meas_mdl, estimated_params)
sd_posterior_fun <- function(filepath, meas_data_mdl, estimated_params,
                             start_time = NULL, stop_time = NULL,
                             timestep = NULL, integ_method = "euler") {


  ll_fun_obj <- sd_loglik_fun(filepath      = filepath,
                              unknown_pars  = estimated_params,
                              meas_data_mdl = meas_data_mdl,
                              start_time    = start_time,
                              stop_time     = stop_time,
                              timestep      = timestep,
                              integ_method  = integ_method)

  log_lik_fun <- ll_fun_obj$fun

  log_prior_fun <- log_prior_fun_generator(estimated_pars, meas_data_mdl)

  function(pars) log_lik_fun(pars) + prior_fun(pars)
}

log_prior_fun_generator <- function(estimated_pars) {

}

