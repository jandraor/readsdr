#' Prior predictive checks
#'
#' @param n_draws An integer that indicates how many time-series will be
#'   returned.
#' @inheritParams read_xmile
#' @inheritParams sd_Bayes
#' @inheritParams sd_simulate
#'
#' @return A list of two data frames.
#' @export
#'
#' @examples
#'   filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")
#'   meas_mdl   <- list("y ~ neg_binomial_2(net_flow(C), phi)")
#'   estimated_params <- list(
#'     sd_prior("par_beta", "lognormal", c(0, 1)),
#'     sd_prior("par_rho", "beta", c(2, 2)),
#'     sd_prior("I0", "lognormal", c(0, 1), "init"))
#'   sd_prior_checks(filepath, meas_mdl, estimated_params, n_draws = 2,
#'    start_time = 0, stop_time = 5,
#'    integ_method = "rk4", timestep = 1/32)
sd_prior_checks <- function(filepath, meas_mdl, estimated_params, n_draws,
                            start_time   = NULL,
                            stop_time    = NULL,
                            timestep     = NULL,
                            integ_method = "euler") {

  pars_names    <- get_names(estimated_params, "par_name")

  estimated_params <- get_meas_params(meas_mdl, estimated_params)
  unk_types        <- sapply(estimated_params,
                             function(prior_obj) prior_obj$type)

  idx_meas   <- which(unk_types == "meas_par")
  n_meas_par <- length(idx_meas)

  prior_fun_list   <- prior_fun_factory(estimated_params, n_draws)
  prior_vals       <- lapply(prior_fun_list,
                             function(prior_fun) prior_fun())

  df1 <- cbind(data.frame(iter = 1:n_draws), as.data.frame(prior_vals))


  mdl_structure <- extract_structure_from_XMILE(filepath, pars_names)
  ds_inputs     <- get_deSolve_elems(mdl_structure)

  if(!(integ_method %in% c("euler", "rk4"))) stop("Invalid integration method")

  ds_inputs <- update_sim_params(ds_inputs, start_time, stop_time, timestep)

  if(n_meas_par > 0) {

    meas_params <- estimated_params[idx_meas]

    # List of configured measurement models.
    meas_mdl_conf <- configure_meas_models(meas_mdl, meas_params, prior_vals)
  }

  df2 <- purrr::map_dfr(1:n_draws, function(i) {

    for(param in pars_names) ds_inputs$consts[[param]] <- prior_vals[[param]][[i]]

    par_list    <- unlist(purrr::transpose(prior_vals)[i], recursive = FALSE)
    readsdr_env <- list2env(par_list)

    ds_inputs$stocks <- purrr::map_dbl(ds_inputs$stocks, function(x) {

      eval(parse(text = x), envir = readsdr_env)
    })

    measurement_df <- sd_measurements(1, meas_mdl_conf[[i]],
                                      ds_inputs,
                                      integ_method = integ_method)
    measurement_df$iter <- i
    measurement_df
  })


  list(parameters   = df1,
       measurements = df2)
}


