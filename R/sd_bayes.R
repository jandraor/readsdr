#' Create Stan file for Bayesian inference
#'
#' @param meas_mdl A list
#' @param estimated_parameters A list
#' @param data_parameters A optional string vector defining which model
#"   parameters will be configured through the Stan data block. That is, the
#"   user will provide fixed values for such parameters at every Stan run.
#'
#' @inheritParams read_xmile
#'
#' @return A string
#' @export
#'
#' @examples
#'   filepath         <- system.file("models/", "SEIR.stmx", package = "readsdr")
#'   mm1              <- "y ~ neg_binomial_2(net_flow(C), phi)"
#'   meas_mdl         <- list(mm1)
#'   estimated_params <- list(
#'     sd_prior("par_beta", "lognormal", c(0, 1)),
#'     sd_prior("par_rho", "beta", c(2, 2)),
#'     sd_prior("I0", "lognormal", c(0, 1), "init"))
#'   sd_Bayes(filepath, meas_mdl, estimated_params)
sd_Bayes <- function(filepath, meas_mdl, estimated_params, data_params = NULL,
                     const_list = NULL, LFO_CV = FALSE) {

  extra_params <- lapply(meas_mdl, extract_extra_params) %>% remove_NULL()

  if(length(extra_params) > 0) estimated_params <- c(estimated_params,
                                                     extra_params)

  unk_types     <- sapply(estimated_params, function(prior_obj) prior_obj$type)
  any_unk_inits <- any(unk_types == "init")

  unk_inits <- NULL

  if(any_unk_inits) {

    inits_idx <- which(unk_types == "init")

    unk_inits <- sapply(estimated_params[inits_idx ],
                        function(prior_obj) prior_obj$par_name)
  }

  mdl_pars      <- NULL
  any_unk_const <- any(unk_types == "constant")

  if(any_unk_const) {

    const_idx <- which(unk_types == "constant")
    mdl_pars  <- sapply(estimated_params[const_idx],
                        function(prior_obj) prior_obj$par_name)
  }

  if(!is.null(data_params)) mdl_pars <- c(mdl_pars, data_params)

  mdl_structure <- extract_structure_from_XMILE(filepath, unk_inits)
  lvl_obj       <- mdl_structure$levels
  lvl_names   <- get_names(mdl_structure$levels)

  ODE_fn      <- "X_model"
  stan_fun    <- stan_ode_function(func_name       = ODE_fn,
                                   pars            = mdl_pars,
                                   const_list      = const_list,
                                   XMILE_structure = mdl_structure)

  stan_data   <- stan_data(meas_mdl, any_unk_inits, LFO_CV, data_params)

  stan_params <- stan_params(estimated_params)

  stan_tp     <- stan_trans_params(estimated_params, meas_mdl, lvl_obj,
                                   any_unk_inits, data_params, LFO_CV)

  stan_model  <- stan_model(estimated_params, meas_mdl, lvl_names)

  stan_gc     <- stan_gc(meas_mdl, LFO_CV, lvl_names)

  stan_file <- paste(stan_fun, stan_data, stan_params,
                     stan_tp, stan_model, stan_gc, "", sep = "\n")

  stan_file
}

extract_extra_params <- function(meas_obj) {

  decomposed_meas <- decompose_meas(meas_obj)
  dist_obj        <- get_dist_obj(decomposed_meas$rhs)

  if(dist_obj$dist_name == "neg_binomial_2") {

    par_name <- as.character(stringr::str_glue("inv_{dist_obj$phi}"))

    prior_obj <- list(par_name  = par_name,
                      dist      = "exponential",
                      beta      = 5,
                      min       = 0,
                      type      = "meas_par",
                      par_trans = "inv")

    return(prior_obj)
  }

  NULL
}
