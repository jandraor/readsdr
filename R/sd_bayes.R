#' Create Stan file for Bayesian inference
#'
#' @section Negative binomial measurement component:
#'
#' While this package aims to avoid making decisions for users whenever
#' possible, I have taken the liberty to automate the transformation of phi
#' (the concentration parameter) when using the Negative Binomial distribution
#' (\href{https://mc-stan.org/docs/functions-reference/nbalt.html}{alternative parameterisation})
#' as a measurement component. \code{sd_Bayes()} automatically creates an
#' inverse phi parameter for computational efficiency, which will be subject to
#' inference (instead of phi). Additionally, I have provided a default prior for
#' this inv_phi but users can override it as needed.
#'
#' @param meas_mdl A list of strings. Each string corresponds to a sampling
#' statement written in Stan language.
#' @param estimated_params A list of lists. Each sublist describes each
#'   parameter that will be estimated in the inference stage. To construct this
#'   description, the user can avail of the function `sd_prior`.
#' @param data_params An optional string vector defining which model
#'   parameters will be configured through the Stan data block. That is, the
#'   user will provide fixed values for such parameters at every Stan run.
#' @param data_inits An optional string vector defining which model
#'  parameters that \strong{only affect initial values} (of stocks) will be
#'  configured through the Stan data block. That is, the user will provide fixed
#'  values for such parameters at every Stan run.
#' @param LFO_CV An optional boolean that indicates whether the returned Stan
#'   file supports Leave-Future-Out Cross-Validation. This support corresponds to
#'   estimating the log-likelihood of the ts + 1 measurement.
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
                     data_inits = NULL, const_list = NULL,
                     LFO_CV = FALSE) {


  estimated_params <- get_meas_params(meas_mdl, estimated_params)
  est_params_names <- get_names(estimated_params, "par_name")

  unk_types     <- sapply(estimated_params, function(prior_obj) prior_obj$type)

  mdl_pars      <- NULL
  any_unk_const <- any(unk_types == "constant")

  if(any_unk_const) {

    const_idx <- which(unk_types == "constant")
    mdl_pars  <- sapply(estimated_params[const_idx],
                        function(prior_obj) prior_obj$par_name)
  }

  if(!is.null(data_params)) mdl_pars <- c(mdl_pars, data_params)

  params <- c(est_params_names, data_params, data_inits)

  mdl_structure <- extract_structure_from_XMILE(filepath, params,
                                                const_list = const_list)
  lvl_obj       <- mdl_structure$levels
  lvl_names     <- get_names(mdl_structure$levels)

  init_vals     <- purrr::map(lvl_obj, "initValue")
  init_vals     <- suppressWarnings(as.numeric(init_vals))

  any_unk_inits <- ifelse(any(is.na(init_vals)), TRUE, FALSE)

  ODE_fn      <- "X_model"
  stan_fun    <- stan_ode_function(func_name       = ODE_fn,
                                   pars            = mdl_pars,
                                   const_list      = const_list,
                                   XMILE_structure = mdl_structure)

  stan_data   <- stan_data(meas_mdl, any_unk_inits, LFO_CV, data_params,
                           data_inits, length(lvl_obj))

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

    if(is_string_numeric(dist_obj$phi)) return(NULL)

    par_name <- as.character(stringr::str_glue("inv_{dist_obj$phi}"))

    prior_obj <- list(par_name  = par_name,
                      dist      = "exponential",
                      beta      = 5,
                      min       = 0,
                      type      = "meas_par",
                      par_trans = "inv")

    return(prior_obj)
  }

  if(dist_obj$dist_name == "normal") {

    par_name <- as.character(stringr::str_glue("{dist_obj$sigma}"))

    prior_obj <- list(par_name  = par_name,
                      dist      = "exponential",
                      beta      = 1,
                      min       = 0,
                      type      = "meas_par")

    return(prior_obj)

  }

  NULL
}
