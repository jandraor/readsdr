#' Create Stan file for Bayesian inference
#'
#' @param meas_mdl A list
#' @param prior A list
#'
#' @inheritParams read_xmile
#'
#' @return A string
#' @export
#'
#' @examples
#'   filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")
#'   mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
#'   meas_mdl <- list(mm1)
#'   prior <- list(
#'     sd_prior("par_beta", "lognormal", c(0, 1)),
#'     sd_prior("par_rho", "beta", c(2, 2)),
#'     sd_prior("I0", "lognormal", c(0, 1), "init"))
#'   sd_Bayes(filepath, meas_mdl, prior)
sd_Bayes <- function(filepath, meas_mdl, prior, LFO_CV = FALSE) {

  extra_priors <- lapply(meas_mdl, extract_extra_prior) %>% remove_NULL()

  if(length(extra_priors) > 0) prior <- c(prior, extra_priors)

  unk_types     <- sapply(prior, function(prior_obj) prior_obj$type)
  any_unk_inits <- any(unk_types == "init")

  unk_inits <- NULL

  if(any_unk_inits) {

    inits_idx <- which(unk_types == "init")

    unk_inits <- sapply(prior[inits_idx ],
                        function(prior_obj) prior_obj$par_name)
  }

  mdl_pars      <- NULL
  any_unk_const <- any(unk_types == "constant")

  if(any_unk_const) {

    const_idx <- which(unk_types == "constant")
    mdl_pars  <- sapply(prior[const_idx],
                        function(prior_obj) prior_obj$par_name)
  }

  mdl_structure <- extract_structure_from_XMILE(filepath, unk_inits)

  ODE_fn      <- "X_model"
  stan_fun    <- stan_ode_function(func_name       = ODE_fn,
                                   pars            = mdl_pars,
                                   XMILE_structure = mdl_structure)

  stan_data   <- stan_data(meas_mdl, any_unk_inits, LFO_CV)

  stan_params <- stan_params(prior)

  stan_tp     <- stan_trans_params(prior, meas_mdl, mdl_structure$levels,
                                   any_unk_inits, LFO_CV)

  stan_model  <- stan_model(prior, meas_mdl)
  stan_gc     <- stan_gc(meas_mdl, LFO_CV)

  stan_file <- paste(stan_fun, stan_data, stan_params,
                     stan_tp, stan_model, stan_gc, "", sep = "\n")

  stan_file
}

extract_extra_prior <- function(meas_obj) {

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
