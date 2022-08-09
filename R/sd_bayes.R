sd_Bayes <- function(filepath, prior, meas_mdl) {

  mdl_structure <- extract_structure_from_XMILE(filepath)

  ODE_fn      <- "X_model"
  stan_fun    <- stan_ode_function(filepath, ODE_fn)

  extra_priors <- lapply(meas_mdl, extract_extra_prior) %>% remove_NULL()

  if(length(extra_priors) > 0) prior <- c(prior, extra_priors)

  unk_types   <- sapply(prior, function(prior_obj) prior_obj$type)
  unk_inits   <- any(unk_types == "init")

  stan_data   <- stan_data(meas_mdl, unk_inits)

  lvl_obj       <- mdl_structure$levels

  stan_params   <- stan_params(prior)

  stan_tp     <- stan_trans_params(prior, mdl_structure$levels, unk_inits)
  stan_model  <- stan_model(prior, meas_mdl)
  stan_gc     <- stan_gc(meas_mdl)

  paste(stan_fun, stan_data, stan_params,
        stan_tp, stan_model, stan_gc, sep = "\n") |> cat()

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
