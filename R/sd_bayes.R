sd_Bayes <- function(filepath, meas_mdl, unk_list) {

  ODE_fn      <- "X_model"
  stan_fun    <- stan_ode_function(filepath, ODE_fn)

  meas_vars   <- purrr::map_chr(meas_mdl, "meas_name")
  dist_names  <- purrr::map_chr(meas_mdl, c("dist", "name"))
  dist_types  <- dist_type(dist_names)
  stan_data   <- stan_data(meas_vars, dist_types)

  stan_params     <- stan_params(unk_list)
  model_structure <- extract_structure_from_XMILE(filepath)

  stan_tp     <- stan_trans_params(unk_list, model_structure$levels)
  stan_model  <- ""
  stan_gc     <- ""

  paste(stan_fun, stan_data, stan_params,
        stan_tp, stan_model, stan_gc, sep = "\n")

}
