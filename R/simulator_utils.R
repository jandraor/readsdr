configure_meas_models <- function(meas_mdl, meas_params, prior_vals) {

  for(meas_par_obj in meas_params) {

    # Parameter's name before the transformation
    before_name <- stringr::str_remove(meas_par_obj$par_name,
                                       paste0(meas_par_obj$par_trans, "_"))

    trans_value <- execute_trans(prior_vals[[meas_par_obj$par_name]],
                                 meas_par_obj$par_trans)

    configured_mdl <- lapply(trans_value, function(val) {

      lapply(meas_mdl, function(meas_obj) {

        stringr::str_replace(meas_obj, before_name,
                             as.character(val))
      })
    })
  }

  configured_mdl
}
