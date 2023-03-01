# Function generators utils

extract_meas_pars <- function(meas_data_mdl) {

  meas_mdl    <- lapply(meas_data_mdl, function(meas_obj) meas_obj$formula)
  detected_mp <- lapply(meas_mdl, extract_extra_params) %>% remove_NULL()
  detected_mp[!duplicated(detected_mp)]
}
