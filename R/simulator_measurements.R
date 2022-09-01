#' Generate measurements
#'
#' @param n_meas Number of measurements. An integer.
#' @param meas_model Measurement model. A list of strings, in which each string
#' corresponds to sampling statement in Stan language.
#'
#'@inheritParams sd_simulate
#'
#' @return A data frame.
#' @export
#'
#' @examples
#'   filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")
#'   mdl      <- read_xmile(filepath)
#'
#'   mm1        <- "y ~ poisson(C)"
#'   meas_model <- list(mm1)
#'
#'   sd_measurements(n_meas       = 2,
#'                   meas_model   = meas_model,
#'                   ds_inputs    = mdl$deSolve_components,
#'                   start_time   = 0,
#'                   stop_time    = 10,
#'                   timestep     = 1/16,
#'                   integ_method = "rk4")
sd_measurements <- function(n_meas, meas_model, ds_inputs,
                            start_time   = NULL,
                            stop_time    = NULL,
                            timestep     = NULL,
                            integ_method = "euler") {

  if(!(integ_method %in% c("euler", "rk4"))) stop("Invalid integration method")

  ds_inputs <- update_sim_params(ds_inputs, start_time, stop_time, timestep)
  X_output  <- sd_simulate(ds_inputs, integ_method = integ_method)

  purrr::map_dfr(meas_model, function(sampling_statement) {

    sampling_obj <- sampling_statement_to_list(sampling_statement)

    # measurement type (mt)
    mt <- sampling_obj$meas_type

    if(mt == "as_is") meas_df <- measurements_as_is(n_meas, X_output,
                                                           sampling_obj)

    if(mt == "net_flow") meas_df <- measurements_net_change(n_meas, X_output,
                                                            sampling_obj)

    meas_df
  })
}

measurements_as_is <- function(n_meas, X_output, sampling_obj) {

  discrete_df <- X_output[X_output$time - trunc(X_output$time) == 0, ]
  # This line below assumes that the sampling distribution's first parameter is
  #   always the measured stock.
  stk         <- sampling_obj$dist[[2]]
  stk_df      <- discrete_df[, c("time", stk)]
  time_vals   <- discrete_df[, "time"]
  vals        <- discrete_df[, stk]

  n_vals      <- length(vals)

  r_fun       <- sampling_obj$dist$dist_name

  if(length(sampling_obj$dist) == 2L) args <- list(n_vals, vals)

  if(length(sampling_obj$dist) == 3L) args <- list(n_vals, vals,
                                                   as.numeric(sampling_obj$dist[[3]]))

  names(args) <- c("n", names(sampling_obj$dist)[-1])

  purrr::map_dfr(seq_len(n_meas), function(iter) {

    data.frame(iter        = iter,
               time        = time_vals,
               var_name    = sampling_obj$meas_name,
               measurement = do.call(r_fun, args))

  })
}

measurements_net_change <- function(n_meas, X_output, sampling_obj) {

  nf            <- sampling_obj$dist[[2]]
  stk           <- stringr::str_match(nf, "net_flow\\((.+?)\\)")[[2]]
  net_change_df <- sd_net_change(X_output, stk)

  vals          <- net_change_df$value
  n_vals        <- nrow(net_change_df)

  r_fun       <- sampling_obj$dist$dist_name

  if(length(sampling_obj$dist) == 2L) args <- list(n_vals, vals)

  if(length(sampling_obj$dist) == 3L) args <- list(n_vals, vals,
                                                   as.numeric(sampling_obj$dist[[3]]))

  names(args) <- c("n", names(sampling_obj$dist)[-1])

  purrr::map_dfr(seq_len(n_meas), function(iter) {

    data.frame(iter        = iter,
               time        = net_change_df$time,
               var_name    = sampling_obj$meas_name,
               measurement = do.call(r_fun, args))

  })
}

sampling_statement_to_list <- function(sampling_statement) {

  decomposed_statement <- decompose_meas(sampling_statement)
  dist_obj             <- get_dist_obj(decomposed_statement$rhs,
                                       language = "R")
  meas_type <- "as_is"

  nf_pattern                <- get_pattern_regex("net_flow")
  is_net_flow               <- stringr::str_detect(dist_obj[[2]], nf_pattern)
  if(is_net_flow) meas_type <- "net_flow"

  list(meas_type  = meas_type,
       meas_name  = decomposed_statement$lhs,
       dist       = dist_obj)
}

