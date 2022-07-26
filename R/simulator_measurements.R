#' Generate measurements
#'
#' @param n_meas Number of measurements
#' @param meas_model Measurement model. A list of sampling distributions. Each
#' sampling distribution is a list of three elements:
#'
#'   - \code{stock_name} (a string): refers to the stock upon which measurements are made;
#'
#'   - \code{meas_type} (a string): indicates whether the measurement is performed on the
#'  value of the stock ('as_is') or its net change ('net_change');
#'
#'   - \code{dist} (a list): The probability distribution used to model measurements on
#'   the stock. This list also consists of three elements:
#'   name (RNG function such as rnorm), stock_par (indicates which distribution's
#'   parameter corresponds to the measured stock), and extra_pars, should the
#'   probability distributions requires additional parameters.
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
#'   mm1 <- list(stock_name = "C",
#'               meas_type  = "as_is",
#'               dist       = list(name       = "rnbinom",
#'                                 stock_par  = "mu",
#'                                 extra_pars = list(size = 0.1)))
#'   meas_model <- list(mm1)
#'
#'   sd_measurements(n_meas       = 2,
#'                   meas_model   = meas_model,
#'                   ds_inputs    = mdl$deSolve_components,
#'                   start_time   = 0,
#'                   stop_time    = 10,
#'                   timestep     = 1/16,
#'                   integ_method = "rk4")
sd_measurements <- function(n_meas, ds_inputs, meas_model,
                            start_time   = NULL,
                            stop_time    = NULL,
                            timestep     = NULL,
                            integ_method = "euler") {

  if(!(integ_method %in% c("euler", "rk4"))) stop("Invalid integration method")

  ds_inputs <- update_sim_params(ds_inputs, start_time, stop_time, timestep)
  X_output  <- sd_simulate(ds_inputs, integ_method = integ_method)


  purrr::map_dfr(meas_model, function(sampling_dist) {

    meas_type <- sampling_dist$meas_type

    if(meas_type == "as_is") {

      meas_df <- measurements_as_is(n_meas, X_output, sampling_dist)
    }

    if(meas_type == "net_change") {

      meas_df <- measurements_net_change(n_meas, X_output, sampling_dist)

    }

    meas_df



  })
}

measurements_as_is <- function(n_meas, X_output, sampling_dist) {

  discrete_df <- X_output[X_output$time - trunc(X_output$time) == 0, ]
  stk         <- sampling_dist$stock_name
  stk_df      <- discrete_df[, c("time", stk)]
  time_vals   <- discrete_df[, "time"]
  vals        <- discrete_df[, stk]
  n_vals      <- length(vals)

  r_fun       <- sampling_dist$dist$name
  args        <- list(n_vals, vals)
  names(args) <- c("n", sampling_dist$dist$stock_par)

  if("extra_pars" %in% names(sampling_dist$dist)) {
    args <- c(args, sampling_dist$dist$extra_pars)
  }

  purrr::map_dfr(seq_len(n_meas), function(iter) {

    data.frame(iter        = iter,
               time        = time_vals,
               var_name    = stk,
               measurement = do.call(r_fun, args))

  })
}

measurements_net_change <- function(n_meas, X_output, sampling_dist) {

  stk           <- sampling_dist$stock_name
  net_change_df <- sd_net_change(X_output, stk)
  vals          <- net_change_df$value
  n_vals        <- nrow(net_change_df)

  r_fun       <- sampling_dist$dist$name
  args        <- list(n_vals, vals)
  names(args) <- c("n", sampling_dist$dist$stock_par)

  if("extra_pars" %in% names(sampling_dist$dist)) {
    args <- c(args, sampling_dist$dist$extra_pars)
  }

  purrr::map_dfr(seq_len(n_meas), function(iter) {

    data.frame(iter        = iter,
               time        = net_change_df$time,
               var_name    = paste0("delta_", stk),
               measurement = do.call(r_fun, args))

  })
}

