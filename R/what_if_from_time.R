#' What if from time t we change the value of some parameters
#'
#' @param time Time at which the parameter values change
#' @param up_to_time Time from which the original values are restored.
#' @param par_list A list that indicates which parameters change from time t.
#'   For instance, if you wanted to change the value of parameter \code{c} to 4,
#'   you would provide the \code{list(c = 4)}
#'
#' @inheritParams sd_simulate
#'
#' @return A data frame
#' @export
#'
#' @examples
#'   filepath       <- system.file("models/", "SIR.stmx", package = "readsdr")
#'   mdl            <- read_xmile(filepath)
#'   ds_components  <- mdl$deSolve_components
#'   output         <- sd_what_if_from_time(3, Inf, list(c = 4), ds_components)
sd_what_if_from_time <- function(time, up_to_time = Inf, par_list, ds_inputs,
                                 start_time = NULL, stop_time = NULL,
                                 timestep = NULL, integ_method = "euler") {

  stopifnot("'up_to_time' must be greater than 'time'" = up_to_time > time)

  unchanged_consts <- ds_inputs$consts

  if(!is.null(start_time)) {
    ds_inputs$sim_params$start <- start_time
  }

  if(!is.null(stop_time)) {
    ds_inputs$sim_params$stop <- stop_time
  } else {
    stop_time <- ds_inputs$sim_params$stop
  }

  if(!is.null(timestep)) {
    ds_inputs$sim_params$dt <- timestep
  }

  mid_time   <- time

  first_run  <- sd_simulate(ds_inputs, stop_time = mid_time,
                            integ_method = integ_method)

  last_row             <- utils::tail(first_run, 1)
  stk_names            <- names(ds_inputs$stocks)
  new_stocks           <- last_row[, stk_names] %>% unlist()
  ds_inputs$stocks     <- new_stocks

  par_names                       <- names(par_list)
  ds_inputs$consts[par_names]     <- unlist(par_list)


  sr_stop_time <- min(up_to_time, stop_time) # second run stop time

  second_run <- sd_simulate(ds_inputs,
                            start_time   = mid_time,
                            stop_time    = sr_stop_time,
                            integ_method = integ_method)

  current_output <- rbind(utils::head(first_run, -1), second_run)

  if(is.infinite(up_to_time)) return(current_output)

  last_row             <- utils::tail(current_output, 1)
  stk_names            <- names(ds_inputs$stocks)
  new_stocks           <- last_row[, stk_names] %>% unlist()
  ds_inputs$stocks     <- new_stocks
  ds_inputs$consts     <- unchanged_consts

  third_run <- sd_simulate(ds_inputs,
                           start_time   = sr_stop_time,
                           stop_time    = stop_time,
                           integ_method = integ_method)

  rbind(utils::head(current_output, -1), third_run)
}
