#' Function factory for SBC
#' @return A function.
#' @export

#' @inheritParams read_xmile
#' @inheritParams sd_Bayes
#' @inheritParams sd_simulate
#' @examples
#'   filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")
#'   meas_mdl <- list("y ~ poisson(net_flow(C))")
#'   estimated_params <- list(
#'     sd_prior("par_beta", "lognormal", c(0, 1)),
#'     sd_prior("par_rho", "beta", c(2, 2)),
#'     sd_prior("I0", "lognormal", c(0, 1), "init"))
#'   sd_data_generator_fun(filepath, estimated_params, meas_mdl)
sd_data_generator_fun <- function(filepath, estimated_params, meas_mdl,
                                  start_time   = NULL,
                                  stop_time    = NULL,
                                  timestep     = NULL,
                                  integ_method = "euler") {

  pars_names    <- get_names(estimated_params, "par_name")

  estimated_params <- get_meas_params(meas_mdl, estimated_params)

  prior_fun_list <- prior_fun_factory(estimated_params, 1)

  mdl_structure <- extract_structure_from_XMILE(filepath, pars_names)
  ds_inputs     <- get_deSolve_elems(mdl_structure)

  if(!(integ_method %in% c("euler", "rk4"))) stop("Invalid integration method")

  ds_inputs <- update_sim_params(ds_inputs, start_time, stop_time, timestep)

  n_stocks <- length(ds_inputs$stocks)

  unk_types  <- sapply(estimated_params, function(prior_obj) prior_obj$type)
  n_consts   <- sum(unk_types == "constant")

  idx_meas   <- which(unk_types == "meas_par")
  n_meas_par <- length(idx_meas)

  data_fun <- function() {

    prior_vals <- lapply(prior_fun_list, function(prior_fun) prior_fun())

    for(param in pars_names) ds_inputs$consts[[param]] <- prior_vals[[param]]

    readsdr_env <- list2env(prior_vals)

    ds_inputs$stocks <- purrr::map_dbl(ds_inputs$stocks, function(x) {

      eval(parse(text = x), envir = readsdr_env)
    })

    if(n_meas_par > 0) {

      meas_params <- estimated_params[idx_meas]

      for(meas_par_obj in meas_params) {

        # Parameter's name before the transformation
        before_name <- stringr::str_remove(meas_par_obj$par_name,
                                            paste0(meas_par_obj$par_trans, "_"))

        trans_value <- execute_trans(prior_vals[[meas_par_obj$par_name]],
                                     meas_par_obj$par_trans)

        meas_mdl <- lapply(meas_mdl, function(meas_obj) {

          stringr::str_replace(meas_obj, before_name,
                               as.character(trans_value))
        })
      }
    }

    measurement_df <- sd_measurements(1, meas_mdl, ds_inputs,
                                      start_time   = start_time,
                                      stop_time    = stop_time,
                                      timestep     = timestep,
                                      integ_method = integ_method)

    n_obs <- length(unique(measurement_df$time))

    split_df         <- split(measurement_df, measurement_df$var_name)
    meas_list        <- lapply(split_df, function(df) df$measurement)
    names(meas_list) <- unique(measurement_df$var_name)

    specs_list <- list(
      n_obs    = n_obs,
      n_params = n_consts,
      n_difeq  = n_stocks,
      t0       = 0,
      ts       = 1:n_obs)

    list(variables = prior_vals,
         generated = c(meas_list, specs_list))
  }
}

prior_fun_factory <- function(estimated_params, n_draws) {

  n_params <- length(estimated_params)

  fun_list <- vector("list", n_params)

  for (i in 1:n_params) {

    par_obj   <- estimated_params[[i]]

    dist_name <- par_obj$dist

    arg_names_R    <- get_dist_args(dist_name, "R")
    arg_names_Stan <- get_dist_args(dist_name)
    arg_list       <- par_obj[arg_names_Stan]

    fun_name  <- Stan_to_R(dist_name)

    fun_list[[i]]      <- fun_factory(fun_name, arg_names_R, arg_list, n_draws)
    names(fun_list)[i] <- par_obj$par_name
  }

  fun_list
}

fun_factory <- function(fun_name, arg_names_R, arg_list, n_draws) {

  dist_args_txt <- paste(arg_names_R, arg_list, sep = " = ")
  n_arg         <- paste0("n = ", n_draws)
  args_txt      <- paste(c(n_arg, dist_args_txt), collapse = ", ")
  body_fun      <- stringr::str_glue("{fun_name}({args_txt })")

  rlang::new_function(args = NULL, body = rlang::parse_expr(body_fun))
}
