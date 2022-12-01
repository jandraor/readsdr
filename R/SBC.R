sd_data_generator_fun <- function(estimated_params, meas_mdl) {

  prior_fun_list <- prior_fun_factory(estimated_params)

  data_fun <- function() {

    par_beta <- prior_fun_list$par_beta()
    par_rho  <- prior_fun_list$par_rho()
    I0       <- prior_fun_list$I0()

    ds_inputs$consts[["par_beta"]] <- par_beta
    ds_inputs$consts[["par_rho"]]    <- par_rho

    N_val  <- ds_inputs$consts[["N"]]

    ds_inputs$stocks[["S"]] <- N_val - par_I0
    ds_inputs$stocks[["I"]] <- par_I0

    measurement_df <- sd_measurements(1, meas_mdl, ds_inputs, 0, 80,
                                      timestep = 1/32, integ_method = "rk4")

    n_obs <- nrow(measurement_df)

    list(
      variables = list(
        par_beta   = par_beta,
        par_rho    = par_rho,
        I0         = par_I0),
      generated =   list(
        n_obs    = n_obs,
        y        = measurement_df$measurement,
        n_params = 2,
        n_difeq  = 5,
        t0       = 0,
        ts       = 1:n_obs))
  }
}

prior_fun_factory <- function(estimated_params) {

  n_params <- length(estimated_params)

  fun_list <- vector("list", n_params)

  for (i in 1:n_params) {

    par_obj   <- estimated_params[[i]]

    dist_name <- par_obj$dist

    arg_names_R    <- get_dist_args(dist_name, "R")
    arg_names_Stan <- get_dist_args(dist_name)
    arg_list       <- par_obj[arg_names_Stan]

    fun_name  <- Stan_to_R(dist_name)

    fun_list[[i]]      <- fun_factory(fun_name, arg_names_R, arg_list)
    names(fun_list)[i] <- par_obj$par_name
  }

  fun_list
}

fun_factory <- function(fun_name, arg_names_R, arg_list) {

  dist_args_txt <- paste(arg_names_R, arg_list, sep = " = ")
  n_arg         <- "n = 1"
  args_txt      <- paste(c(n_arg, dist_args_txt), collapse = ", ")
  body_fun      <- stringr::str_glue("{fun_name}({args_txt })")

  rlang::new_function(args = NULL, body = rlang::parse_expr(body_fun))
}
