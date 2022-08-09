
stan_trans_params <- function(prior, lvl_obj, unk_inits) {

  var_decl <- paste(
    "  array[n_obs] vector[n_difeq] x; // Output from the ODE solver",
    "  array[n_params] real params;", sep = "\n")

  if(unk_inits) {

    var_decl <- paste(var_decl,
                      "  vector[n_difeq] x0; // init values", sep = "\n")
  }

  par_trans_list <- extract_par_trans(prior) %>% remove_NULL()

  if(length(par_trans_list) > 0) {

    par_trans_decl <- sapply(par_trans_list, function(pt_obj) pt_obj$decl) %>%
      paste(collapse = "\n")

    var_decl <- paste(var_decl, par_trans_decl, sep = "\n")
  }

  # Assignments

  pars_asg <- construct_pars_asg(prior)
  asg      <- pars_asg

  if(unk_inits) {
    stock_init_lines <- construct_stock_init_lines(lvl_obj)
    asg              <- paste(stock_init_lines, asg, sep = "\n")
  }

  if(length(par_trans_list) > 0) {
    par_trans_asg <- sapply(par_trans_list, function(pt_obj) pt_obj$trans) %>%
      paste(collapse = "\n")
    asg              <- paste(par_trans_asg, asg, sep = "\n")
  }

  run_model_line <- "  x = ode_rk45(X_model, x0, t0, ts, params);"
  asg            <- paste(asg, run_model_line, sep = "\n")

  # assignments <- "  //assignments"
  # net_flow_line <- "  array[n_obs] real delta_x;" # this is optional
  #
  # block_body <- paste(var_decl,
  #                     assignments, sep = "\n")

  block_body <- paste(var_decl, asg, sep = "\n")

  paste("transformed parameters{",
        block_body,
        "}", sep = "\n")
}

get_trans_line <- function(par_obj) {

  par_trans <- par_obj$par_trans
  par_name  <- par_obj$name

  if(par_trans == "inv") {

    return(stringr::str_glue("  {par_name} = 1 / inv_{par_name};"))
  }
}

construct_stock_init_lines <- function(stock_list) {

  lines_list <- purrr::imap_chr(stock_list, function(stk_obj, i) {
    stk_name <- stk_obj$name
    stringr::str_glue("  x0[{i}] = {stk_obj$initValue}; // {stk_name}")
  })

  paste(lines_list, collapse = "\n")
}

construct_pars_asg <- function(prior) {

  par_names <- sapply(prior, function(prior_obj) {

    if(prior_obj$type == "init" | prior_obj$type == "meas_par") return (NULL)

    prior_obj$par_name
  }) %>%
    remove_NULL() %>%
    as.character()

  stringr::str_glue("  params[{seq_along(par_names)}] = {par_names};") %>%
    paste(collapse = "\n")

}

extract_par_trans <- function(prior) {

  types   <- sapply(prior, function(prior_obj) prior_obj$type)
  indexes <- which(types == "meas_par")

  meas_pars <- prior[indexes]

  lapply(meas_pars, function(par_obj) {

    obj_elems <- names(par_obj)

    if(!"par_trans" %in% obj_elems) return (NULL)

    pattern <- stringr::str_glue("{par_obj$par_trans}_")

    var_name <- stringr::str_remove(par_obj$par_name, pattern)

    list(decl  = stringr::str_glue("  real {var_name};") %>% as.character(),
         trans = trans_par(var_name, par_obj$par_trans))

  })
}

trans_par <- function(var_name, par_trans) {

  if(par_trans == "inv") {
    return(stringr::str_glue("  {var_name} = 1 / inv_{var_name};") %>%
      as.character())
  }

  msg <- stringr::str_glue("Parameter transformation {par_trans} not supported")
  stop(msg, call. = FALSE)
}
