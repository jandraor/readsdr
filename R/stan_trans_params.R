
stan_trans_params <- function(prior, lvl_obj) {

  var_decl <- paste(
    "  array[n_obs] vector[n_difeq] x; // Output from the ODE solver",
    "  array[n_params] real params;", sep = "\n")

  if(3 == 2) {

    var_decl <- paste(var_decl,
                      "  vector[n_difeq] x0; // init values", sep = "\n")
  }

  # Assignments
  stock_init_lines <- construct_stock_init_lines(lvl_obj)
  pars_asg         <- construct_pars_asg(prior)
  run_model_line   <- "  x = ode_rk45(X_model, x0, t0, ts, params);"
  asg              <- paste(stock_init_lines, pars_asg, run_model_line,
                            sep = "\n")

  # unk_elems <- names(unk_list)
  #
  # if("measurement" %in% unk_elems) {
  #
  #   meas_obj <- lapply(unk_list$measurement, function(par_obj) {
  #
  #     obj_elems <- names(par_obj)
  #
  #     if("par_trans" %in% obj_elems) {
  #
  #       decl  <- stringr::str_glue("  real {par_obj$name};")
  #       asnmt <- get_trans_line(par_obj)
  #
  #
  #       return( list(decl  = decl,
  #                    asnmt = asnmt))
  #     }
  #
  #     NULL
  #
  #   }) %>% remove_NULL()
  #
  # }
  #
  #
  #


  # assignments <- "  //assignments"
  #
  # if(length(meas_obj) > 0) {
  #
  #   meas_decl_lines <- purrr::map_chr(meas_obj, "decl") %>% paste(sep = "\n")
  #   var_decl        <- paste(var_decl, meas_decl_lines, sep = "\n")
  #
  #   asnmt_lines <- purrr::map_chr(meas_obj, "asnmt") %>% paste(sep = "\n")
  #   assignments <- paste(assignments, asnmt_lines, sep = "\n")
  # }
  #
  # stock_inits <- construct_stock_init_lines(stock_list, unk_list)
  # assignments <- paste(assignments, stock_inits, sep = "\n")
  #
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

    if(prior_obj$type == "init") return (NULL)

    prior_obj$par_name
  }) %>%
    remove_NULL() %>%
    as.character()

  stringr::str_glue("  params[{seq_along(par_names)}] = {par_names};") %>%
    paste(collapse = "\n")

}
