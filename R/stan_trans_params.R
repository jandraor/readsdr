
stan_trans_params <- function(estimated_params, meas_mdl, lvl_obj, unk_inits,
                              data_params, LFO_CV) {

  n_difeq <- length(lvl_obj)

  sim_output_decl <- ifelse(
    LFO_CV,
    stringr::str_glue("  array[n_obs + 1] vector[{n_difeq}] x; // Output from the ODE solver"),
    stringr::str_glue("  array[n_obs] vector[{n_difeq}] x; // Output from the ODE solver"))

  par_assigned_names <- pars_assigned(estimated_params, data_params)
  n_params           <- length(par_assigned_names)

  var_decl <- paste(sim_output_decl,
                    stringr::str_glue("  array[{n_params}] real params;"),
                    sep = "\n")

  if(unk_inits) {

    var_decl <- paste(var_decl,
                      stringr::str_glue("  vector[{n_difeq}] x0; // init values"), sep = "\n")
  }

  delta_meas <- subset_delta_meas(meas_mdl) |> remove_NULL()

  if(length(delta_meas) > 0) {

    delta_decl_list <- extract_delta_decl(delta_meas)
    delta_decl      <- paste(delta_decl_list, collapse = "\n")
    var_decl        <- paste(var_decl, delta_decl, sep = "\n")
  }

  par_trans_list <- extract_par_trans(estimated_params) |>  remove_NULL()

  if(length(par_trans_list) > 0) {

    par_trans_decl <- sapply(par_trans_list, function(pt_obj) pt_obj$decl) |>
      paste(collapse = "\n")

    var_decl <- paste(var_decl, par_trans_decl, sep = "\n")
  }

  if(LFO_CV) {
    var_decl <- paste(var_decl, "  real y_pred;", sep = "\n")
  }

  # Assignments

  pars_asg <- construct_pars_asg(par_assigned_names)
  asg      <- pars_asg

  if(unk_inits) {
    stock_init_lines <- construct_stock_init_lines(lvl_obj)
    asg              <- paste(stock_init_lines, asg, sep = "\n")
  }

  if(length(par_trans_list) > 0) {
    par_trans_asg <- sapply(par_trans_list, function(pt_obj) pt_obj$trans) |>
      paste(collapse = "\n")
    asg              <- paste(par_trans_asg, asg, sep = "\n")
  }

  run_model_line <- "  x = ode_rk45(X_model, x0, t0, ts, params);"
  asg            <- paste(asg, run_model_line, sep = "\n")

  lvl_names  <- get_names(lvl_obj)

  if(length(delta_meas) > 0) {

    delta_first_asg <- get_delta_first_asg(delta_meas, lvl_obj)

    for_body  <- get_for_body(delta_meas, lvl_obj)

    for_lines <- paste("  for (i in 1:n_obs-1) {",
                       for_body, "  }",
                       sep = "\n")

    delta_asg <- paste(delta_first_asg, for_lines, sep = "\n")

    asg <- paste(asg, delta_asg, sep = "\n")

    if(LFO_CV) asg <- paste(asg, get_pred_asg(meas_mdl, lvl_names, "delta_meas"), sep = "\n")
  }

  if(length(delta_meas) == 0 & LFO_CV) {

    y_pred_asg <- get_pred_asg(meas_mdl, lvl_names, "stock_meas")
    asg        <- paste(asg, y_pred_asg, sep = "\n")
  }

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

construct_stock_init_lines <- function(stock_list, unk_init_list) {

  lines_list <- purrr::imap_chr(stock_list, function(stk_obj, i) {
    stk_name <- stk_obj$name
    stringr::str_glue("  x0[{i}] = {stk_obj$initValue}; // {stk_name}")
  })

  paste(lines_list, collapse = "\n")
}

construct_pars_asg <- function(par_names) {

  stringr::str_glue("  params[{seq_along(par_names)}] = {par_names};") |>
    paste(collapse = "\n")
}

extract_par_trans <- function(estimated_params) {

  types   <- sapply(estimated_params, function(prior_obj) prior_obj$type)
  indexes <- which(types == "meas_par")

  meas_pars <- estimated_params[indexes]

  lapply(meas_pars, function(par_obj) {

    obj_elems <- names(par_obj)

    if(!"par_trans" %in% obj_elems) return (NULL)

    pattern <- stringr::str_glue("{par_obj$par_trans}_")

    var_name <- stringr::str_remove(par_obj$par_name, pattern)

    list(decl  = stringr::str_glue("  real {var_name};") |>  as.character(),
         trans = trans_par(var_name, par_obj$par_trans))

  })
}

trans_par <- function(var_name, par_trans) {

  if(par_trans == "inv") {
    return(stringr::str_glue("  {var_name} = 1 / inv_{var_name};") |>
      as.character())
  }

  msg <- stringr::str_glue("Parameter transformation {par_trans} not supported")
  stop(msg, call. = FALSE)
}

subset_delta_meas <- function(meas_mdl) {

  lapply(meas_mdl, function(meas_obj) {

    pattern          <- "net_flow\\(.+?\\)"
    pattern_detected <- stringr::str_detect(meas_obj, pattern)

    if(!pattern_detected) return (NULL)

    meas_obj
  })
}

extract_delta_decl <- function(meas_mdl) {

  lapply(seq_along(meas_mdl), function(i) {

    meas_obj <- meas_mdl[[1]]
    as.character(stringr::str_glue("  array[n_obs] real delta_x_{i};"))
  })
}

get_delta_first_asg <- function(meas_mdl, lvl_obj) {

  lvl_names <- get_names(lvl_obj)

  lapply(seq_along(meas_mdl), function(i) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)
    rhs             <- decomposed_meas$rhs

    lvl_name <- stringr::str_match(rhs, "net_flow\\((.+?)\\)")[[2]] |>
      stringr::str_trim()

    idx <- which(lvl_name == lvl_names)

    stringr::str_glue("  delta_x_{i}[1] =  x[1, {idx}] - x0[{idx}] + 1e-5;")
  }) %>% paste(collapse = "\n")

}

get_for_body <- function(meas_mdl, lvl_obj) {


  lvl_names <- get_names(lvl_obj)

  lapply(seq_along(meas_mdl), function(i) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)
    rhs             <- decomposed_meas$rhs

    lvl_name <- stringr::str_match(rhs, "net_flow\\((.+?)\\)")[[2]] |>
      stringr::str_trim()

    idx <- which(lvl_name == lvl_names)

    stringr::str_glue("    delta_x_{i}[i + 1] = x[i + 1, {idx}] - x[i, {idx}] + 1e-5;")
  }) |>  paste(collapse = "\n")

}

get_pred_asg <- function(meas_mdl, lvl_names, meas_type) {

  lapply(seq_along(meas_mdl), function(i) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)
    lhs             <- decomposed_meas$lhs
    rhs             <- decomposed_meas$rhs

    if(meas_type == "delta_meas") {

      lvl_name <- stringr::str_match(rhs, "net_flow\\((.+?)\\)")[[2]] |>
        stringr::str_trim()

      idx <- which(lvl_name == lvl_names)
      pred_asg <- stringr::str_glue("  {lhs}_pred = x[n_obs + 1, {idx}] - x[n_obs, {idx}];")
    }

    if(meas_type == "stock_meas") {

       dist_obj <- get_dist_obj(rhs)
       lvl_name <- dist_obj[[2]]
       idx      <- which(lvl_name == lvl_names)
       pred_asg <- stringr::str_glue("  {lhs}_pred = x[n_obs + 1, {idx}];")
    }

    pred_asg

  }) |>  paste(collapse = "\n")
}

pars_assigned <- function(estimated_params, data_params) {

  par_names <- sapply(estimated_params, function(prior_obj) {

    if(prior_obj$type == "init" | prior_obj$type == "meas_par") return (NULL)

    prior_obj$par_name
  }) |> remove_NULL() |> as.character()

  if(!is.null(data_params)) par_names <- c(par_names, data_params)

  par_names
}
