stan_gc <- function(meas_mdl, lvl_names, forecast = FALSE) {

  ll_decl <- "  real log_lik;"

  sim_data_obj <- generate_sim_data_lines(meas_mdl, lvl_names, forecast)

  rhs         <- get_log_lik_statement(meas_mdl, lvl_names)
  log_lik_asg <- paste("  log_lik =", rhs)

  decl <- paste(ll_decl,
                sim_data_obj$decl,
                sep = "\n")

  sim_data_asg <- sim_data_obj$assign

  if(forecast > 0) {

    fcst_decl <- create_fcst_declaration(lvl_names, meas_mdl)
    decl      <- paste(decl, fcst_decl, sep = "\n")

    fcst_sim_lines <- create_fcst_sim_lines(lvl_names, meas_mdl)
    sim_data_asg   <- paste(fcst_sim_lines, sim_data_asg, sep = "\n")
  }

  block_body <- paste(decl,
                      log_lik_asg,
                      sim_data_asg,
                      sep = "\n")

  paste("generated quantities {", block_body, "}", sep = "\n")
}

get_log_lik_statement <- function(meas_mdl, lvl_names) {

  delta_counter <- 1
  n_meas        <- length(meas_mdl)

  ll_lines <- vector(mode = "character", length = n_meas)

  for(i in seq_len(n_meas)) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)
    dist_obj        <- get_dist_obj(decomposed_meas$rhs)
    prob_fun_obj    <- get_dist_dens_mass_fun(decomposed_meas$lhs, dist_obj,
                                              lvl_names, delta_counter)

    delta_counter <- prob_fun_obj$delta_counter
    ll_lines[[i]] <- prob_fun_obj$rhs
  }

  ll_lines |> paste(collapse = " +\n    ") |>  paste0(";")
}

get_dist_dens_mass_fun <- function(lhs, dist_obj, lvl_names, delta_counter) {

  dist_obj$random_var <- lhs

  stock_txt       <- dist_obj[[2]]
  translation_obj <- translate_stock_text(stock_txt, delta_counter, lvl_names)

  delta_counter <- translation_obj$delta_counter
  new_stock_txt <- translation_obj$stock_txt

  dist_obj[[2]] <- new_stock_txt

  rhs <- get_density_statement(dist_obj)

  list(rhs           = as.character(rhs),
       delta_counter = delta_counter)
}

get_density_statement <- function(dist_obj) {

  d_name <- dist_obj$dist_name
  rv     <- dist_obj$random_var

  if(d_name == "lognormal") return(stringr::str_glue("lognormal_lpdf({rv} | {dist_obj$mu}, {dist_obj$sigma})"))

  if(d_name == "normal") return(stringr::str_glue("normal_lpdf({rv} | {dist_obj$mu}, {dist_obj$sigma})"))

  if(d_name == "neg_binomial_2") return(stringr::str_glue("neg_binomial_2_lpmf({rv} | {dist_obj$mu}, {dist_obj$phi})"))

  if(d_name == "poisson") return(stringr::str_glue("poisson_lpmf({rv} | {dist_obj$lambda})"))

  msg <- stringr::str_glue("get_density_statement() does not support the '{dist_obj$dist_name}' distribution.")
  stop(msg, call. = FALSE)
}

generate_sim_data_lines <- function(meas_mdl, lvl_names, fcst) {

  n_meas <- length(meas_mdl)

  n_lines <- ifelse(fcst > 0, n_meas * 2, n_meas)

  decl_lines   <- vector(mode = "character", length = n_lines)
  assign_lines <- vector(mode = "character", length = n_lines)

  delta_counter <- 1
  fcst_counter  <- 1

  for(i in seq_along(meas_mdl)) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)

    lhs             <- decomposed_meas$lhs
    type            <- get_dist_type(decomposed_meas$rhs)

    ms <- determine_meas_size(decomposed_meas$rhs) # Measurement size

    if(ms == 1) decl_line <- stringr::str_glue("  {type} sim_{lhs};")

    if(ms == Inf) decl_line <- stringr::str_glue("  array[n_obs] {type} sim_{lhs};")

    decl_lines[[i]] <- decl_line

    dist_obj          <- get_dist_obj(decomposed_meas$rhs)
    dname             <- dist_obj$dist_name

    stock_txt  <- dist_obj[[2]]

    translation_obj <- translate_stock_text(stock_txt, delta_counter, lvl_names)

    delta_counter <- translation_obj$delta_counter
    pars          <- translation_obj$stock_txt

    if(length(dist_obj) == 3L) pars <- paste(pars, dist_obj[[3]], sep = ", ")

    assign_lines[[i]] <- stringr::str_glue("  sim_{lhs} = {dname}_rng({pars});")

    if(fcst > 0) {

      pred_decl                <- create_pred_decl(lhs, type, ms)
      decl_lines[[i + n_meas]] <- pred_decl

      assign_lines[[i + n_meas]] <- ""

      if(ms != 1) {

        translation_obj <- translate_stock_text(stock_txt, fcst_counter,
                                                lvl_names, "x_fcst")

        fcst_counter  <- translation_obj$delta_counter
        pars          <- translation_obj$stock_txt

        if(length(dist_obj) == 3L) pars <- paste(pars, dist_obj[[3]], sep = ", ")

        assign_lines[[i + n_meas]] <- stringr::str_glue("  fcst_{lhs} = {dname}_rng({pars});")
      }
    }
  }

  decl   <- decl_lines[decl_lines != ""] |> paste(collapse = "\n")
  assign <- assign_lines[assign_lines != ""] |> paste(collapse = "\n")

  list(decl   = decl,
       assign = assign)
}

create_pred_decl <- function(lhs, type, ms) {

  if(ms == 1) decl_line <- ""

  if(ms == Inf) decl_line <- stringr::str_glue("  array[n_fcst] {type} fcst_{lhs};")

  decl_line
}

create_fcst_declaration <- function(lvl_names, meas_mdl) {

  n_lvl <- length(lvl_names)

  var_decl_lines <- paste(
    stringr::str_glue("  array[n_fcst] vector[{n_lvl}] x_fcst; // Forecast"),
    stringr::str_glue("  array[n_fcst] real t_fcst;"),
    stringr::str_glue("  vector[{n_lvl}] x_fcst0; // Forecast init values"),
    sep = "\n")

  delta_meas <- subset_delta_meas(meas_mdl) |> remove_NULL()

  if(length(delta_meas) > 0) {

    delta_decl_list <- extract_delta_decl(delta_meas,
                                            n_var = "n_fcst",
                                          var_name = "x_fcst")
    delta_decl      <- paste(delta_decl_list, collapse = "\n")
    var_decl_lines  <- paste(var_decl_lines, delta_decl, sep = "\n")
  }

  var_decl_lines
}

create_fcst_sim_lines <- function(lvl_names, meas_mdl) {

  sim_lines <- paste(
    "  // Simulate forecast",
    "  x_fcst0 = x[n_obs, :];",
    stringr::str_glue("  t_fcst = linspaced_array(n_fcst, 1, n_fcst);"),
    "  x_fcst = ode_rk45(X_model, x_fcst0, 0, t_fcst, params);",
    sep = "\n")

  delta_meas <- subset_delta_meas(meas_mdl) |> remove_NULL()

  if(length(delta_meas) > 0) {

    delta_first_asg <- get_delta_first_asg(delta_meas,
                                           lvl_names,
                                           "x_fcst")

    for_body  <- get_for_body(delta_meas, lvl_names, "x_fcst")

    for_lines <- paste("  for (i in 1:n_fcst-1) {",
                       for_body, "  }",
                       sep = "\n")

    delta_asg <- paste(delta_first_asg, for_lines, sep = "\n")

    sim_lines <- paste(sim_lines, delta_asg, sep = "\n")
  }

  sim_lines
}
