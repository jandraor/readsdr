stan_gc <- function(meas_mdl, LFO_CV, lvl_names) {

  decl <- "  real log_lik;"

  sim_data_obj <- generate_sim_data_lines(meas_mdl, lvl_names)

  if(LFO_CV) decl <- paste(decl, "  real log_lik_pred;", sep = "\n")

  rhs         <- get_log_lik_statement(meas_mdl, FALSE, lvl_names)
  log_lik_asg <- paste("  log_lik =", rhs)

  if(LFO_CV) {

    rhs_pred    <- get_ll_pred_asg(meas_mdl, LFO_CV, lvl_names)
    pred_asg    <- paste("  log_lik_pred =", rhs_pred)
    log_lik_asg <- paste(log_lik_asg, pred_asg, sep = "\n")
  }

  block_body <- paste(decl,
                      sim_data_obj$decl,
                      log_lik_asg,
                      sim_data_obj$assign,
                      sep = "\n")

  paste("generated quantities {",
        block_body,
        "}", sep = "\n")
}

get_log_lik_statement <- function(meas_mdl, LFO_CV, lvl_names) {

  delta_counter <- 1
  n_meas        <- length(meas_mdl)

  ll_lines <- vector(mode = "character", length = n_meas)

  for(i in seq_len(n_meas)) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)
    dist_obj        <- get_dist_obj(decomposed_meas$rhs)
    prob_fun_obj    <- get_dist_dens_mass_fun(decomposed_meas$lhs, dist_obj,
                                              LFO_CV, lvl_names, delta_counter)

    delta_counter <- prob_fun_obj$delta_counter
    ll_lines[[i]] <- prob_fun_obj$rhs
  }

  ll_lines |> paste(collapse = "+") %>% paste0(";")
}

get_dist_dens_mass_fun <- function(lhs, dist_obj, LFO_CV, lvl_names,
                                   delta_counter) {

  if(LFO_CV) {

    dist_obj$random_var <- paste(lhs, "ahead", sep = "_")
    dist_obj[[2]]       <- paste(lhs, "pred", sep = "_")

    rhs <- get_density_statement(dist_obj)

    return(list(rhs           = rhs,
                delta_counter = delta_counter))
  }

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

get_ll_pred_asg <- function(meas_mdl, LFO_CV, lvl_names) {

  delta_counter <- 1
  n_meas        <- length(meas_mdl)

  ll_lines <- vector(mode = "character", length = n_meas)

  for(i in seq_len(n_meas)) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)
    dist_obj        <- get_dist_obj(decomposed_meas$rhs)
    prob_fun_obj    <- get_dist_dens_mass_fun(decomposed_meas$lhs, dist_obj,
                                              LFO_CV, lvl_names, delta_counter)

    delta_counter <- prob_fun_obj$delta_counter
    ll_lines[[i]] <- prob_fun_obj$rhs
  }

  ll_lines %>% paste(collapse = "+") %>% paste0(";")
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

generate_sim_data_lines <- function(meas_mdl, lvl_names) {

  n_meas <- length(meas_mdl)

  decl_lines   <- vector(mode = "character", length = n_meas)
  assign_lines <- vector(mode = "character", length = n_meas)

  delta_counter <- 1

  for(i in seq_along(meas_mdl)) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)

    lhs             <- decomposed_meas$lhs
    type            <- get_dist_type(decomposed_meas$rhs)
    decl_lines[[i]] <- stringr::str_glue("  array[n_obs] {type} sim_{lhs};")

    dist_obj          <- get_dist_obj(decomposed_meas$rhs)
    dname             <- dist_obj$dist_name

    stock_txt  <- dist_obj[[2]]

    translation_obj <- translate_stock_text(stock_txt, delta_counter, lvl_names)

    delta_counter <- translation_obj$delta_counter
    pars          <- translation_obj$stock_txt

    if(length(dist_obj) == 3L) pars <- paste(pars, dist_obj[[3]], sep = ", ")

    assign_lines[[i]] <- stringr::str_glue("  sim_{lhs} = {dname}_rng({pars});")
  }

  decl   <- paste(decl_lines, collapse = "\n")
  assign <- paste(assign_lines, collapse = "\n")

  list(decl   = decl,
       assign = assign)
}
