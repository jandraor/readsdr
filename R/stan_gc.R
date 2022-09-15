stan_gc <- function(meas_mdl, LFO_CV, lvl_names) {

  decl <- "  real log_lik;"

  if(LFO_CV) decl <- paste(decl, "  real log_lik_pred;", sep = "\n")

  rhs         <- get_log_lik_statement(meas_mdl, FALSE, lvl_names)
  log_lik_asg <- paste("  log_lik =", rhs)

  if(LFO_CV) {

    rhs_pred    <- get_ll_pred_asg(meas_mdl, LFO_CV, lvl_names)
    pred_asg    <- paste("  log_lik_pred =", rhs_pred)
    log_lik_asg <- paste(log_lik_asg, pred_asg, sep = "\n")
  }

  block_body <- paste(decl, log_lik_asg, sep = "\n")

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

  ll_lines %>% paste(collapse = "+") %>% paste0(";")
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

  nf_pattern <- get_pattern_regex("net_flow")
  is_nf      <- stringr::str_detect(dist_obj[[2]], nf_pattern)

  if(is_nf)  {

    dist_obj[[2]] <- stringr::str_glue("delta_x_{delta_counter}")
    delta_counter <- delta_counter + 1
  }

  if(!is_nf) dist_obj[[2]] <- translate_stock(dist_obj[[2]], lvl_names)

  rhs <- get_density_statement(dist_obj)

  list(rhs           = rhs,
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

  if(d_name == "normal") return(stringr::str_glue("normal_lpdf({rv} | {dist_obj$mu}, {dist_obj$sigma})"))

  if(d_name == "neg_binomial_2") return(stringr::str_glue("neg_binomial_2_lpmf({rv} | {dist_obj$mu}, {dist_obj$phi})"))

  if(d_name == "poisson") return(stringr::str_glue("poisson_lpmf({rv} | {dist_obj$lambda})"))

  msg <- stringr::str_glue("get_density_statement() does not support the '{dist_obj$dist_name}' distribution.")
  stop(msg, call. = FALSE)
}

