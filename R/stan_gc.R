stan_gc <- function(meas_mdl, LFO_CV, lvl_names) {

  decl <- "  real log_lik;"

  if(LFO_CV) decl <- paste(decl, "  real log_lik_pred;", sep = "\n")

  rhs         <- get_log_lik_statement(meas_mdl, FALSE, lvl_names)
  log_lik_asg <- paste("  log_lik =", rhs)

  if(LFO_CV) {

    rhs_pred    <- get_ll_pred_asg(meas_mdl, LFO_CV)
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
    ll_lines[[i]]   <- get_dist_dens_mass_fun(decomposed_meas$lhs, dist_obj,
                                              LFO_CV, lvl_names)
  }

  ll_lines %>% paste(collapse = "+") %>% paste0(";")
}

get_dist_dens_mass_fun <- function(lhs, dist_obj, LFO_CV, lvl_names) {

  delta_counter <- 1 # This has to change.

  if(dist_obj$dist_name == "neg_binomial_2") {

    ll_statement <- ifelse(
      LFO_CV,
      stringr::str_glue("neg_binomial_2_lpmf({lhs}_ahead | {lhs}_pred, {dist_obj$phi})"),
      stringr::str_glue("neg_binomial_2_lpmf({lhs} | {dist_obj$mu}, {dist_obj$phi})"))

    pattern     <- "net_flow\\(.+?\\)"
    replacement <- stringr::str_glue("delta_x_{delta_counter}")

    ll_statement <- stringr::str_replace(ll_statement, pattern, replacement)

    return(ll_statement)
  }

  if(dist_obj$dist_name == "poisson") {

    nf_pattern <- "net_flow\\(.+?\\)"
    is_nf      <- stringr::str_detect(dist_obj$lambda, nf_pattern)

    if(is_nf) {

      dist_obj$lambda <- stringr::str_glue("poisson(delta_x_{delta_counter})")
    }

    if(!is_nf) dist_obj$lambda <- translate_stock(dist_obj$lambda, lvl_names)


    ll_statement <- ifelse(
      LFO_CV,
      stringr::str_glue("poisson_lpmf({lhs}_ahead | {lhs}_pred)"),
      stringr::str_glue("poisson_lpmf({lhs} | {dist_obj$lambda})"))

    return(ll_statement)
  }

  msg <- stringr::str_glue("Distribution '{dist_obj$dist_name}' not supported")
  stop(msg, call. = FALSE)
}

get_ll_pred_asg <- function(meas_mdl, LFO_CV) {

  delta_counter <- 1
  n_meas        <- length(meas_mdl)

  ll_lines <- vector(mode = "character", length = n_meas)

  for(i in seq_len(n_meas)) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)
    dist_obj        <- get_dist_obj(decomposed_meas$rhs)
    ll_lines[[i]]   <- get_dist_dens_mass_fun(decomposed_meas$lhs, dist_obj,
                                              LFO_CV)
  }

  ll_lines %>% paste(collapse = "+") %>% paste0(";")
}

