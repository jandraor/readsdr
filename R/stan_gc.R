stan_gc <- function(meas_mdl) {

  rhs <- get_log_lik_statement(meas_mdl)

  log_lik_asg <- paste("  log_lik =", rhs)

  block_body <- paste("  real log_lik;", log_lik_asg, sep = "\n")

  paste("generated quantities {",
        block_body,
        "}", sep = "\n")
}

get_log_lik_statement <- function(meas_mdl) {

  delta_counter <- 1
  n_meas        <- length(meas_mdl)

  ll_lines <- vector(mode = "character", length = n_meas)

  for(i in seq_len(n_meas)) {

    meas_obj        <- meas_mdl[[i]]
    decomposed_meas <- decompose_meas(meas_obj)
    dist_obj        <- get_dist_obj(decomposed_meas$rhs)
    ll_lines[[i]]   <- get_dist_dens_mass_fun(decomposed_meas$lhs, dist_obj)
  }

  ll_lines %>% paste(collapse = "+") %>% paste0(";")
}

get_dist_dens_mass_fun <- function(lhs, dist_obj) {

  if(dist_obj$dist_name == "neg_binomial_2") {

    delta_counter <- 1 # This has to change.

    ll_statement <- stringr::str_glue("neg_binomial_2_lpmf({lhs} | {dist_obj$mu}, {dist_obj$phi})")

    pattern     <- "net_flow\\(.+?\\)"
    replacement <- stringr::str_glue("delta_x_{delta_counter}")

    ll_statement <-stringr::str_replace(ll_statement, pattern, replacement)

    return(ll_statement)
  }

  msg <- stringr::str_glue("Distribution '{dist}' not supported")
  stop(msg, call. = FALSE)
}

