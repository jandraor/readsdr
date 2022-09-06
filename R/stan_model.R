stan_model <- function(estimated_params, meas_mdl, lvl_names) {

  prior_lines <- sapply(estimated_params, construct_prior_line) %>%
    paste(collapse = "\n")

  likelihood_lines <- get_likelihood_lines(meas_mdl, lvl_names)

  block_body <- paste(prior_lines, likelihood_lines, sep = "\n")

  paste("model {",
        block_body,
        "}", sep = "\n")
}

construct_prior_line <- function(prior_obj) {

  dist_args <- get_dist_args(prior_obj$dist)

  dist_pars <- prior_obj[dist_args] %>%
    as.numeric() %>%
    paste(collapse = ", ")

  stringr::str_glue("  {prior_obj$par_name} ~ {prior_obj$dist}({dist_pars});")

}

get_likelihood_lines <- function(meas_mdl, lvl_names) {

  delta_counter <- 1
  n_meas        <- length(meas_mdl)

  meas_lines <- vector(mode = "character", length = n_meas)

  for(i in seq_len(n_meas)) {
    meas_lines[[i]] <- construct_likelihood_line(meas_mdl[[i]],
                                                 delta_counter,
                                                 lvl_names)
  }

  paste(meas_lines, collapse = "\n")

}

construct_likelihood_line <- function(meas_obj, delta_counter, lvl_names) {

  decomposed_meas <- decompose_meas(meas_obj)
  lhs             <- decomposed_meas$lhs
  dist_obj        <- get_dist_obj(decomposed_meas$rhs)
  new_rhs         <- translate_lik_rhs(dist_obj, delta_counter, lvl_names)

  stringr::str_glue("  {lhs} ~ {new_rhs};")
}

translate_lik_rhs <- function(dist_obj, delta_counter, lvl_names) {

  if(dist_obj$dist_name == "normal") {

    stock_txt <- dist_obj$mu

    nf_pattern <- get_pattern_regex("net_flow")
    is_nf      <- stringr::str_detect(stock_txt, nf_pattern)

    if(is_nf) {

      new_rhs <- stringr::str_glue("normal(delta_x_{delta_counter}, {dist_obj$sigma})")
      return(new_rhs)
    }

    new_mu  <- translate_stock(stock_txt, lvl_names)
    new_rhs <- stringr::str_glue("normal({new_mu}, {dist_obj$sigma})")

    return(new_rhs)
  }

  if(dist_obj$dist_name == "neg_binomial_2") {

    stock_txt <- dist_obj$mu

    nf_pattern     <- "net_flow\\(.+?\\)"

    is_nf <- stringr::str_detect(stock_txt, nf_pattern)

    if(is_nf) {

      new_rhs <- stringr::str_glue("neg_binomial_2(delta_x_{delta_counter}, {dist_obj$phi})")
      return(new_rhs)
    }

  }

  if(dist_obj$dist_name == "poisson") {

    stock_txt <- dist_obj$lambda

    nf_pattern     <- "net_flow\\(.+?\\)"

    is_nf <- stringr::str_detect(stock_txt, nf_pattern)

    if(is_nf) {

      new_rhs <- stringr::str_glue("poisson(delta_x_{delta_counter})")
      return(new_rhs)
    }

    new_lambda <- translate_stock(stock_txt, lvl_names)
    new_rhs    <- stringr::str_glue("poisson({new_lambda})")

    return(new_rhs)
  }

  msg <- paste0("translate_lik_rhs() does not support the ",
               dist_obj$dist_name, " distribution.")

  stop(msg, call. = FALSE)
}

translate_stock <- function(stock_name, lvl_names) {

  stock_name <- stringr::str_trim(stock_name)
  idx        <- which(stock_name == lvl_names)
  stringr::str_glue("x[:, {idx}]")

}
