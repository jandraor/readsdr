stan_model <- function(estimated_params, meas_mdl, lvl_names) {

  prior_lines <- sapply(estimated_params, construct_prior_line) |>
    paste(collapse = "\n")

  likelihood_lines <- get_likelihood_lines(meas_mdl, lvl_names)

  block_body <- paste(prior_lines, likelihood_lines, sep = "\n")

  paste("model {",
        block_body,
        "}", sep = "\n")
}

construct_prior_line <- function(prior_obj) {

  dist_args <- get_dist_args(prior_obj$dist)

  dist_pars <- prior_obj[dist_args] |>
    as.numeric() |>
    paste(collapse = ", ")

  stringr::str_glue("  {prior_obj$par_name} ~ {prior_obj$dist}({dist_pars});")

}

get_likelihood_lines <- function(meas_mdl, lvl_names) {

  delta_counter <- 1
  n_meas        <- length(meas_mdl)

  meas_lines <- vector(mode = "character", length = n_meas)

  for(i in seq_len(n_meas)) {

    meas_obj <- meas_mdl[[i]]
    ll_obj   <- construct_likelihood_line(meas_obj, delta_counter, lvl_names)

    if(length(ll_obj$line) == 0) {

      msg <- stringr::str_glue("Failed to translate '{meas_obj}' in the model block")
      stop(msg, call. = FALSE)
    }

    meas_lines[[i]] <- ll_obj$line
    delta_counter   <- ll_obj$delta_counter
  }

  paste(meas_lines, collapse = "\n")

}

construct_likelihood_line <- function(meas_obj, delta_counter, lvl_names) {

  decomposed_meas <- decompose_meas(meas_obj)
  lhs             <- decomposed_meas$lhs
  dist_obj        <- get_dist_obj(decomposed_meas$rhs)
  translation_obj <- translate_lik_rhs(dist_obj, delta_counter, lvl_names)
  new_rhs         <- translation_obj$rhs

  list(line          = as.character(stringr::str_glue("  {lhs} ~ {new_rhs};")),
       delta_counter = translation_obj$delta_counter)
}

translate_lik_rhs <- function(dist_obj, delta_counter, lvl_names) {

  return_obj <- list(rhs           = NULL,
                     delta_counter = NULL)

  stock_txt  <- dist_obj[[2]]

  translation_obj <- translate_stock_text(stock_txt, delta_counter, lvl_names)

  return_obj$delta_counter <- translation_obj$delta_counter
  stock_txt                <- translation_obj$stock_txt

  if(dist_obj$dist_name == "normal") {

    new_mu          <- stock_txt
    return_obj$rhs  <- stringr::str_glue("normal({new_mu}, {dist_obj$sigma})")
    return(return_obj)
  }

  if(dist_obj$dist_name == "neg_binomial_2") {

    new_mu         <- stock_txt
    return_obj$rhs <- stringr::str_glue("neg_binomial_2({new_mu}, {dist_obj$phi})")
    return(return_obj)
  }

  if(dist_obj$dist_name == "poisson") {

    new_lambda      <- stock_txt
    return_obj$rhs  <- stringr::str_glue("poisson({new_lambda})")
    return(return_obj)
  }

  if(dist_obj$dist_name == "lognormal") {

    new_mu         <- stock_txt
    return_obj$rhs <- stringr::str_glue("lognormal({new_mu}, {dist_obj$sigma})")
    return(return_obj)
  }

  msg <- paste0("translate_lik_rhs() does not support the ",
               dist_obj$dist_name, " distribution.")

  stop(msg, call. = FALSE)
}
