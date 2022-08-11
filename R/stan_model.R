stan_model <- function(prior, meas_mdl) {

  prior_lines <- sapply(prior, construct_prior_line) %>%
    paste(collapse = "\n")

  likelihood_lines <- get_likelihood_lines(meas_mdl)

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

get_likelihood_lines <- function(meas_mdl) {

  delta_counter <- 1
  n_meas        <- length(meas_mdl)

  meas_lines <- vector(mode = "character", length = n_meas)

  for(i in seq_len(n_meas)) {
    meas_lines[[i]] <- construct_likelihood_line(meas_mdl[[i]], delta_counter)
  }

  paste(meas_lines, collapse = "\n")

}

construct_likelihood_line <- function(meas_obj, delta_counter) {

  pattern     <- "net_flow\\(.+?\\)"
  replacement  <- stringr::str_glue("delta_x_{delta_counter}")

  new_meas_obj <-stringr::str_replace(meas_obj, pattern, replacement)

  stringr::str_glue("  {new_meas_obj};")
}
