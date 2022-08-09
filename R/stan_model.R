stan_model <- function(prior, meas_mdl) {

  prior_lines <- sapply(prior, construct_prior_line) %>%
    paste(collapse = "\n")

  block_body <- prior_lines

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

