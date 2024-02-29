# Stan's data block for ODE models
stan_data <- function(meas_mdl, unk_inits, data_params, data_inits,
                      n_difeq = NULL, forecast) {

  external_params <- c(data_params, data_inits)

  decl <- "  int<lower = 1> n_obs;"

  data_decl <- lapply(meas_mdl, construct_data_decl) |>
    paste(collapse = "\n")

  final_decl <- "  array[n_obs] real ts;"

  body_block <- paste(decl, data_decl, final_decl, sep = "\n")

  if(!unk_inits) {
    body_block <- paste(body_block,
                        stringr::str_glue("  vector[{n_difeq}] x0;"),
                        sep = "\n")
  }

  if(!is.null(external_params)) {

    data_params_lines <- stringr::str_glue("  real {external_params};") |>
      paste(collapse = "\n")

    body_block <- paste(body_block, data_params_lines, sep = "\n")
  }

  if(forecast) body_block <- paste(body_block, "  int<lower = 1> n_fcst;",
                                   sep = "\n")

  paste("data {", body_block, "}", sep = "\n")
}

construct_data_decl <- function(meas_obj) {

  decomposed_meas <- decompose_meas(meas_obj)
  lhs             <- decomposed_meas$lhs
  rhs             <- decomposed_meas$rhs
  type            <- get_dist_type(rhs)

  meas_size <- determine_meas_size(rhs)

  # meas_ipt_ln = measurement input line

  if(meas_size == 1) meas_ipt_ln <- stringr::str_glue("  {type} {lhs};")

  if(meas_size == Inf) meas_ipt_ln <- stringr::str_glue("  array[n_obs] {type} {lhs};")

  meas_ipt_ln
}
