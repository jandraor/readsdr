# Stan's data block for ODE models
stan_data <- function(meas_mdl, unk_inits, LFO_CV, data_params, data_inits,
                      n_difeq = NULL) {

  external_params <- c(data_params, data_inits)

  decl <- "  int<lower = 1> n_obs;"

  data_decl <- lapply(meas_mdl, construct_data_decl, LFO_CV) %>%
    paste(collapse = "\n")

  final_decl <- ifelse(LFO_CV,
                       "  array[n_obs + 1] real ts;",
                       "  array[n_obs] real ts;")

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

  paste("data {", body_block, "}", sep = "\n")
}

construct_data_decl <- function(meas_obj, LFO_CV) {

  decomposed_meas <- decompose_meas(meas_obj)
  lhs             <- decomposed_meas$lhs
  rhs             <- decomposed_meas$rhs
  type            <- get_dist_type(rhs)

  meas_size <- determine_meas_size(rhs)

  # meas_ipt_ln = measurement input line

  if(meas_size == 1) meas_ipt_ln <- stringr::str_glue("  {type} {lhs};")

  if(meas_size == Inf) meas_ipt_ln <- stringr::str_glue("  array[n_obs] {type} {lhs};")

  if(!LFO_CV) return(meas_ipt_ln)

  pred_data_line <- stringr::str_glue("  {type} {lhs}_ahead;")

  paste(meas_ipt_ln, pred_data_line, sep = "\n")
}
