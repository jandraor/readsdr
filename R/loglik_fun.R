#' Generate a log-likelihood function for an SD model
#'
#' @param pars_df A data frame of three columns (\code{name}, \code{type},
#' \code{par_trans}). Each row in this data frame corresponds to an unknown
#' parameter. The column \code{name} must correspond to exactly the parameter's
#' name in the SD model. The entries in column \code{type} can be either
#' \code{"constant"} or \code{"stock"}. Lastly, column \code{par_trans} supports
#' the following transformations: \code{"log"} for strictly positive values
#' & \code{"logit"} for unknowns in the interval (0,1).
#' @param deSolve_components A list
#' @param sim_controls A list
#' @param meas_data_mdl A list of lists. Each second-level list corresponds to
#'  a measurement model. Here is an example: \cr
#'  \code{list(stock_name = "Infected", stock_fit_type = "net_change",
#'             dist = list(name     = "dpois", sim_data = "lambda"),
#'             data = 1:10)}
#' @param extra_stocks An optional list of lists. Every sublist has two
#'   elements. The first element corresponds to \code{name}, which is a string
#'   that indicates the name of the stock whose initial value depends upon
#'   another stock. The second element corresponds to \code{init}, whose value
#'   (string) is an equation that expresses a constraint.
#' @param extra_constraints An optional list
#' @param neg_log A boolean that indicates whether the log-likelihood function
#'        returns a positive or negative value. If \code{TRUE}, the function
#'        returns a positive value (for minimisation optimisers). If
#'        \code{FALSE}, the function returns the original log-likelihood.
#'
#' @return A list of two elements. The first element \code{fun} corresponds to
#' the log likelihood function. The second element \code{par_names} indicates
#' the order in which the unknowns are returned.
#' @export
#'
#' @examples
#'   pars_df            <- data.frame(name = "beta_var", type = "constant", par_trans = "log")
#'   filepath           <- system.file("models/", "SIR.stmx", package = "readsdr")
#'   mdl                <- read_xmile(filepath)
#'   deSolve_components <- mdl$deSolve_components
#'   sim_controls       <- list(start = 0, stop = 10, step = 0.25, integ_method ="rk4")
#'   mm1 <- list(stock_name = "Infected", stock_fit_type = "net_change",
#'               dist       = list(name     = "dpois", sim_data = "lambda", dist_offset = "1e-5"),
#'               data       = 1:10)
#'   sd_loglik_fun(pars_df, deSolve_components, sim_controls, list(mm1))

sd_loglik_fun <- function(filepath, unknown_pars, meas_data_mdl, neg_log = FALSE,
                          start_time = NULL, stop_time = NULL, timestep = NULL,
                          integ_method = "euler") {

  unk_constants <- unknown_pars
  n_consts      <- length(unk_constants)

  pars_names <- get_names(unknown_pars, "par_name")

  mdl_structure <- extract_structure_from_XMILE(filepath, pars_names)
  ds_inputs     <- get_deSolve_elems(mdl_structure)

  meas_mdl  <- lapply(meas_data_mdl, function(meas_obj) meas_obj$formula)
  meas_pars <- lapply(meas_mdl, extract_extra_params) %>% remove_NULL()

  if(length(meas_pars) > 0) unknown_pars <- c(unknown_pars, meas_pars)

  pars_trans_text  <- transform_pars(unknown_pars)
  pars_assign_text <- assign_pars_text(unk_constants)
  model_exe_text   <- get_model_run_text(integ_method)

  meas_model_text  <- get_meas_model_text(meas_data_mdl, n_consts,
                                          unknown_pars, neg_log)

  body_list <- list(pars_trans_text,
                    pars_assign_text,
                    model_exe_text,
                    meas_model_text)

  body_text <- paste(body_list, collapse = "\n")

  body_func <- paste("{", body_text, "}", sep = "\n")

  model_func <- rlang::new_function(
    args = rlang::exprs(pars = ),
    body = rlang::parse_expr(body_func)
  )

  rlang::fn_env(model_func)$ds_inputs <- ds_inputs

  for(i in length(meas_data_mdl)) {

    meas_model <- meas_data_mdl[[i]]
    data_id <- paste0("data_", i)
    rlang::fn_env(model_func)[[data_id]] <- meas_model$measurements

  }

  list(fun = model_func, par_names = NULL)
}


transform_pars <- function(unknown_pars) {

  pars_lines <- lapply(seq_along(unknown_pars), function(i) {

    par_obj <- unknown_pars[[i]]
    lhs     <- stringr::str_glue("  pars[[{i}]]")
    rhs     <- stringr::str_glue("pars[[{i}]]")

    obj_names <- names(par_obj)

    has_a_lb  <- ifelse("min" %in% obj_names, TRUE, FALSE) # Lower bound(lb)
    has_an_ub <- ifelse("max" %in% obj_names, TRUE, FALSE) # Upper bound(ub)

    if(has_a_lb & !has_an_ub) rhs <- paste0("exp", "(", rhs, ")")

    if(has_a_lb & has_an_ub)  rhs <- paste0("expit", "(", rhs, ")")

    par_line        <- paste(lhs, rhs, sep = " <- ")
  })

  paste(pars_lines, collapse = "\n")
}

assign_pars_text <- function(unk_constants) {

  assign_lines <- lapply(seq_along(unk_constants), function(i) {

    unk_obj <- unk_constants[[i]]
    lhs <- stringr::str_glue('  ds_inputs$consts["{unk_obj$par_name}"]')
    rhs <- stringr::str_glue('pars[[{i}]]')

    assign_line       <- paste(lhs, rhs, sep = " <- ")
  })

  paste(assign_lines, collapse = "\n")
}

get_model_run_text <- function(im) {

  stocks_assing_lines <- paste(
    "  readsdr_env <- list2env(as.list(ds_inputs$consts))",
    "  ds_inputs$stocks <- purrr::map_dbl(ds_inputs$stocks, function(x) {",
    "    eval(parse(text = x), envir = readsdr_env)",
    "  })", sep = "\n")

  sd_simulate_line <- stringr::str_glue('  o    <- sd_simulate(ds_inputs, integ_method = "{im}")')

  paste(stocks_assing_lines, sd_simulate_line, "  o_df <- data.frame(o)",
        sep = "\n")
}

get_meas_model_text <- function(meas_data_mdl, n_consts, unknown_pars, neg_log) {

  pos  <- n_consts
  n_mm <- length(meas_data_mdl) # number of measurements

  text_output <- vector(mode = "character", length = n_mm)

  for(i in seq_len(n_mm)) {

    meas_data_obj <- meas_data_mdl[[i]]

    decomposed_meas <- decompose_meas(meas_data_obj$formula)
    dist_obj        <- get_dist_obj(decomposed_meas$rhs)

    nf_pattern <- get_pattern_regex("net_flow")
    is_nf      <- stringr::str_detect(dist_obj[[2]], nf_pattern) # Is net flow?

    if(is_nf) stock_name <- stringr::str_match(dist_obj[[2]],
                                               "net_flow\\((.+?)\\)")[[2]]

    if(!is_nf) stock_name <- dist_obj[[2]]


    if(!is_nf) {
      lhs <- paste0("sim_data_", i)
      rhs <- "dplyr::filter(o_df, time - trunc(time) == 0)"
      sim_data_line <- stringr::str_glue("{lhs} <- {rhs}")
      sim_data_text <- stringr::str_glue("sim_data_{i}[, '{stock_name}']")
    }

    if(is_nf) {
      sim_data_line <- stringr::str_glue('sim_data_{i} <- sd_net_change(o_df, "{stock_name}")')
      sim_data_text <- stringr::str_glue("sim_data_{i}[, 'value']")
    }

    sim_data_text <- paste(sim_data_text, "1e-05", sep = " + ")

    distr          <- Stan_to_R(dist_obj$dist_name, "d")
    dist_args      <- get_dist_args(dist_obj$dist_name, language = "R")
    dist_args_Stan <- get_dist_args(dist_obj$dist_name)
    par_sim_data <- dist_args[[1]]

    par_assignment <- stringr::str_glue("{par_sim_data} = {sim_data_text}")

    if(length(dist_args) == 2) {

      dist_par_known <- is_string_numeric(dist_obj[[3]])

      if(!dist_par_known) {

        unknown_obj <- dist_obj[[3]]
        pos         <- pos + 1
        par_name    <- dist_args[[2]]
        rhs         <- stringr::str_glue("pars[[{pos}]]")

        if("par_trans" %in% names(unknown_pars[[pos]])) {

          rhs <- execute_trans(rhs, unknown_pars[[pos]]$par_trans, "text")
        }

        unknown_par <- stringr::str_glue("{par_name} = {rhs}")
        par_assignment <- paste(par_assignment, unknown_par, sep = ", ")
      }

      if(dist_par_known) {
        par_name  <- names(meas_mdl$dist$known_par)
        known_par <- stringr::str_glue("{par_name} = {meas_mdl$dist$known_par}")
        par_assignment <- paste(par_assignment, known_par, sep = ", ")
      }
    }



    log_lik_line <- stringr::str_glue(
      "loglik_{i}   <- sum({distr}(data_{i}, {par_assignment}, log = TRUE))")



    text_output[[i]] <- paste(sim_data_line, log_lik_line, sep = "\n")
  }

  loglik_defs <- paste(text_output, collapse = "\n")

  lhs <- "loglik"
  rhs <- paste0("loglik_", seq_len(n_mm)) %>% paste(collapse = " + ")

  loglik_sum_line <- stringr::str_glue("{lhs}     <- {rhs}")

  return_line <- "loglik"

  if(neg_log) return_line <- paste0("-", return_line)

  paste(loglik_defs, loglik_sum_line, return_line, sep = "\n")
}

get_constraint_text <- function(extra_constraints, pars_df) {
  pars_df2  <- dplyr::mutate(pars_df,
                             pos = dplyr::row_number())

  sapply(extra_constraints, function(constraint) {

    condition <- identify_pars(constraint, pars_df2)
    stringr::str_glue("if({condition}) return(-Inf)")
  }) -> constraints_vector

  paste(constraints_vector, collapse = "\n")
}

identify_pars <- function(equation, pars_df) {

  n_rows <- nrow(pars_df)

  for(i in seq_len(n_rows)) {
    pattern     <- paste0("\\b", pars_df[[i, "name"]], "\\b")
    idx         <- pars_df[[i, "pos"]]
    replacement <- stringr::str_glue("pars[[{idx}]]")
    equation    <- stringr::str_replace_all(equation, pattern, replacement)
  }

  equation
}
