
#' Create Stan ODE function
#'
#' @param func_name A string for naming the ODE function
#' @param pars A character vector that indicates which constants will be
#'   considered as parameters in the ODE function
#' @param extra_funs A vector of strings. Each string corresponds to a
#' user-defined function.
#' @inheritParams read_xmile
#'
#' @return A string with the code containing a function with the model's
#'   equations in the format required by cmdstan 2.24+.
#' @export
#'
#' @examples
#' path <- system.file("models", "SIR.stmx", package = "readsdr")
#' stan_ode_function(path, "my_model")
stan_ode_function <- function(filepath, func_name, pars = NULL,
                              const_list = NULL,
                              extra_funs = NULL) {

  XMILE_structure  <- extract_structure_from_XMILE(filepath)

  if(!is.null(const_list)) {
    XMILE_structure <- override_consts(XMILE_structure, const_list)
  }

  levels           <- XMILE_structure$levels
  variables        <- XMILE_structure$variables
  constants        <- XMILE_structure$constants

  n_stocks      <- length(levels)
  level_names   <- get_names(levels)

  fun_declaration     <- get_fun_declaration(func_name)
  diff_eq_declaration <- get_diffeq_declaration(n_stocks)
  auxs_declaration    <- get_auxs_declaration(variables)

  constants           <- set_unknowns(pars, constants)

  auxs_equations      <- get_equations(variables, constants, level_names)

  diff_eq <- get_diffeq(levels)

  stan_fun <- paste("functions {",
                    fun_declaration,
                    diff_eq_declaration,
                    auxs_declaration,
                    auxs_equations,
                    diff_eq,
                    "    return dydt;",
                    "  }", sep = "\n")

  if(!is.null(extra_funs)) {
    af_text  <- paste(extra_funs, sep = "\n")
    stan_fun <- paste(stan_fun, af_text, sep = "\n")
  }

  paste(stan_fun, "}", sep = "\n")
}

get_fun_declaration <- function(func_name) {
  paste0("  vector ", func_name, "(real time, vector y, real[] params) {")
}

get_diffeq_declaration <- function(n_stocks) {
  stringr::str_glue("    vector[{n_stocks}] dydt;")
}

get_auxs_declaration <- function(var_list) {

  declaration_vector <- sapply(var_list, function(var_obj) {
    stringr::str_glue("    real {var_obj$name};")
  })

  paste(declaration_vector, collapse = "\n")
}

# This fun determines which pars will be considered as unknowns
set_unknowns <- function(unknowns, const_list) {

  const_names   <- get_names(const_list)

  for(param in unknowns) {
    pos_param                     <- which(param == unknowns)
    replacement                   <- paste0("params[", pos_param,"]")
    pos_const                     <- which(param == const_names)

    if(length(pos_const) == 0L) {
      stop(stringr::str_glue("'{param}' not found"), call. = FALSE)
    }

    const_list[[pos_const]]$value  <- replacement
  }

  const_list
}

get_equations <- function(var_list, const_list, level_names) {

  const_names   <- get_names(const_list)

  equations_list <- sapply(var_list, function(var_obj) {

    lhs      <- var_obj$name
    equation <- var_obj$equation
    elements <- extract_variables(lhs, equation)

    stocks_found   <- elements[elements %in% level_names]
    counter <- 1

    for(sf in stocks_found) {
      stock_pos     <- which(level_names == sf)
      regex_pattern <- stringr::regex(paste0("\\b", sf,"\\b"))
      equation      <- stringr::str_replace_all(
        equation, regex_pattern, paste0("y", counter, "[", stock_pos, "]"))
      counter       <- counter + 1
    }

    equation <- stringr::str_replace_all(equation, "y\\d+", "y")

    consts_found   <- elements[elements %in% const_names]

    for(cf in consts_found) {
      const_pos     <- which(const_names == cf)
      const_value   <- const_list[[const_pos]]$value

      const_value   <- ifelse(is.numeric(const_value),
                              as.character(round(const_value, 10)),
                              const_value)

      regex_pattern <- stringr::regex(paste0("\\b", cf,"\\b"))
      equation      <- stringr::str_replace_all(
        equation, regex_pattern, const_value)
    }

    paste0("    ", lhs, " = ", equation, ";")
  })

  paste(equations_list, collapse = "\n")
}

get_diffeq <- function(stock_list) {

  diff_eq_list <- vector(mode = "character", length = length(stock_list))

  for(i in seq_along(stock_list)) {
    rhs               <- stock_list[[i]]$equation
    diff_eq_list[[i]] <- paste0("    dydt[", i, "] = ", rhs, ";")
  }

  paste(diff_eq_list, collapse = "\n")
}

