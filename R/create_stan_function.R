#' Create a Stan's ODE function from an XMILE file
#'
#' \code{create_stan_function} returns a string with the code for a Stan's ODE function
#'
#' This function extracts the xml from the file specified via \code{filepath} to
#' generate the code for an equivalent model in Stan.
#'
#' @param override.consts A list in which each element is a name-value pair that
#' replaces values of constants.
#' @param additional_funs A vector of strings. Each string corresponds to a
#' user-defined function.
#' @inheritParams read_xmile
#' @inheritParams stan_ode_function
#'
#' @return A string with the code containing the model's equations in the
#'  format required by Stan.
#' @export
#'
#' @examples
#' path <- system.file("models", "SIR.stmx", package = "readsdr")
#' create_stan_function(path, "my_model")

create_stan_function <- function (filepath, func_name, pars = NULL,
                                  override.consts = NULL,
                                  additional_funs = NULL) {

  XMILE_structure  <- extract_structure_from_XMILE(filepath)

  levels           <- XMILE_structure$levels
  variables        <- XMILE_structure$variables
  constants        <- XMILE_structure$constants

  n_stocks      <- length(levels)
  level_names   <- get_names(levels)

  function_name_line       <- paste0("  real[] ", func_name, "(real time,")
  diff_eq_declaration_line <- paste0("  real dydt[", n_stocks, "];")

  const_names   <- get_names(constants)

  purrr::walk(override.consts, function(const_list) {
    pos <- which(const_list$name == const_names)
    constants[[pos]]$value <<- const_list$value
  })

  constants <- set_unknowns(pars, constants)

  vars_declaration <- get_auxs_declaration(variables)

  vars_equations <- get_equations(variables, constants, level_names)

  counter       <- 0
  diff_eq       <- get_diffeq(levels)

  stan_function <- paste(
    "functions {",
    function_name_line ,
    "              real[] y,",
    "              real[] params,",
    "              real[] x_r,",
    "              int[] x_i) {",
    diff_eq_declaration_line,
    vars_declaration,
    vars_equations,
    diff_eq,
    "  return dydt;",
    "  }",
    sep = "\n")

  if(!is.null(additional_funs)) {
    af_text       <- paste(additional_funs, sep = "\n")
    stan_function <- paste(stan_function, af_text, sep = "\n")
  }

  stan_function    <- paste(stan_function, "}", sep = "\n")
}


