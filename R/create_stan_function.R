#' Create a Stan's ODE function from an XMILE file
#'
#' \code{create_stan_function} returns a string with the code for a Stan's ODE function
#'
#' This function extracts the xml from the file specified via \code{filepath} to
#' generate the code for an equivalent model in Stan.
#'
#' @param func_name A string for naming the ODE function
#' @param pars A character vector that indicates which constants will be
#'   considered as parameters in the ODE function.
#' @param override.consts A list in which each element is a name-value pair that
#' replaces values of constants.
#' @inheritParams read_xmile
#'
#' @return A string with the code containing the model's equations in the
#'  format required by Stan.
#' @export
#'
#' @examples
#' path <- system.file("models", "SIR.stmx", package = "readsdr")
#' create_stan_function(path, "my_model")

create_stan_function <- function (filepath, func_name, pars = NULL,
                                  override.consts = NULL) {

  XMILE_structure  <- extract_structure_from_XMILE(filepath)

  levels           <- XMILE_structure$levels
  variables        <- XMILE_structure$variables
  constants        <- XMILE_structure$constants

  n_stocks      <- length(levels)
  level_names   <- sapply(levels, function(level) level$name)

  function_name_line       <- paste0("  real[] ", func_name, "(real time,")
  diff_eq_declaration_line <- paste0("  real dydt[", n_stocks, "];")

  const_names   <- sapply(constants, function(constant) constant$name)

  purrr::walk(override.consts, function(const_list) {
    pos <- which(const_list$name == const_names)
    constants[[pos]]$value <<- const_list$value
  })

  for(param in pars) {
    pos_param                     <- which(pars == param)
    replacement                   <- paste0("params[", pos_param,"]")
    pos_const                     <- which(param == const_names)

    if(length(pos_const) == 0L) {
      stop(stringr::str_glue("'{param}' not found"), call. = FALSE)
    }

    constants[[pos_const]]$value  <- replacement
  }

  vars_declaration <- sapply(variables, function(variable) {
    paste0("  real ", variable$name,";")
  }) %>% paste(collapse = "\n")

  vars_equations <- sapply(variables, function(variable) {
    equation       <- variable$equation

    raw_elements   <- stringr::str_split(equation, "\\b")[[1]] %>%
      stringi::stri_remove_empty()

    boolean_filter <- stringr::str_detect(raw_elements, "/|\\*|\\+|-|\\(|\\)")
    elements       <- raw_elements[!boolean_filter]
    stocks_found   <- elements[elements %in% level_names]

    counter <- 1

    for(sf in stocks_found) {
      stock_pos     <- which(level_names == sf)
      regex_pattern <- stringr::regex(paste0("\\b", sf,"\\b"))
      equation      <- stringr::str_replace_all(
        equation, regex_pattern, paste0("y", counter, "[", stock_pos, "]"))
      counter <- counter + 1
    }

    equation <- stringr::str_replace_all(equation, "y\\d+", "y")

    consts_found   <- elements[elements %in% const_names]

    for(cf in consts_found) {
      const_pos     <- which(const_names == cf)
      const_value   <- constants[[const_pos]]$value

      const_value   <- ifelse(is.numeric(const_value),
                              as.character(round(const_value, 10)),
                              const_value)

      regex_pattern <- stringr::regex(paste0("\\b", cf,"\\b"))
      equation      <- stringr::str_replace_all(
        equation, regex_pattern, const_value)
    }

    paste0("  ", variable$name, " = ", equation, ";")
  }) %>% paste(collapse = "\n")

  counter       <- 0
  diff_eq       <- sapply(levels, function(level) {
    counter     <<- counter + 1
    paste0("  dydt[", counter, "] = ", level$equation, ";")
  }) %>% paste(collapse = "\n")


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
    "}",
    sep = "\n")
}
