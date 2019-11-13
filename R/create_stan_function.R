#' @param filepath A string with the filepath to the xmile file
#' @param func_name A string with the name of the STAN ODE function
#' @param pars A character vector that indicates the constants that will be considered as parameters in the STAN ODE function
#' @param override.consts A list of lists. Second-level lists include constants' name & value to replace.
#' @return a string with the transformation of the file \code{filepath} into a STAN ODE function
create_stan_function <- function (filepath, func_name, pars = NULL, override.consts = NULL) {
  raw_xml       <- xml2::read_xml(filepath)
  variables_xml <- raw_xml %>% xml2::xml_find_first(".//d1:variables")
  stocks_xml    <- variables_xml %>% xml2::xml_find_all(".//d1:stock")
  n_stocks      <- length(stocks_xml)
  levels        <- create_level_obj_xmile(stocks_xml)
  level_names   <- sapply(levels, function(level) level$name)

  function_name_line       <- paste0("  real[] ", func_name, "(real t,")
  diff_eq_declaration_line <- paste0("  real dydt[", n_stocks, "];")

  auxs_xml         <- variables_xml %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")
  vars_and_consts  <- create_vars_consts_obj_xmile(auxs_xml)
  variables        <- vars_and_consts$variables %>% arrange_variables()
  constants        <- vars_and_consts$constants
  const_names      <- sapply(constants, function(constant) constant$name)

  purrr::walk(override.consts, function(const_list) {
    pos <- which(const_list$name == const_names)
    constants[[pos]]$value <<- const_list$value
  })

  for(param in pars) {
    pos_param                     <- which(pars == param)
    replacement                   <- paste0("params[", pos_param,"]")
    pos_const                     <- which(param == const_names)
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