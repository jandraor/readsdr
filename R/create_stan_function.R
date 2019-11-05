
create_stan_function <- function (filepath, func_name) {
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
  variables        <- vars_and_consts$variables
  constants        <- vars_and_consts$constants
  const_names      <- sapply(constants, function(constant) constant$name)

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
      const_value   <- as.character(round(constants[[const_pos]]$value, 10))
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
