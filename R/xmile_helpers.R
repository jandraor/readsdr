extract_structure_from_XMILE <- function(filepath) {
  raw_xml    <- xml2::read_xml(filepath)

  sim_specs  <- xml2::xml_find_all(raw_xml, ".//d1:sim_specs")
  parameters <- create_param_obj_xmile(sim_specs)

  variables_xml  <- raw_xml %>% xml2::xml_find_first(".//d1:variables")

  auxs_xml        <- variables_xml %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")
  vars_and_consts <- create_vars_consts_obj_xmile(auxs_xml)
  variables       <- vars_and_consts$variables %>% arrange_variables()
  constants       <- vars_and_consts$constants

  stocks_xml     <- variables_xml %>% xml2::xml_find_all(".//d1:stock")
  levels         <- create_level_obj_xmile(stocks_xml, variables, constants)

  list(parameters = parameters,
       levels = levels,
       variables = variables,
       constants = constants)
}

compute_init_value <- function(var_name, equation, auxs) {
  vars_in_equation <- extract_variables(var_name, equation)
  newEquation      <- equation

  for(var_in_equation in vars_in_equation) {
    auxs_names  <- sapply(auxs, function(aux) aux$name)
    pos_aux     <- which(auxs_names == var_in_equation)
    replacement <- paste0("(", auxs[[pos_aux]]$equation, ")")
    pattern     <- paste0("\\b", var_in_equation, "\\b")
    newEquation <- gsub(pattern, replacement, newEquation)
  }

  contains_characters <- stringr::str_detect(newEquation, "[A-Za-z]")

  if(contains_characters) {
    initValue <- compute_init_value(var_name, newEquation, auxs)
    return(initValue)
  }

  if(!contains_characters) {
    newEquation <- parse(text = newEquation)
    initValue   <- eval(newEquation)
  }

  initValue
}

sanitise_elem_name <- function(elem_name) {
  elem_name %>%
    stringr::str_replace_all("\\{.*?\\}", "") %>%  # removes commentaries
    stringr::str_replace_all("\n|\t|~","") %>%
  stringr::str_replace_all(" |\\\\n", "_")
}

sanitise_aux_equation <- function(equation) {
  equation %>% stringr::str_replace_all("\n|\t|~| ","") %>%
    stringr::str_replace_all("\\{.*?\\}", "") %>%  # removes commentaries
    stringr::str_replace_all("\\bMIN\\b", "min") %>%
    stringr::str_replace_all("\\bMAX\\b", "max")
}

