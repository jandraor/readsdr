extract_structure_from_XMILE <- function(filepath) {

  file_extension <- stringr::str_match(filepath, "^.+\\.(.+)$")[[2]]

  if(file_extension == "stmx") {
    raw_xml    <- xml2::read_xml(filepath)
  }

  if(file_extension == "xmile") {
    raw_xml    <- readChar(filepath, file.info(filepath)$size) %>%
      sanitise_xml() %>% xml2::read_xml()
  }

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
  auxs_names       <- sapply(auxs, function(aux) aux$name)

  for(var_in_equation in vars_in_equation) {
    pos_aux     <- which(auxs_names == var_in_equation)
    aux_obj     <- auxs[[pos_aux]]
    rpl_val     <- aux_obj$equation # replacement value


    if(!is.null(aux_obj$graph)){
      input_equation <- stringr::str_match(rpl_val, "f.+\\((.+)\\)")[[2]]
      input          <- compute_init_value("", input_equation, auxs)
      assign(aux_obj$graph_fun$name, aux_obj$graph_fun$fun)
      rpl_val        <- do.call(aux_obj$graph_fun$name, list(input))
    }

    replacement <- paste0("(", rpl_val, ")")
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
  equation %>% translate_ifelse() %>%
    translate_step() %>%
    stringr::str_replace_all("\n|\t|~| ","") %>%
    stringr::str_replace_all("\\{.*?\\}", "") %>%  # removes commentaries
    stringr::str_replace_all("\\bMIN\\b", "min") %>%
    stringr::str_replace_all("\\bMAX\\b", "max") %>%
    stringr::str_replace_all(":AND:", "&") %>%
    stringr::str_replace_all(":OR:", "|") %>%
    stringr::str_replace_all("(?<!<|>)=", "==") %>%
    stringr::str_replace_all("\\bTime\\b", "time") %>%
    eval_constant_expr() # Must go at the end
}

translate_ifelse <- function(equation) {

  ifelse_Stella <- stringr::str_detect(equation, "\\bIF\\b")

  if(ifelse_Stella) {
    pattern      <- stringr::regex("IF\\((.+)\\).*THEN(.*)ELSE(.*)",
                                   dotall = TRUE)
    string_match <- stringr::str_match(equation, pattern)
    condition    <- string_match[[2]]
    if_true      <- string_match[[3]]
    if_false     <- string_match[[4]]
    body_ifelse  <- paste(condition, if_true, if_false, sep = ", ")
    equation     <- paste0("ifelse(", body_ifelse, ")")
    return(equation)
  }

  ifelse_Vensim <- stringr::str_detect(equation, "IF_THEN_ELSE")

  if(ifelse_Vensim) {
    equation <- stringr::str_replace(equation, "IF_THEN_ELSE", "ifelse")
  }

  equation
}

eval_constant_expr <- function(equation) {
  tryCatch(
    error = function(cnd) equation,
    {
      evaluated_expr <- eval(parse(text = equation), envir = baseenv())
      as.character(evaluated_expr)
    }
  )
}

check_elem_name <- function(elem_name) {
  is_valid <- make.names(elem_name) == elem_name

  if(!is_valid) {
    error_message <- paste0(elem_name , " is not a valid name for a variable")
    stop(error_message, call. = FALSE)
  }

  elem_name
}

translate_step <- function(equation) {
  pattern_step  <- stringr::regex("STEP\\((.+?),(.+?)\\)")
  there_is_step <- stringr::str_detect(equation, pattern_step)

  if(there_is_step) {
    new_equation <- stringr::str_replace(equation, pattern_step,
                                     "ifelse(time >=\\2, \\1, 0)")

    new_equation <- translate_step(new_equation)
    equation <- new_equation
  }

  equation
}

