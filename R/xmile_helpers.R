create_param_obj_xmile <- function(sim_specs) {
  start      <- sim_specs %>% xml2::xml_find_first("//d1:start") %>%
    xml2::xml_double()
  stop       <- sim_specs %>% xml2::xml_find_first("//d1:stop") %>%
    xml2::xml_double()
  dt_html    <- sim_specs %>% xml2::xml_find_first("//d1:dt")
  dt         <- xml2::xml_double(dt_html)

  if(xml2::xml_has_attr(dt_html ,"reciprocal")) {
    if(xml2::xml_attr(dt_html ,"reciprocal") == "true"){
      dt <- 1 / dt
    }
  }

  list(start = start,
       stop = stop,
       dt = dt)
}

create_level_obj_xmile <- function(stocks_xml, variables, constants) {

  # This makes consts & auxs have the same properties
  constants <- lapply(constants, function(const) {
    list(name = const$name, equation = const$value)
  })

  auxs      <- c(variables, constants)


  lapply(stocks_xml, function(stock_xml) {

    initValue <- stock_xml %>% xml2::xml_find_first(".//d1:eqn") %>%
      xml2::xml_text() %>%
      sanitise_elem_name()

    inflow  <- stock_xml %>% xml2::xml_find_first(".//d1:inflow") %>%
      xml2::xml_text() %>%
      sanitise_elem_name()

    outflow <- stock_xml %>% xml2::xml_find_first(".//d1:outflow") %>%
      xml2::xml_text()  %>%
      sanitise_elem_name()

    if(!is.na(inflow) && !is.na(outflow)) {
      netflow <- paste(inflow, outflow, sep = "-")
    }

    if(!is.na(inflow) && is.na(outflow)) {
      netflow <- inflow
    }

    if(is.na(inflow) && !is.na(outflow)) {
      netflow <- paste0("-", outflow)
    }

    if(is.na(inflow) && is.na(outflow)) {
      netflow <- "0"
    }

    stock_name <- stock_xml %>% xml2::xml_attr("name") %>%
      sanitise_elem_name()

    is_numeric <- !is.na(as.numeric(initValue))

    if(is_numeric) {
      initValue <- as.numeric(initValue)
    }

    if(!is_numeric) {
      newInitValue <- compute_init_value(initValue, auxs)
      initValue    <- as.numeric(newInitValue)
    }

    list(name = stock_name,
         equation = netflow,
         initValue = initValue)
  })
}

create_vars_consts_obj_xmile <- function(auxs_xml) {

  #-----------------------------------------------------------------------------
  # Exception for Vensim PRO that adds the variable 'Time'
  all_names <- sapply(auxs_xml, function(node){
    xml2::xml_attr(node, "name")
  })

  pos_Time <- which(all_names == "Time")

  if(length(pos_Time) == 1) auxs_xml <- auxs_xml[-pos_Time]
  #-----------------------------------------------------------------------------
  n_vars_consts <- length(auxs_xml)
  vars          <- list()
  consts        <- list()

  if(n_vars_consts == 0L) {
    return(list(variables = vars, constants = consts))
  }

  counter_v     <- 1
  counter_c     <- 1

  for(i in 1:n_vars_consts){
    aux_xml  <- auxs_xml[[i]]
    equation <- aux_xml %>% xml2::xml_find_first(".//d1:eqn") %>%
      xml2::xml_text() %>% sanitise_aux_equation()
    is_const <- !is.na(suppressWarnings(as.numeric(equation)))

    var_name <- aux_xml %>% xml2::xml_attr("name") %>%
      sanitise_elem_name()

    # if the aux is a variable
    if(!is_const) {
      there_is_graph_fun <- FALSE

      # Vensim
      if(stringr::str_detect(equation, "WITHLOOKUP")) {
        there_is_graph_fun <- TRUE
        translation        <- translate_Vensim_graph_func(equation)
        fun_name           <- paste0("f_", var_name)
        equation           <- paste0(fun_name, "(", translation$input, ")")
        graph_fun          <- list(name = fun_name,
                                   fun = translation$graph_fun)
      }

      # Stella
      graph_fun_xml <- aux_xml %>% xml2::xml_find_first(".//d1:gf")

      # Added the second as precaution
      if(length(graph_fun_xml) > 0 & there_is_graph_fun == FALSE) {
        there_is_graph_fun <- TRUE
        func               <- translate_graph_func(graph_fun_xml)
        fun_name           <- paste0("f_", var_name)
        equation           <- paste0("f_", var_name, "(", equation, ")")
        graph_fun          <- list(name = fun_name,
                                   fun = func)
      }

      variable          <- list(name = var_name,
                                equation = equation)

      if(there_is_graph_fun) {
        variable$graph_fun <- graph_fun
      }

      vars[[counter_v]] <- variable
      counter_v         <- counter_v + 1
    }

    # if the aux is a constant
    if(is_const) {
      const               <- list(name = var_name,
                                  value = as.numeric(equation))
      consts[[counter_c]] <- const
      counter_c           <- counter_c + 1
    }
  }

  list(variables = vars,
       constants = consts)
}

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

compute_init_value <- function(equation, auxs) {
  vars_in_equation <- extract_variables(equation)
  newEquation        <- equation

  for(var_in_equation in vars_in_equation) {
    auxs_names  <- sapply(auxs, function(aux) aux$name)
    pos_aux     <- which(auxs_names == var_in_equation)
    replacement <- paste0("(", auxs[[pos_aux]]$equation, ")")
    pattern     <- paste0("\\b", var_in_equation, "\\b")
    newEquation <- gsub(pattern, replacement, newEquation)
  }

  contains_characters <- stringr::str_detect(newEquation, "[A-Za-z]")

  if(contains_characters) {
    initValue <- compute_init_value(newEquation, auxs)
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

