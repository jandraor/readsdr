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

    stock_name <- stock_xml %>% xml2::xml_attr("name") %>%
      sanitise_elem_name()

    is_numeric <- !is.na(as.numeric(initValue))

    if(is_numeric) {
      initValue <- as.numeric(initValue)
    }

    if(!is_numeric) {
      initValue <- compute_init_value(initValue)







      initValue <- as.numeric(newInitValue)

    }

    list(name = stock_name,
         equation = netflow,
         initValue = initValue)
  })
}

create_vars_consts_obj_xmile <- function(auxs_xml) {
  n_vars_consts <- length(auxs_xml)
  vars          <- list()
  consts        <- list()
  counter_v     <- 1
  counter_c     <- 1

  for(i in 1:n_vars_consts){
    aux_xml  <- auxs_xml[[i]]
    equation <- aux_xml %>% xml2::xml_find_first(".//d1:eqn") %>%
      xml2::xml_text() %>% stringr::str_replace_all("\n|\t|~| ","")
    is_const <- !is.na(suppressWarnings(as.numeric(equation)))

    # if the aux is a variable
    var_name <- aux_xml %>% xml2::xml_attr("name") %>%
      sanitise_elem_name()

    if(!is_const) {
      variable          <- list(name = var_name,
                                equation = equation)
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
    newEquation <- gsub(var_in_equation, replacement, newEquation)
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
  elem_name %>% stringr::str_replace_all("\n|\t|~","") %>%
  stringr::str_replace_all(" |\\\\n", "_")
}

