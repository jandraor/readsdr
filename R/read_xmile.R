create_param_obj_xmile <- function(sim_specs) {
  start      <- sim_specs %>% xml_find_first("//d1:start") %>%
    xml2::xml_double()
  stop       <- sim_specs %>% xml_find_first("//d1:stop") %>%
    xml2::xml_double()
  dt_html    <- sim_specs %>% xml_find_first("//d1:dt")
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

create_level_obj_xmile <- function(stocks_xml) {

  lapply(stocks_xml, function(stock_xml) {

    initValue <- stock_xml %>% xml2::xml_find_first(".//d1:eqn") %>%
      xml2::xml_text() %>%
      stringr::str_replace_all("\n|\t|~| ","")

    inflow  <- stock_xml %>% xml2::xml_find_first(".//d1:inflow") %>%
      xml2::xml_text() %>%
      stringr::str_replace_all("\n|\t|~| ","")

    outflow <- stock_xml %>% xml2::xml_find_first(".//d1:outflow") %>%
      xml2::xml_text() %>%
      stringr::str_replace_all("\n|\t|~| ","")

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
      stringr::str_replace(" ", "_")

    list(name = stock_name,
         equation = netflow,
         initValue = as.numeric(initValue))
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
      stringr::str_replace(" ", "_")

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


read_xmile <- function(filepath) {

  raw_xml    <- xml2::read_xml(filepath)

  sim_specs  <- xml2::xml_find_all(raw_xml, ".//d1:sim_specs")
  parameters <- create_param_obj_xmile(sim_specs)

  variables_xml  <- raw_xml %>% xml2::xml_find_first(".//d1:variables")
  stocks_xml     <- variables_xml %>% xml2::xml_find_all(".//d1:stock")
  levels         <- create_level_obj_xmile(stocks_xml)

  auxs_xml        <- variables_xml %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")
  vars_and_consts <- create_vars_consts_obj_xmile(auxs_xml)
  variables       <- vars_and_consts$variables
  constants       <- vars_and_consts$constants

  ds_model_func <- generate_model_func(variables, levels, constants)
  ds_stocks     <- generate_stocks_vector(levels)
  ds_consts     <- generate_constants_vector(constants)

  nodes_df <- generate_nodes_df(levels, variables)
  edges_df <- generate_edges_df(levels, variables, constants)

  list(
    description = list(
      parameters = parameters,
      levels     = levels,
      variables  = variables,
      constants  = constants),
    deSolve_components = list(
      stocks = ds_stocks,
      consts = ds_consts,
      func   = ds_model_func),
    graph_dfs = list(
      nodes = nodes_df,
      edges = edges_df
    )
  )
}
