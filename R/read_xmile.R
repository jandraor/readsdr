create_param_obj_xmile <- function(sim_specs) {
  start      <- sim_specs %>% xml_find_first("//d1:start") %>%
    xml2::xml_double()
  stop       <- sim_specs %>% xml_find_first("//d1:stop") %>%
    xml2::xml_double()
  dt         <- sim_specs %>% xml_find_first("//d1:dt") %>%
    xml2::xml_double()

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


    list(name = stock_xml %>% xml2::xml_attr("name"),
         equation = netflow,
         initValue = initValue)
  })
}

read_xmile <- function(filepath) {

  raw_xml    <- xml2::read_xml(filepath)
  sim_specs  <- xml2::xml_find_all(raw_xml, ".//d1:sim_specs")
  parameters <- create_param_obj_xmile(sim_specs)
  stocks_xml <- raw_xml %>% xml2::xml_find_all(".//d1:stock")
  levels     <- create_level_obj_xmile(stocks_xml)


  list(
    description = list(
      parameters = parameters,
      levels     = levels,
      variables  = "variables",
      constants  = "constants"),
    deSolve_components = list(
      stocks = "ds_stocks",
      consts = "ds_consts",
      func   = "ds_model_func"),
    graph_dfs = list(
      nodes = "nodes_df",
      edges = "edges_df"
    )
  )
}
