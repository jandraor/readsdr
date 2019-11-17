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

  nodes_df <- generate_nodes_df(levels, variables, constants)
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
