read_xmile <- function(filepath) {

  XMILE_structure  <- extract_structure_from_XMILE(filepath)
  parameters       <- XMILE_structure$parameters
  levels           <- XMILE_structure$levels
  variables        <- XMILE_structure$variables
  constants        <- XMILE_structure$constants

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
