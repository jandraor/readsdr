create_param_obj <- function(paramLine) {
  param_components <- stringr::str_match(paramLine, "(.+)=(.+)")
  lhs <- param_components[, 2]
  rhs <- param_components[, 3]
  list(name = lhs,
       value = rhs)
}


create_level_obj <- function(equation) {
  equation_components <- stringr::str_match(equation, "(.+)=(.+)")
  lhs <- equation_components[, 2]
  rhs <- equation_components[, 3] %>%
    stringr::str_replace_all("INTEG|\\(|\\)", "") %>%
    stringr::str_split(",", simplify = T)
  list(name = lhs,
       equation = rhs[1, 1],
       initValue = as.numeric(rhs[1, 2]))
}

is_constant <- function(equation) {
  equation_components <- stringr::str_match(equation, "(.+)=(.+)")
  rhs <- equation_components[, 3]
  !is.na(suppressWarnings(as.numeric(rhs)))
}

create_const_obj <- function(equation) {
  equation_components <- stringr::str_match(equation, "(.+)=(.+)")
  lhs <- equation_components[, 2]
  rhs <- as.numeric(equation_components[, 3])
  list(name = lhs,
       value = rhs)
}

create_var_obj <- function(equation) {
  equation_components <- stringr::str_match(equation, "(.+)=(.+)")
  lhs <- equation_components[, 2]
  rhs <- equation_components[, 3]
  list(name = lhs,
       equation = rhs)
}


#' Parse Vensim
#'
#' @param file A path to a Vensim file with extension .mdl
#' @importFrom magrittr %>%
#' @return A list...
#'  @export

read_vensim <- function(file) {
  mdlRawText    <- readr::read_file(file)

  pattern   <- stringr::regex("\\{.*?\\}|\\:macro\\:.+?\\:end of macro\\:|\\s*\\\\\\\\\\\\.*$",
                     multiline = T, ignore_case = T, dotall = T)
  mdlFilteredText <- stringr::str_remove_all(mdlRawText, pattern)

  pattern   <- stringr::regex("\\*{56}.*\\*{56}(.*)", dotall = T)

  mdlParamsRaw <- stringr::str_match(mdlFilteredText, pattern)[, 2]

  lines_split  <- stringr::str_split(mdlParamsRaw,"\\|")[[1]]

  lines_containing_params <- lines_split[stringr::str_detect(lines_split, ".+=.+")]

  pattern    <- stringr::regex("(.+=.+)~.*~.*", dotall = T)
  parameters <- stringr::str_match(lines_containing_params, pattern)[, 2] %>%
    stringr::str_replace_all("\n|\t|~| ","") %>% lapply(create_param_obj)

  pattern         <- stringr::regex("(.*)\\*{56}.*\\*{56}", dotall = T)

  mdlEquationsRaw <- stringr::str_match(mdlFilteredText, pattern)[, 2] %>%
    stringr::str_replace_all("\n|\t| ","")

  # Removes the last "|" character to make easier the split
  mdlEquationsRaw <- stringr::str_sub(mdlEquationsRaw, 1,
                                      nchar(mdlEquationsRaw) - 1)

  lines_split     <- stringr::str_split(mdlEquationsRaw, "\\|")[[1]]
  pattern         <- stringr::regex("(.+=.+)~.*~.*", dotall = T)
  mdlEquations    <- stringr::str_match(lines_split, pattern)[, 2]

  levels <- stringr::str_subset(mdlEquations, "INTEG") %>% lapply(create_level_obj)

  vars_and_consts <- mdlEquations[!stringr::str_detect(mdlEquations, "INTEG")]

  are_they_consts <- is_constant(vars_and_consts)

  constants       <- vars_and_consts[are_they_consts] %>%
    lapply(create_const_obj)

  variables <- vars_and_consts[!are_they_consts] %>%
    lapply(create_var_obj)

  ds_model_func <- generate_model_func(variables, levels, constants)
  ds_stocks     <- generate_stocks_vector(levels)
  ds_consts     <- generate_constants_vector(constants)

  nodes_df <- generate_nodes_df(levels, variables)
  edges_df <- generate_edges_df(levels, variables, constants)

  list(
    description = list(
      parameters = parameters,
      levels = levels,
      variables = variables,
      constants = constants),
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
