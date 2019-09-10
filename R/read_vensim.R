generate_edges_df <- function(stocks, variables, constants) {
  stocks_edges <- purrr::map_df(stocks, function(stock) {
    rhs <- stringr::str_split(stock$equation, pattern = "\\+|-|\\*|/")[[1]]
    rhs <- rhs[rhs != ""]
    data.frame(from = rhs,
               to = rep(stock$name, length(rhs)),
               type = "flow",
               stringsAsFactors = F)
  })

  const_names <- sapply(constants, function(constant) constant$name)

  variables_edges <- purrr::map_df(variables,
                                   const_names = const_names,
                                   function(variable, const_names) {
                                     rhs <- stringr::str_split(variable$equation,
                                                               pattern = "\\+|-|\\*|/")[[1]]
                                     rhs <- rhs[rhs != ""]
                                     rhs <- rhs[!rhs %in% const_names ]

                              data.frame(from = rhs,
                                         to = rep(variable$name, length(rhs)),
                                         type = "vars_link",
                                         stringsAsFactors = F)
                            })

  dplyr::bind_rows(stocks_edges, variables_edges)
}


generate_nodes_df <- function(stocks, variables) {
  stocks_df <- purrr::map_df(stocks, function(stock) {

    data.frame(name = stock$name,
               type = "stock",
               equation = stock$equation,
               stringsAsFactors = F)
  })

  variables_df <- purrr::map_df(variables, function(variable) {
    data.frame(name = variable$name,
               type = "variable",
               equation = variable$equation,
               stringsAsFactors = F)
  })

  dplyr::bind_rows(stocks_df, variables_df)
}

generate_stocks_vector <- function(stocks) {
  stocks_vector <- sapply(stocks, function(stock){
    stockElement <- stock$initValue
    names(stockElement) <- stock$name
    stockElement
  })
}

generate_constants_vector <- function(constants) {
  const_vector <- sapply(constants, function(constant) {
    constantElement <- constant$value
    names(constantElement) <- constant$name
    constantElement
  })
}

construct_vars_text <- function(variables) {
  equations <- sapply(variables, function(variable) {
    paste0(variable[[1]], " <- ", variable[[2]])
  })
  vars_text <- paste(equations, collapse = "\n")
}

arrange_in_comp_order <- function(vars_equations, var_list) {

  var_names <- sapply(var_list, function(varElem) {
    varElem$name
  })

  n_equations <- length(var_names)
  states <- rep(0, n_equations)
  names(states) <- var_names
  equations <- strsplit(vars_equations, "\n")[[1]] %>%
    stringr::str_replace_all(" ","")

  aux_equations    <- equations
  sorted_equations <- vector(mode = "character", length = n_equations)
  current_pos      <- 1

  while (length(aux_equations) > 0) {
    equation <- aux_equations[1]
    other_equations <- aux_equations[-1]
    param_components <- stringr::str_match(equation, "(.+)<-(.+)")
    lhs <- param_components[, 2]
    rhs <- param_components[, 3]
    rh_vars <- stringr::str_split(rhs,
                                  pattern = "\\+|-|\\*|/")[[1]]

    undefined_vars <- sapply(rh_vars, function(var){
      ifelse(var %in% var_names && states[var] == 0, TRUE, FALSE)
    })

    n_und_var <- sum(undefined_vars)

    if(n_und_var == 0) {
      sorted_equations[current_pos] <- equation
      states[lhs] <- 1
      aux_equations <- aux_equations[-1]
      current_pos <- current_pos + 1
    }

    if(n_und_var > 0){
      aux_equations <- c(other_equations, equation)
    }
  }

  sorted_equations <- paste(sorted_equations, collapse = "\n")
}

construct_nf_text <- function(stocks) {
  equations <- sapply(stocks, function(stock) {
    paste0('d_', stock$name, '_dt', ' <- ', stock$equation)
  })
  nf_text <- paste(equations, collapse = "\n")
}

construct_return_statement <- function(stocks, variables) {
  formattedStocks <- sapply(stocks, function(stock){
    paste0('d_', stock$name, '_dt')
  })

  stocks_text <- paste(formattedStocks, collapse = ", ")

  var_names <- sapply(variables, function(var) {
    paste0(var$name, ' = ', var$name)
  })

  var_text  <- paste(var_names, collapse = ",\n")
  paste0('return (list(c(', stocks_text, '),', var_text, '))')
}

generate_model_func <- function (variables, stocks) {
  var_equations    <- construct_vars_text(variables)
  var_equations    <- arrange_in_comp_order(var_equations, variables)
  net_flows        <- construct_nf_text(stocks)
  return_statement <- construct_return_statement(stocks, variables)

  func_body <- paste(
    'with(as.list(c(stocks, auxs)), {',
    var_equations,
    net_flows,
    return_statement,
    '})', sep = "\n")

  model_func <- rlang::new_function(
    args = rlang::exprs(time = , stocks =, auxs = ),
    body = rlang::parse_expr(func_body)
  )
}

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

  ds_model_func <- generate_model_func(variables, levels)
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
