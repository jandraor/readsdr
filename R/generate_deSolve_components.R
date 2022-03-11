get_deSolve_elems <- function(mdl_structure) {
  levels      <- mdl_structure$levels
  variables   <- mdl_structure$variables
  constants   <- mdl_structure$constants

  ds_graphs_funs <- generate_gf_list(variables)
  there_are_gf   <- ifelse(length(ds_graphs_funs) > 0, TRUE, FALSE)

  delayed_vars <- identify_delayed_vars(variables)

  ds_model_func <- generate_model_func(variables, levels, constants,
                                       there_are_gf, delayed_vars)
  ds_stocks     <- generate_stocks_vector(levels)
  ds_consts     <- generate_constants_vector(constants)

  deSolve_components <- list(
    stocks     = ds_stocks,
    consts     = ds_consts,
    func       = ds_model_func,
    sim_params = mdl_structure$parameters)

  if(there_are_gf) deSolve_components$graph_funs <- ds_graphs_funs

  if(length(delayed_vars) > 0) deSolve_components$delayed_vars <- delayed_vars

  deSolve_components
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

#netflows
construct_nf_text <- function(stocks) {
  equations <- sapply(stocks, function(stock) {
    paste0('d_', stock$name, '_dt', ' <- ', stock$equation)
  })
  nf_text <- paste(equations, collapse = "\n")
}

construct_return_statement <- function(stocks, variables, constants) {
  formattedStocks <- sapply(stocks, function(stock){
    paste0('d_', stock$name, '_dt')
  })

  stock_text   <- paste(formattedStocks, collapse = ", ")
  stocks_in_rs <- paste0("c(", stock_text, ")" ) # stocks in return statement

  var_names <- sapply(variables, function(var) {
    paste0(var$name, ' = ', var$name)
  })


  if(length(var_names) > 0) {
    vars_in_rs  <- paste(var_names, collapse = ",\n")
  } else {
    vars_in_rs  <- NULL
  }

  const_names <- sapply(constants, function(const) {
    paste0(const$name, ' = ', const$name)
  })

  if(length(const_names) > 0) {
    consts_in_rs  <- paste(const_names, collapse = ",\n")
  } else {
    consts_in_rs <- NULL
  }

  body_elems <- c(stocks_in_rs,  vars_in_rs, consts_in_rs)
  body_elems <- body_elems[!is.null(body_elems)]

  body_return <- paste(body_elems, collapse = ",\n")

  paste0('return (list(', body_return,'))')
}

generate_model_func <- function (variables, stocks, constants, graph_fun,
                                 delayed_vars) {
  variables        <- arrange_variables(variables)
  var_equations    <- construct_vars_text(variables)
  net_flows        <- construct_nf_text(stocks)
  return_statement <- construct_return_statement(stocks, variables, constants)

  without_graph_fun <- 'with(as.list(c(stocks, auxs)), {'
  with_graph_fun    <- 'with(as.list(c(stocks, auxs, graph_funs)), {'

  with_statement    <- ifelse(graph_fun, with_graph_fun, without_graph_fun)

  func_body <- paste(
    with_statement,
    var_equations,
    net_flows, sep = "\n")

  if(!is.null(delayed_vars)) {

    sapply(delayed_vars, function(d_var) {
      stringr::str_glue(".memory[as.character(time), c('{d_var}')] <<- {d_var}")
    }) -> memory_vector

    memory_lines <- paste(memory_vector, collapse = "\n")

    func_body <- paste(func_body, memory_lines, sep = "\n")
  }

  func_body <- paste(func_body, return_statement, '})', sep = "\n")

  func_args <- NULL

  if(!graph_fun) {
    func_args <- rlang::exprs(time = , stocks =, auxs = )
  }

  if(graph_fun) {
    func_args <- rlang::exprs(time = , stocks =, auxs = , graph_funs = )
  }

  model_func <- rlang::new_function(
    args = func_args,
    body = rlang::parse_expr(func_body)
  )
}

# Generate a list of graphical functions
generate_gf_list <- function(variable_list) {

  filtered_list <- lapply(variable_list, function(var_obj) {
    var_obj$graph_fun
  }) %>% remove_NULL()

  graph_fun_names       <- purrr::map_chr(filtered_list, "name")
  graph_fun_list        <- purrr::map(filtered_list, "fun")
  names(graph_fun_list) <- graph_fun_names

  graph_fun_list
}
