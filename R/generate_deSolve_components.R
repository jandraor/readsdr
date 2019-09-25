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

  stock_text <- paste(formattedStocks, collapse = ", ")

  var_names <- sapply(variables, function(var) {
    paste0(var$name, ' = ', var$name)
  })

  var_text  <- paste(var_names, collapse = ",\n")

  const_names <- sapply(constants, function(const) {
    paste0(const$name, ' = ', const$name)
  })

  const_text  <- paste(const_names, collapse = ",\n")

  paste0('return (list(c(', stock_text, '),', var_text, ",\n", const_text, '))')
}

generate_model_func <- function (variables, stocks, constants) {
  var_equations    <- construct_vars_text(variables)
  var_equations    <- arrange_in_comp_order(var_equations, variables)
  net_flows        <- construct_nf_text(stocks)
  return_statement <- construct_return_statement(stocks, variables, constants)

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
