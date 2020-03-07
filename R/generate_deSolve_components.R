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

  vars_in_rs  <- paste(var_names, collapse = ",\n")

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

generate_model_func <- function (variables, stocks, constants) {
  variables        <- arrange_variables(variables)
  var_equations    <- construct_vars_text(variables)
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
