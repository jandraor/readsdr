arrange_variables <- function(var_list) {

  var_names <- sapply(var_list, function(varElem) varElem$name)

  n_equations <- length(var_names)
  states <- rep(0, n_equations) # Flag that indicates whether the var is defined
  names(states) <- var_names
  equations <- sapply(var_list, function(var_obj) var_obj$equation)

  aux_equations    <- equations
  sorted_variables <- vector(mode = "list", length = n_equations)
  current_pos      <- 1

  while (length(aux_equations) > 0) {
    equation <- aux_equations[1]
    pos_equation <- which(equation == equations)
    other_equations <- aux_equations[-1]
    lhs     <- var_list[[pos_equation]]$name
    rh_vars <- extract_variables(equation)

    undefined_vars <- sapply(rh_vars, function(var){
      ifelse(var %in% var_names && states[var] == 0, TRUE, FALSE)
    })

    n_und_var <- sum(undefined_vars)

    if(n_und_var == 0) {
      sorted_variables[current_pos] <- var_list[pos_equation]
      states[lhs] <- 1
      aux_equations <- aux_equations[-1]
      current_pos <- current_pos + 1
    }

    if(n_und_var > 0){
      aux_equations <- c(other_equations, equation)
    }
  }

  sorted_variables
}
