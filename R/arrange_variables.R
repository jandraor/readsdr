#' Arrange variables
#'
#' \code{arrange_variables} returns a list of auxiliary variables sorted in
#'    computational order
#'
#' This function iterates over each element until the variables are ordered in
#' computational order. This is how should be used:
#'
#' unordered_vars <- list(list(name = "b", equation = "a + 1"),
#'                        list(name = "a", equation = "alpha"))
#' ordered_vars   <- arrange_variables(unordered_vars)
#'
#' @param var_list A list of lists. Each second-level list corresponds to a
#' variable in the model and must be a name-equation pair. Given that variables
#' depend on other elements in the model, equations cannot consist of a single
#' scalar.
#'
#' @return A list consisting of the elements in \code{var_list} but arranged in
#' computational order.
#'
#' @noRd

arrange_variables <- function(var_list) {

  if (length(var_list) == 0L) return (var_list)

  var_names <- sapply(var_list, function(varElem) varElem$name)

  n_equations <- length(var_names)
  states <- rep(0, n_equations) # Flag that indicates whether the var is defined
  names(states) <- var_names
  equations <- sapply(var_list, function(var_obj) var_obj$equation)

  equations_df <- data.frame(stringsAsFactors = FALSE, equation = equations) %>%
    dplyr::group_by(equation) %>%
    dplyr::mutate("ocurrence" = dplyr::row_number()) %>%
    dplyr::ungroup()

  equations_df$id <- paste(equations_df$equation, equations_df$ocurrence,
                           sep = "_")

  equations_df$ocurrence <- NULL

  aux_ids          <- equations_df$id
  sorted_variables <- vector(mode = "list", length = n_equations)
  current_pos      <- 1

  while (length(aux_ids) > 0) {

    id           <- aux_ids[1]
    pos_equation <- which(id == equations_df$id)
    equation     <- var_list[[pos_equation]]$equation
    other_ids    <- aux_ids[-1]
    lhs          <- var_list[[pos_equation]]$name
    rh_vars      <- extract_variables(lhs, equation)

    if(length(rh_vars) == 0L) {

      msg <- paste0("There are no variables in the RHS of `", lhs, "`. RHS: ",
                    equation)

      stop(msg, call. = FALSE)
    }

    undefined_vars <- sapply(rh_vars, function(var){
      ifelse(var %in% var_names && states[var] == 0, TRUE, FALSE)
    })

    n_und_var <- sum(undefined_vars)

    if(n_und_var == 0) {
      sorted_variables[current_pos] <- var_list[pos_equation]
      states[lhs] <- 1
      aux_ids <- aux_ids[-1]
      current_pos <- current_pos + 1
    }

    if(n_und_var > 0) aux_ids <- c(other_ids, id)
  }

  sorted_variables
}
