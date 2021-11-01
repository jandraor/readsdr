#' Construct velocity equations in terms of stocks and constants
#'
#' @param desc_list Element 'description' from the list returned by \code{read_xmile()}
#'
#' @return A data frame of two columns.
#' @export
#'
#' @examples
#'  filepath  <- system.file("models/", "SIR.stmx", package = "readsdr")
#'  mdl       <- read_xmile(filepath)
#'  desc_list <- mdl$description
#'  sd_velocities(desc_list)
sd_velocities <- function(desc_list) {

  levels      <- desc_list$levels
  stock_names <- purrr::map_chr(levels, "name")

  consts      <- desc_list$constants
  const_names <- purrr::map_chr(consts, "name")

  var_obj     <- desc_list$variables

  elem_names  <- c(stock_names, const_names)

  equations   <- purrr::map_chr(levels, construct_velocity_equation,
                                elem_names, var_obj)


  data.frame(stock = stock_names, equation = equations)
}

construct_velocity_equation <- function(lvl_obj, elem_names, var_obj) {

  lvl_name  <- lvl_obj$name
  equation  <- lvl_obj$equation

  disentangle_equation(lvl_name, equation, elem_names, var_obj)
}

disentangle_equation <- function(lvl_name, equation, elem_names, var_obj) {

  var_names  <- purrr::map_chr(var_obj, "name")
  elems_rhs  <- extract_variables(lvl_name, equation)
  validation <- sum(!(elems_rhs %in% elem_names))

  if(validation == 0) return(equation)

  if(validation > 0) {

    auxs <- elems_rhs[!(elems_rhs %in% elem_names)]

    for(i in seq_along(auxs)) {

      aux           <- auxs[i]
      pos_aux       <- which(var_names == aux)
      aux_eq        <- var_obj[[pos_aux]]$equation
      aux_eq        <- stringr::str_glue("({aux_eq})")
      regex_pattern <- stringr::regex(paste0("\\b", aux,"\\b"))
      equation      <- stringr::str_replace_all(equation, regex_pattern, aux_eq)
    }

    disentangle_equation(lvl_name, equation, elem_names, var_obj)

  }
}
