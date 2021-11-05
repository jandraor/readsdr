
#' Construct inputs for performing structural analysis via the impact method
#'
#' @param desc_list Element 'description' from the list returned by \code{read_xmile()}
#'
#' @return A list of three elements. The first element, \code{flows}, is a data
#'   frame that lists all the stock-flow links in the model. Further, this data
#'   frame describes the equation that governs the link and whether the link is
#'   an inflow (+) or an outflow (-). The second element, \code{pathways}, is a
#'   data frame that lists all the pathways among stocks. The third element,
#'   \code{velocities}, is a data frame in which each row corresponds to a
#'   stock. Each row consists of two columns (name  & equation).
#' @export
#'
#' @examples
#'   filepath  <- system.file("models/", "SIR.stmx", package = "readsdr")
#'   mdl       <- read_xmile(filepath)
#'   desc_list <- mdl$description
#'   sd_impact_inputs(desc_list)
sd_impact_inputs <- function(desc_list) {

  flows_df      <- flow_equations(desc_list)
  pathways_df   <- pathways(flows_df)
  velocities_df <- velocity_equations(desc_list)

  list(flows      = flows_df,
       pathways   = pathways_df,
       velocities = velocities_df)
}

# Construct velocity equations in terms of stocks and constants
velocity_equations <- function(desc_list) {

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

flow_equations <- function(desc_list) {

  levels      <- desc_list$levels
  lvl_names   <- purrr::map_chr(levels, "name")

  consts      <- desc_list$constants
  const_names <- purrr::map_chr(consts, "name")

  elem_names  <- c(lvl_names, const_names)

  var_obj     <- desc_list$variables

  purrr::map_df(desc_list$levels, function(lvl_obj) {

    lvl_name  <- lvl_obj$name
    stock_col <- data.frame(stock = lvl_name)
    cbind(stock_col, decompose_net_flow(lvl_obj$equation))
  }) -> flow_df

  flow_list <- purrr::transpose(flow_df)

  sapply(flow_list, function(flow_obj) {
    disentangle_equation(flow_obj$stock, flow_obj$flow, elem_names, var_obj)
  }) -> eqs

  flow_df$equation <- eqs

  flow_df
}

decompose_net_flow <- function(flow_eq) {

  val <- stringr::str_starts(flow_eq, "-", negate = TRUE)

  if(val) flow_eq <- paste0("+", flow_eq)

  flows <- extract_variables("", flow_eq)
  signs <- stringr::str_extract_all(flow_eq, "\\+|\\-")[[1]]

  data.frame(flow = flows, sign = signs)
}

pathways <- function(flows_df) {

  lvl_names  <- unique(flows_df$stock)
  flows_list <- purrr::transpose(flows_df)

  purrr::map_df(flows_list, function(flow_obj) {

    to        <- flow_obj$stock
    through   <- flow_obj$flow
    flow_vars <- extract_variables(to, flow_obj$equation)
    from      <- flow_vars[flow_vars %in% lvl_names]

    data.frame(from = from, to = to, through = through)
  })
}
