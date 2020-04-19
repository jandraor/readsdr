get_igraph_inputs <- function(model_structure) {

  levels           <- model_structure$levels
  variables        <- model_structure$variables
  constants        <- model_structure$constants

  nodes_df <- generate_nodes_df(levels, variables, constants)
  edges_df <- generate_edges_df(levels, variables, constants)

  list(
    nodes = nodes_df,
    edges = edges_df
  )
}

generate_nodes_df <- function(stocks, variables, constants) {
  const_names <- sapply(constants, function(const) const$name)

  stocks_df <- purrr::map_df(stocks, function(stock) {

    data.frame(name = stock$name,
               type = "stock",
               equation = stock$equation,
               stringsAsFactors = F)
  })

  variables_df <- purrr::map_df(variables, function(variable) {
    equation        <- variable$equation
    extracted_vars  <- extract_variables(variable$name, equation)

    if("time" %in% extracted_vars) stop("A variable depends on time",
                                         call. = FALSE)

    detected_consts <- extracted_vars[extracted_vars %in% const_names]
    n_det_consts    <- length(detected_consts)

    if(n_det_consts > 0) {

      for(det_const in detected_consts){

        regex_pattern <- stringr::regex(paste0("\\b", det_const,"\\b"))
        pos_const     <- which(det_const == const_names)
        const_value   <- constants[[pos_const]]$value

        const_value   <- ifelse(is.numeric(const_value),
                                as.character(round(const_value, 10)),
                                const_value)

        equation      <- stringr::str_replace_all(
          equation, regex_pattern, const_value)
      }
    }

    data.frame(name = variable$name,
               type = "variable",
               equation = equation,
               stringsAsFactors = F)
  })

  dplyr::bind_rows(stocks_df, variables_df)
}

generate_edges_df <- function(stocks, variables, constants) {

  stocks_edges <- purrr::map_df(stocks, construct_stock_edge)

  const_names     <- sapply(constants, function(constant) constant$name)

  variables_edges <- purrr::map_df(variables, const_names = const_names,
                                   construct_var_edge)

  dplyr::bind_rows(stocks_edges, variables_edges)
}

construct_var_edge <- function(variable, const_names) {
  rhs            <- extract_variables(variable$name, variable$equation)
  rhs            <- rhs[!rhs %in% const_names ] %>% unique()

  if(length(rhs) == 0L) return(NULL)

  data.frame(from = rhs, to = rep(variable$name, length(rhs)),
             type = "info_link", stringsAsFactors = F)
}

construct_stock_edge <- function(stock_obj) {
  rhs <- extract_variables(stock_obj$name, stock_obj$equation)

  if(length(rhs) == 0L) return(NULL)

  data.frame(from = rhs,
             to = rep(stock_obj$name, length(rhs)),
             type = "flow",
             stringsAsFactors = F)
}
