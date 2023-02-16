extract_vars_in_stocks <- function(stocks_xml, vars_and_consts, inits_vector) {

  vars      <- vars_and_consts$variables
  consts    <- vars_and_consts$constants
  b_stocks  <- vars_and_consts$builtin_stocks
  new_elems <- lapply(stocks_xml, extract_delay_vars, consts, inits_vector) %>%
    remove_NULL()

  new_vars <- purrr::map(new_elems, "variable_list") %>%
    unlist(recursive = FALSE)

  if(length(new_vars) > 0) vars <- c(vars, new_vars)

  new_stocks <- purrr::map(new_elems, "stock_list") %>%
    unlist(recursive = FALSE)

  if(length(new_stocks) > 0) b_stocks <- c(b_stocks, new_stocks)

  vars_and_consts$variables      <- vars
  vars_and_consts$builtin_stocks <- b_stocks

  vars_and_consts
}

extract_delay_vars <- function(stock_xml, consts, inits_vector) {

  delay_vars <- list(variable_list = NULL,
                     stock_list    = NULL)

  eq <- xml2::xml_find_all(stock_xml, ".//d1:eqn") %>% xml2::xml_text()

  if(grepl("\\bDELAY_FIXED\\b", eq)) {

    var_name <- stock_xml %>%  xml2::xml_attr("name") %>%
      sanitise_elem_name() %>%  check_elem_name()

    eq <- sanitise_aux_equation(eq, "Vensim")

    var_list <- list(list(name     = var_name,
                          equation = translate_delay(eq, "Vensim")))

    delay_vars$variable_list <- var_list

    return(delay_vars)
  }

  stl_delayn <- stringr::str_detect(eq, "\\bDELAY_N\\b")

  if(stl_delayn) {

    var_name <- stock_xml %>%  xml2::xml_attr("name") %>%
      sanitise_elem_name() %>%  check_elem_name()

    DELAYN_translation <- translate_DELAYN(var_name, eq, "Vensim", consts,
                                           inits_vector)

    return(DELAYN_translation)
  }

  NULL
}
