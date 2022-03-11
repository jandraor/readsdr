extract_vars_in_stocks <- function(stocks_xml, vars_and_consts) {

  vars     <- vars_and_consts$variables
  new_vars <- lapply(stocks_xml, extract_delay_vars) %>% remove_NULL()

  if(length(new_vars) > 0) vars <- c(vars, new_vars)

  vars_and_consts$variables <- vars

  vars_and_consts
}

extract_delay_vars <- function(stock_xml) {

  eq <- xml2::xml_find_all(stock_xml, ".//d1:eqn") %>% xml2::xml_text()

  if(grepl("\\bDELAY_FIXED\\b", eq)) {

    var_name <- stock_xml %>%  xml2::xml_attr("name") %>%
      sanitise_elem_name() %>%  check_elem_name()

    eq <- sanitise_aux_equation(eq, "Vensim")

    list(name     = var_name,
         equation = translate_delay(eq, "Vensim"))

  } else {
    return(NULL)
  }
}
