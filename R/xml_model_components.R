create_param_obj_xmile <- function(sim_specs) {
  start      <- sim_specs %>% xml2::xml_find_first("//d1:start") %>%
    xml2::xml_double()
  stop       <- sim_specs %>% xml2::xml_find_first("//d1:stop") %>%
    xml2::xml_double()
  dt_html    <- sim_specs %>% xml2::xml_find_first("//d1:dt")
  dt         <- xml2::xml_double(dt_html)

  if(xml2::xml_has_attr(dt_html ,"reciprocal")) {
    if(xml2::xml_attr(dt_html ,"reciprocal") == "true"){
      dt <- 1 / dt
    }
  }

  list(start = start,
       stop = stop,
       dt = dt)
}

create_level_obj_xmile <- function(stocks_xml, variables, constants,
                                   builtin_stocks = NULL) {

  if(length(stocks_xml) == 0L & is.null(builtin_stocks)) {
    stop("A model must contain stocks", call. = FALSE)
  }

  # This makes consts & auxs have the same properties
  constants <- lapply(constants, function(const) {
    list(name = const$name, equation = const$value)
  })

  stocks_list <- lapply(stocks_xml, extract_stock_info)

  if(!is.null(builtin_stocks)) {
    stocks_list <- c(builtin_stocks, stocks_list)
  }

  stock_auxs <- lapply(stocks_list, function(stock) {
    list(name = stock$name, equation = stock$initValue)
  })

  auxs        <- c(variables, constants, stock_auxs)

  n_stocks    <- length(stocks_list)

  for(i in seq_len(n_stocks)){
    initValue  <- stocks_list[[i]]$initValue
    stock_name <- stocks_list[[i]]$name

    is_numeric <- suppressWarnings(!is.na(as.numeric(initValue)))

    if(is_numeric) {
      stocks_list[[i]]$initValue <- as.numeric(initValue)
    }

    if(!is_numeric) {
      newInitValue               <- compute_init_value(stock_name, initValue,
                                                       auxs)
      stocks_list[[i]]$initValue <- as.numeric(newInitValue)
    }
  }

  stocks_list
}

extract_stock_info <- function(stock_xml) {

  inflow_list <- stock_xml %>% xml2::xml_find_all(".//d1:inflow") %>%
    xml2::xml_text() %>% sanitise_elem_name()

  n_inflow    <- length(inflow_list)

  if(n_inflow > 0L) {
    text_inflow  <- paste(inflow_list, collapse = "+")
  }

  outflow_list <- stock_xml %>% xml2::xml_find_all(".//d1:outflow") %>%
    xml2::xml_text() %>% sanitise_elem_name()

  n_outflow    <- length(outflow_list)

  if(n_outflow > 0L) {
    text_outflow  <- paste0("-", outflow_list) %>% paste(collapse = "")
  }

  if(n_inflow > 0L && n_outflow > 0L) {
    netflow <- paste0(text_inflow, text_outflow)
  }

  if(n_inflow > 0L && n_outflow == 0L) {
    netflow <- text_inflow
  }

  if(n_inflow == 0L && n_outflow > 0L) {
    netflow <- text_outflow
  }

  if(n_inflow == 0L && n_outflow == 0L) {
    netflow <- "0"
  }

  stock_name <- stock_xml %>% xml2::xml_attr("name") %>%
    sanitise_elem_name() %>% check_elem_name()

  initValue <- stock_xml %>% xml2::xml_find_first(".//d1:eqn") %>%
    xml2::xml_text() %>% sanitise_init_value()

  list(name = stock_name, equation = netflow, initValue = initValue)
}
