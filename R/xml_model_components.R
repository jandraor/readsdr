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
  stocks_list <- unlist(stocks_list, recursive = FALSE)

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

  dimensions <- xml2::xml_find_all(stock_xml, ".//d1:dimensions")
  n_dims     <- length(dimensions)

  is_arrayed <- ifelse(n_dims > 0, TRUE, FALSE)

  if(is_arrayed) {
    elements_xml <- xml2::xml_find_all(stock_xml, ".//d1:element")
    subs         <- xml2::xml_attr(elements_xml, "subscript")
  }

  inflow_vctr <- stock_xml %>% xml2::xml_find_all(".//d1:inflow") %>%
    xml2::xml_text() %>% sanitise_elem_name()

  n_inflow    <- length(inflow_vctr)

  if(n_inflow > 0L) {

    inflow_list <- list(inflow_vctr)

    if(is_arrayed) {
      inflow_list <- lapply(subs, function(s) paste(inflow_vctr, s, sep = "_"))
    }

    text_inflow  <- sapply(inflow_list, function(inflows) paste(inflows,
                                                                collapse = "+"))
  }

  outflow_vctr <- stock_xml %>% xml2::xml_find_all(".//d1:outflow") %>%
    xml2::xml_text() %>% sanitise_elem_name()

  n_outflow    <- length(outflow_vctr)

  if(n_outflow > 0L) {

    outflow_list <- list(outflow_vctr)

    if(is_arrayed) {
      outflow_list <- lapply(subs, function(s) paste(outflow_vctr, s, sep = "_"))
    }

    text_outflow  <- sapply(outflow_list, function(outflows) {
      paste0("-", outflows) %>% paste(collapse = "")
    })

  }

  if(n_inflow > 0L && n_outflow > 0L) {
    netflows <- paste0(text_inflow, text_outflow)
  }

  if(n_inflow > 0L && n_outflow == 0L) {
    netflows <- text_inflow
  }

  if(n_inflow == 0L && n_outflow > 0L) {
    netflows <- text_outflow
  }

  if(n_inflow == 0L && n_outflow == 0L) {
    netflows <- "0"
  }

  stock_names <- stock_xml %>% xml2::xml_attr("name") %>%
    sanitise_elem_name() %>% check_elem_name()

  if(is_arrayed) {
    stock_names <- paste(stock_names, subs, sep = "_")
  }


  initValues <- stock_xml %>% xml2::xml_find_all(".//d1:eqn") %>%
    xml2::xml_text() %>% sanitise_init_value()

  summary_stocks <- data.frame(name      = stock_names,
                               equation  = netflows,
                               initValue = initValues)

  unname(as_row_list(summary_stocks))
}
