create_dims_obj <- function(raw_xml) {
  dim_xml       <- xml2::xml_find_first(raw_xml, ".//d1:dimensions")
  dim_elems_xml <- xml2::xml_find_all(dim_xml, ".//d1:dim")

  dim_names <- sapply(dim_elems_xml, function(dim_tag) {
    xml2::xml_attr(dim_tag, "name")
  })

  dims_list        <- lapply(dim_elems_xml, extract_dim_elems)
  names(dims_list) <- dim_names

  dims_list
}

extract_dim_elems <- function(dim_tag) {
  flag_size <- xml2::xml_has_attr(dim_tag, "size")

  if(flag_size) {
    dim_size <- xml2::xml_attr(dim_tag, "size")
    dim_size <- as.numeric(dim_size)
    return(1:dim_size)
  }

  elems_xml <- xml2::xml_find_all(dim_tag, ".//d1:elem")

  sapply(elems_xml, function(elem_tag) xml2::xml_attr(elem_tag, "name"))
}


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
                                   builtin_stocks = NULL, dims_obj = NULL) {

  if(length(stocks_xml) == 0L & is.null(builtin_stocks)) {
    stop("A model must contain stocks", call. = FALSE)
  }

  # This makes consts & auxs have the same properties
  constants <- lapply(constants, function(const) {
    list(name = const$name, equation = const$value)
  })

  stocks_list <- lapply(stocks_xml, extract_stock_info, dims_obj = dims_obj)
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

extract_stock_info <- function(stock_xml, dims_obj) {

  dim_xml     <- xml2::xml_find_all(stock_xml, ".//d1:dimensions")
  dimensions  <- xml2::xml_find_all(stock_xml, ".//d1:dim")
  n_dims      <- length(dimensions)

  is_arrayed <- ifelse(n_dims > 0, TRUE, FALSE)

  if(is_arrayed) {
    dim_name     <- xml2::xml_attr(dimensions[[1]], "name")

    cld_xml      <- xml2::xml_children(stock_xml)
    child_names  <- xml2::xml_name(cld_xml)

    if("eqn" %in% child_names) subs <- dims_obj[[dim_name]]

    if(!("eqn" %in% child_names)) {
      elements_xml <- xml2::xml_find_all(stock_xml, ".//d1:element")
      subs         <- xml2::xml_attr(elements_xml, "subscript")
    }
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
