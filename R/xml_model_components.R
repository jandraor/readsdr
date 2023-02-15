create_dims_obj <- function(raw_xml) {

  dim_xml       <- xml2::xml_find_first(raw_xml, ".//d1:dimensions")
  dim_elems_xml <- xml2::xml_find_all(dim_xml, ".//d1:dim")

  dim_names <- sapply(dim_elems_xml, function(dim_tag) {
    xml2::xml_attr(dim_tag, "name")
  })

  dims_list        <- lapply(dim_elems_xml, extract_dim_elems)
  names(dims_list) <- dim_names

  variables_xml   <- xml2::xml_find_first(raw_xml, ".//d1:variables")

  model_elems <- xml2::xml_find_all(variables_xml,
                                   ".//d1:flow|.//d1:aux|.//d1:stock")

  dictionary <- lapply(model_elems, function(elem_obj) {

    elem_name <- xml2::xml_attr(elem_obj, "name") %>% sanitise_elem_name()

    dim_xml   <- xml2::xml_find_all(elem_obj, ".//d1:dimensions")
    dim_tags  <- xml2::xml_find_all(dim_xml, ".//d1:dim")
    n_dims    <- length(dim_tags)

    if(n_dims == 0) return(NULL)

    dim_names <- sapply(dim_tags, function(elem_tag) {
      xml2::xml_attr(elem_tag, "name")
    })

    dict_obj        <- list(dim_names)
    names(dict_obj) <- elem_name
    dict_obj
  }) %>% remove_NULL() %>% unlist(recursive = FALSE)


  list(global_dims = dims_list,
       dictionary  = dictionary)
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
    if(xml2::xml_attr(dt_html, "reciprocal") == "true"){
      dt <- 1 / dt
    }
  }

  list(start = start,
       stop = stop,
       dt = dt)
}

create_level_obj_xmile <- function(stocks_xml, variables, constants,
                                   builtin_stocks = NULL, dims_obj,
                                   time_aux, vendor, fixed_inits = NULL) {

  if(length(stocks_xml) == 0L & is.null(builtin_stocks)) {
    stop("SD models must contain stocks", call. = FALSE)
  }

  # This function makes consts & auxs to have the same properties
  constants <- lapply(constants, function(const) {
    list(name = const$name, equation = const$value)
  })

  stocks_list <- lapply(stocks_xml, extract_stock_info, dims_obj = dims_obj,
                        vendor = vendor)
  stocks_list <- remove_NULL(stocks_list)
  stocks_list <- unlist(stocks_list, recursive = FALSE)

  if(!is.null(builtin_stocks)) {
    stocks_list <- c(builtin_stocks, stocks_list)
  }

  stock_auxs <- lapply(stocks_list, function(stock) {
    list(name = stock$name, equation = stock$initValue)
  })

  auxs        <- c(variables, constants, stock_auxs, list(time_aux))
  stocks_list <- lapply(stocks_list, get_init_value, auxs, fixed_inits)

  stocks_list
}

extract_stock_info <- function(stock_xml, dims_obj, vendor) {

  # Only God knows why Ventana would treat the FIXED DELAY as a stock
  eq <- xml2::xml_find_all(stock_xml, ".//d1:eqn")
  eq <- xml2::xml_text(eq)
  if(any(grepl("DELAY_FIXED|DELAY_N", eq))) return (NULL)
  #-----------------------------------------------------------------------------

  dim_xml     <- xml2::xml_find_all(stock_xml, ".//d1:dimensions")
  dimensions  <- xml2::xml_find_all(stock_xml, ".//d1:dim")
  n_dims      <- length(dimensions)

  is_arrayed <- ifelse(n_dims > 0, TRUE, FALSE)

  if(is_arrayed) {

    global_dims <- dims_obj$global_dims

    dim_tags  <- xml2::xml_find_all(dim_xml, ".//d1:dim")

    dim_names <- sapply(dim_tags, function(elem_tag) {
      xml2::xml_attr(elem_tag, "name")
    })

    dims_list        <- lapply(dim_names, function(dim_name) global_dims[[dim_name]])
    names(dims_list) <- dim_names
    elems            <- combine_dims(dims_list)
  }

  inflow_vctr <- stock_xml %>% xml2::xml_find_all(".//d1:inflow") %>%
    xml2::xml_text() %>% sanitise_elem_name()

  n_inflow    <- length(inflow_vctr)

  if(n_inflow > 0L) {

    inflow_list <- list(inflow_vctr)

    if(is_arrayed) {
      inflow_list <- lapply(elems, function(s) paste(inflow_vctr, s, sep = "_"))
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
      outflow_list <- lapply(elems, function(s) paste(outflow_vctr, s, sep = "_"))
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
    stock_names <- paste(stock_names, elems, sep = "_")
  }

  #-----------------------------------------------------------------------------

  cld_xml      <- xml2::xml_children(stock_xml)
  child_names  <- xml2::xml_name(cld_xml)



  approach <- ifelse(is_arrayed & "eqn" %in% child_names,
                     "approach_1", "approach_2")

  # Apply all from Stella
  if(approach == "approach_1") {

    eqn <- stock_xml %>% xml2::xml_find_all(".//d1:eqn") %>%
      xml2::xml_text() %>% sanitise_init_value(vendor, is_arrayed)

    aux_obj <- list(name     = stock_names,
                    equation = eqn)

    array_obj  <- array_equations(aux_obj, dims_obj, dim_names, vendor)
    initValues <- array_obj$equations
  }


  if(approach == "approach_2") {

    initValues <- stock_xml %>% xml2::xml_find_all(".//d1:eqn") %>%
     xml2::xml_text() %>% sanitise_init_value(vendor, is_arrayed)
  }




  n_init <- length(initValues)

  if(is_arrayed & n_init == 1L) {

    is_const <- !is.na(suppressWarnings(as.numeric(initValues)))

    if(!is_const) {

      if(vendor == "Vensim"){
        initValues <- devectorise_equation(initValues, dims_list)
      }
    }
  }


  #-----------------------------------------------------------------------------

  summary_stocks <- data.frame(name      = stock_names,
                               equation  = netflows,
                               initValue = initValues)

  unname(as_row_list(summary_stocks))
}

get_init_value <- function(stock_obj, auxs, fixed_inits) {

  initValue  <- stock_obj$initValue
  stock_name <- stock_obj$name

  is_numeric <- suppressWarnings(!is.na(as.numeric(initValue)))

  if(is_numeric) stock_obj$initValue <- as.numeric(initValue)

  if(!is_numeric) {

    newInitValue <- compute_init_value(stock_name, initValue, auxs, fixed_inits)

    if(is.null(fixed_inits)) newInitValue <- as.numeric(newInitValue)

    stock_obj$initValue <- newInitValue
  }

  stock_obj

}
