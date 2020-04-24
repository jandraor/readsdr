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

create_level_obj_xmile <- function(stocks_xml, variables, constants) {

  if(length(stocks_xml) == 0L) stop("A model must contain stocks",
                                    call. = FALSE)

  # This makes consts & auxs have the same properties
  constants <- lapply(constants, function(const) {
    list(name = const$name, equation = const$value)
  })

  stocks_list <- lapply(stocks_xml, extract_stock_info)

  stock_auxs <- lapply(stocks_list, function(stock) {
    list(name = stock$name, equation = stock$initValue)
  })

  auxs        <- c(variables, constants, stock_auxs)

  n_stocks    <- length(stocks_xml)

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

create_vars_consts_obj_xmile <- function(auxs_xml, vendor) {

  #-----------------------------------------------------------------------------
  # Exception for Vensim PRO that adds the variable 'Time'
  all_names <- sapply(auxs_xml, function(node){
    xml2::xml_attr(node, "name")
  })

  pos_Time <- which(all_names == "Time")

  if(length(pos_Time) == 1) auxs_xml <- auxs_xml[-pos_Time]
  #-----------------------------------------------------------------------------
  n_vars_consts <- length(auxs_xml)
  vars          <- list()
  consts        <- list()

  if(n_vars_consts == 0L) {
    return(list(variables = vars, constants = consts))
  }

  counter_v     <- 1
  counter_c     <- 1

  for(i in 1:n_vars_consts){
    aux_xml  <- auxs_xml[[i]]
    equation <- aux_xml %>% xml2::xml_find_first(".//d1:eqn") %>%
      xml2::xml_text() %>% sanitise_aux_equation(vendor)
    is_const <- !is.na(suppressWarnings(as.numeric(equation)))

    var_name <- aux_xml %>% xml2::xml_attr("name") %>%
      sanitise_elem_name() %>% check_elem_name()

    # if the aux is a variable
    if(!is_const) {
      there_is_graph_fun <- FALSE

      # Vensim
      if(stringr::str_detect(equation, "WITHLOOKUP")) {
        there_is_graph_fun <- TRUE
        translation        <- translate_Vensim_graph_func(equation)
        fun_name           <- paste0("f_", var_name)
        equation           <- paste0(fun_name, "(", translation$input, ")")
        graph_fun          <- list(name = fun_name,
                                   fun = translation$graph_fun)
      }

      # Stella
      graph_fun_xml <- aux_xml %>% xml2::xml_find_first(".//d1:gf")

      # Added the second as precaution
      if(length(graph_fun_xml) > 0 & there_is_graph_fun == FALSE) {
        there_is_graph_fun <- TRUE
        func               <- translate_graph_func(graph_fun_xml)
        fun_name           <- paste0("f_", var_name)
        equation           <- paste0("f_", var_name, "(", equation, ")")
        graph_fun          <- list(name = fun_name,
                                   fun = func)
      }

      variable          <- list(name = var_name,
                                equation = equation)

      if(there_is_graph_fun) {
        variable$graph_fun <- graph_fun
      }

      vars[[counter_v]] <- variable
      counter_v         <- counter_v + 1
    }

    # if the aux is a constant
    if(is_const) {
      const               <- list(name = var_name,
                                  value = as.numeric(equation))
      consts[[counter_c]] <- const
      counter_c           <- counter_c + 1
    }
  }

  list(variables = vars,
       constants = consts)
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
