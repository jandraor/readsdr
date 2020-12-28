create_vars_consts_obj_xmile <- function(auxs_xml, vendor, dims_obj = NULL) {

  #-----------------------------------------------------------------------------
  # Exception for Vensim PRO that adds the variable 'Time'
  all_names <- sapply(auxs_xml, function(node){
    xml2::xml_attr(node, "name")
  })

  pos_Time <- which(all_names == "Time")

  if(length(pos_Time) == 1) auxs_xml <- auxs_xml[-pos_Time]
  #-----------------------------------------------------------------------------
  n_vars_consts <- length(auxs_xml)
  vars           <- list()
  consts         <- list()
  builtin_stocks <- list()

  if(n_vars_consts == 0L) {
    return(list(variables = vars, constants = consts))
  }

  elem_list      <- lapply(auxs_xml, xml_to_elem_list, vendor = vendor,
                           dims_obj = dims_obj)

  vars           <- lapply(elem_list, function(obj) obj$vars)
  vars           <- remove_NULL(vars)
  vars           <- unlist(vars, recursive = FALSE)

  consts         <- lapply(elem_list, function(obj) obj$consts)
  consts         <- remove_NULL(consts)
  if(length(consts) > 0) consts <- unlist(consts, recursive = FALSE)


  builtin_stocks <- lapply(elem_list, function(obj) obj$builtin_stocks)
  builtin_stocks <- unlist(builtin_stocks, recursive = FALSE)
  builtin_stocks <- remove_NULL(builtin_stocks)

  vars_consts_obj <- list(variables = vars,
                          constants = consts)

  if(length(builtin_stocks) > 0) {
    vars_consts_obj$builtin_stocks <- builtin_stocks
  }

  vars_consts_obj
}

xml_to_elem_list <- function(aux_xml, vendor, dims_obj) {

  vars           <- list()
  consts         <- list()
  builtin_stocks <- list()

  n_dims <- 0

  if(vendor == "isee") {
    dim_xml     <- xml2::xml_find_all(aux_xml, ".//d1:dimensions")
    dimensions  <- xml2::xml_find_all(aux_xml, ".//d1:dim")
    n_dims     <- length(dimensions)
  }

  is_arrayed <- ifelse(n_dims > 0, TRUE, FALSE)

  equations_xml <- xml2::xml_find_all(aux_xml, ".//d1:eqn")
  raw_equations <- xml2::xml_text(equations_xml)

  equations     <- sapply(raw_equations, sanitise_aux_equation, vendor,
                          USE.NAMES = FALSE)

  are_const     <- !is.na(suppressWarnings(as.numeric(equations)))

  raw_var_names <- xml2::xml_attr(aux_xml, "name")
  var_names     <- sanitise_elem_name(raw_var_names)
  var_names     <- check_elem_name(var_names)

  if(is_arrayed) {
    dim_name     <- xml2::xml_attr(dimensions[[1]], "name")

    cld_xml      <- xml2::xml_children(aux_xml)
    child_names  <- xml2::xml_name(cld_xml)

    if("eqn" %in% child_names) subs <- dims_obj[[dim_name]]

    if(!("eqn" %in% child_names)) {
      elements_xml <- xml2::xml_find_all(aux_xml, ".//d1:element")
      subs         <- xml2::xml_attr(elements_xml, "subscript")
    }

    var_names    <- paste(var_names, subs, sep = "_")
  }

  const_list <- Map(list, var_names[are_const], equations[are_const])
  const_list <- unname(const_list)

  if(length(const_list) > 0) {
    consts <- lapply(const_list, tidy_consts)
  }

  non_const_list <- Map(list, var_names[!are_const], equations[!are_const])
  non_const_list <- unname(non_const_list)

  if(vendor == "isee") {
    output_isee    <- lapply(non_const_list, interpret_equations_isee, aux_xml)
    vars           <- lapply(output_isee, function(obj) obj$vars)
    vars           <- unlist(vars, recursive = FALSE)
    builtin_stocks <- lapply(output_isee, function(obj) obj$builtin_stocks)
    builtin_stocks <- unlist(builtin_stocks, recursive = FALSE)
  }

  if(vendor == "Vensim") {
    output_isee    <- lapply(non_const_list, interpret_equations_vensim)
    vars           <- lapply(output_isee, function(obj) obj$vars)
    vars           <- unlist(vars, recursive = FALSE)
    builtin_stocks <- lapply(output_isee, function(obj) obj$builtin_stocks)
    builtin_stocks <- unlist(builtin_stocks, recursive = FALSE)
  }

  output_list <- list(vars   = vars,
                      consts = consts)

  if(length(builtin_stocks) > 0) {
    output_list$builtin_stocks <- builtin_stocks
  }

  output_list
}

tidy_consts <- function(const_obj) {
  list(name  = const_obj[[1]],
       value = as.numeric(const_obj[[2]]))
}

interpret_equations_isee <- function(non_const_obj, aux_xml) {
  var_name <- non_const_obj[[1]]
  equation <- non_const_obj[[2]]

  stl_smooth1 <- stringr::str_detect(equation, "SMTH1")

  if(stl_smooth1) {

    S1_translation      <- translate_SMOOTH1(var_name, equation, "isee")
    variable            <- S1_translation$variable

    return(list(vars           = list(variable),
                builtin_stocks = list(S1_translation$stock)))
  }

  stl_smooth3 <- stringr::str_detect(equation, "SMTH3")

  if(stl_smooth3) {

    S3_translation  <- translate_SMOOTH3(var_name, equation, "isee")

    return(list(vars           = S3_translation$variable_list,
                builtin_stocks = S3_translation$stock_list))
  }

  stl_smoothN <- stringr::str_detect(equation, "SMTHN")

  if(stl_smoothN) {
    SN_translation  <- translate_SMOOTHN(var_name, equation, "isee")

    return(list(vars           = SN_translation$variable_list,
                builtin_stocks = SN_translation$stock_list))
  }

  graph_fun_xml <- xml2::xml_find_first(aux_xml, ".//d1:gf")

  there_is_graph_fun <- FALSE

  if(length(graph_fun_xml) > 0) {
    there_is_graph_fun <- TRUE
    func               <- translate_graph_func(graph_fun_xml)
    fun_name           <- paste0("f_", var_name)
    equation           <- paste0("f_", var_name, "(", equation, ")")
    graph_fun          <- list(name = fun_name,
                               fun = func)
  }

  var_obj <- list(name     = var_name,
                  equation = equation)

  if(there_is_graph_fun) {
    var_obj$graph_fun <- graph_fun
  }

  list(vars = list(var_obj))
}



interpret_equations_vensim <- function(non_const_obj) {

  vendor <- "Vensim"

  var_name <- non_const_obj[[1]]
  equation <- non_const_obj[[2]]

  vns_smooth <- stringr::str_detect(equation, "\\bSMOOTH\\b")

  if(vns_smooth) {
    S1_translation      <- translate_SMOOTH1(var_name, equation, vendor,
                                             "SMOOTH")
    return(list(vars           = list(S1_translation$variable),
                builtin_stocks = list(S1_translation$stock)))
  }

  vns_smoothi <- stringr::str_detect(equation, "\\bSMOOTHI\\b")

  if(vns_smoothi) {
    S1_translation      <- translate_SMOOTH1(var_name, equation, vendor,
                                             "SMOOTHI")

    return(list(vars           = list(S1_translation$variable),
                builtin_stocks = list(S1_translation$stock)))
  }

  vns_smooth3 <- stringr::str_detect(equation, "\\bSMOOTH3\\b")

  if(vns_smooth3) {

    S3_translation  <- translate_SMOOTH3(var_name, equation, vendor,
                                         "SMOOTH3")

    return(list(vars           = S3_translation$variable_list,
                builtin_stocks = S3_translation$stock_list))
  }

  vns_smooth3i <- stringr::str_detect(equation, "\\bSMOOTH3I\\b")

  if(vns_smooth3i) {

    S3_translation  <- translate_SMOOTH3(var_name, equation, vendor,
                                         "SMOOTH3I")

    return(list(vars           = S3_translation$variable_list,
                builtin_stocks = S3_translation$stock_list))
  }

  #-------------------------------------------------------------------------
  # Graphical functions
  #-------------------------------------------------------------------------
  there_is_graph_fun <- FALSE

  if(stringr::str_detect(equation, "WITHLOOKUP")) {
    there_is_graph_fun <- TRUE
    translation        <- translate_Vensim_graph_func(equation)
    fun_name           <- paste0("f_", var_name)
    equation           <- paste0(fun_name, "(", translation$input, ")")
    graph_fun          <- list(name = fun_name,
                               fun = translation$graph_fun)
  }

  var_obj <- list(name     = var_name,
       equation = equation)

  if(there_is_graph_fun) {
    var_obj$graph_fun <- graph_fun
  }

  list(vars = list(var_obj))
}
