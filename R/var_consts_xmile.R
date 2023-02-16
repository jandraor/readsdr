
#' Create an object that describe variables, constants and  builtin stocks
#'
#' @param auxs_xml An xml object.
#' @param vendor A string that indicates the model's origin.
#' @param dims_obj A list.
#' @param const_list A list to override constant values from the XMILE file.
#'
#' @return A list of three elements: \code{variables}, \code{constants},
#'   \code{builtin_stocks}. The element \code{variables} corresponds a list of
#'    lists. Each sublist contains two elements: \code{name} & \code{equation}.
#'
#' @noRd
create_vars_consts_obj_xmile <- function(auxs_xml, vendor, dims_obj = NULL,
                                         const_list, inits_vector) {

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

  elem_list <- lapply(auxs_xml, classify_elems, vendor = vendor,
                      dims_obj = dims_obj)

  consts         <- lapply(elem_list, function(obj) obj$consts)
  consts         <- remove_NULL(consts)

  if(length(consts) > 0) {

    consts <- unlist(consts, recursive = FALSE)

    if(!is.null(const_list)) consts <- override_consts(consts, const_list)
  }

  non_consts <- lapply(elem_list, function(obj) {

    if(length(obj$non_consts$elems) == 0) return(NULL)
    obj$non_consts
  })

  non_consts <- remove_NULL(non_consts)

  # interpreted non-consts
  interpreted_nc <- lapply(non_consts, interpret_non_consts, vendor, consts,
                           auxs_xml, inits_vector)

  vars           <- lapply(interpreted_nc, function(obj) obj$vars)
  vars           <- remove_NULL(vars)
  vars           <- unlist(vars, recursive = FALSE)

  builtin_stocks <- lapply(interpreted_nc, function(obj) obj$builtin_stocks)
  builtin_stocks <- unlist(builtin_stocks, recursive = FALSE)
  builtin_stocks <- remove_NULL(builtin_stocks)

  vars_consts_obj <- list(variables = vars,
                          constants = consts)

  if(length(builtin_stocks) > 0) {
    vars_consts_obj$builtin_stocks <- builtin_stocks
  }

  vars_consts_obj
}

classify_elems <- function(aux_xml, vendor, dims_obj) {

  consts         <- list()

  dim_xml     <- xml2::xml_find_all(aux_xml, ".//d1:dimensions")
  dimensions  <- xml2::xml_find_all(aux_xml, ".//d1:dim")
  n_dims      <- length(dimensions)

  raw_var_names <- xml2::xml_attr(aux_xml, "name")
  var_names     <- sanitise_elem_name(raw_var_names)
  var_names     <- check_elem_name(var_names)

  aux_name <- var_names

  is_arrayed <- ifelse(n_dims > 0, TRUE, FALSE)

  if(!is_arrayed) {

    equation_xml <- xml2::xml_find_first(aux_xml, ".//d1:eqn")
    raw_equation <- xml2::xml_text(equation_xml)
    equations    <- sanitise_aux_equation(raw_equation, vendor)
    are_const    <- !is.na(suppressWarnings(as.numeric(equations)))
  }

  if(is_arrayed) {

    dim_tags  <- xml2::xml_find_all(dim_xml, ".//d1:dim")

    dim_names <- sapply(dim_tags, function(elem_tag) {
      xml2::xml_attr(elem_tag, "name")
    })

    cld_xml      <- xml2::xml_children(aux_xml)
    child_names  <- xml2::xml_name(cld_xml)

    # This means that the equation is vectorised
    if("eqn" %in% child_names) {

      equation_xml <- xml2::xml_find_first(aux_xml, ".//d1:eqn")
      raw_eq       <- xml2::xml_text(equation_xml)
      eq           <- sanitise_aux_equation(raw_eq, vendor)

      aux_obj <- list(name     = var_names,
                      equation = eq)

      array_obj <- array_equations(aux_obj, dims_obj, dim_names, vendor)
      equations <- array_obj$equations
      are_const <- array_obj$are_const
      elems     <- array_obj$elems
    }

    if(!("eqn" %in% child_names)) {

      equations_xml <- xml2::xml_find_all(aux_xml, ".//d1:eqn")
      raw_equations <- xml2::xml_text(equations_xml)

      equations     <- sapply(raw_equations, sanitise_aux_equation, vendor,
                              USE.NAMES = FALSE)

      are_const     <- !is.na(suppressWarnings(as.numeric(equations)))
      elements_xml  <- xml2::xml_find_all(aux_xml, ".//d1:element")
      elems         <- xml2::xml_attr(elements_xml, "subscript")
    }

    var_names    <- paste(var_names, elems, sep = "_")
  }

  const_list <- Map(list, var_names[are_const], equations[are_const])
  const_list <- unname(const_list)

  if(length(const_list) > 0) {
    consts <- lapply(const_list, tidy_consts)
  }

  non_const_list      <- Map(list, var_names[!are_const], equations[!are_const])
  non_const_list      <- unname(non_const_list)

  list(consts     = consts,
       non_consts = list(elems    = non_const_list,
                         aux_name = aux_name))
}


tidy_consts <- function(const_obj) {
  list(name  = const_obj[[1]],
       value = as.numeric(const_obj[[2]]))
}

interpret_non_consts <- function(non_const_obj, vendor, consts, auxs_xml,
                                 inits_vector) {

  aux_name       <- non_const_obj$aux_name
  non_const_list <- non_const_obj$elems

  aux_names <- get_auxs_names(auxs_xml)
  idx       <- which(aux_name == aux_names)
  aux_xml   <-  auxs_xml[[idx]]

  if(vendor == "isee") output <- lapply(non_const_list,
                                        interpret_equations_isee, consts,
                                        aux_xml, inits_vector)


  if(vendor == "Vensim") output <- lapply(non_const_list,
                                          interpret_equations_vensim, consts)

  vars           <- lapply(output, function(obj) obj$vars)
  vars           <- unlist(vars, recursive = FALSE)
  builtin_stocks <- lapply(output, function(obj) obj$builtin_stocks)
  builtin_stocks <- unlist(builtin_stocks, recursive = FALSE)

  output_list <- list(vars = vars)

  if(length(builtin_stocks) > 0) {
    output_list$builtin_stocks <- builtin_stocks
  }

  output_list
}


interpret_equations_isee <- function(non_const_obj, consts, aux_xml,
                                     inits_vector) {

  var_name <- non_const_obj[[1]]
  equation <- non_const_obj[[2]]

   # Stella delayN
  stl_delayn <- stringr::str_detect(equation, "\\bDELAYN\\b")

  if(stl_delayn) {

    DELAYN_translation <- translate_DELAYN(var_name, equation, "isee", consts,
                                           inits_vector)

    return(list(vars           = DELAYN_translation$variable_list,
                builtin_stocks = DELAYN_translation$stock_list))
  }

  stl_smooth1 <- stringr::str_detect(equation, "\\bSMTH1\\b")

  if(stl_smooth1) {

    S1_translation      <- translate_SMOOTH1(var_name, equation, "isee")
    variable            <- S1_translation$variable

    return(list(vars           = list(variable),
                builtin_stocks = list(S1_translation$stock)))
  }

  stl_smooth3 <- stringr::str_detect(equation, "\\bSMTH3\\b")

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



interpret_equations_vensim <- function(non_const_obj, consts) {

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
                               fun  = translation$graph_fun)
  }

  var_obj <- list(name     = var_name,
                  equation = equation)

  if(there_is_graph_fun) {
    var_obj$graph_fun <- graph_fun
  }

  list(vars = list(var_obj))
}

get_auxs_names <- function(auxs_xml) {

  lapply(auxs_xml, function(aux_xml) {

    raw_var_name <- xml2::xml_attr(aux_xml, "name")
    var_name     <- sanitise_elem_name(raw_var_name)
    var_name     <- check_elem_name(var_name)
  })
}

override_consts <- function(actual_consts, const_list) {

  consts_override <- names(const_list)
  const_names     <- get_names(actual_consts)

  for(i in seq_along(const_list)) {

    cst     <- consts_override[[i]]
    pos_cst <- which(cst == const_names)

    if(length(pos_cst) == 0) {
      msg <- paste0("Can't find constant: ", cst)
      stop(msg, call. = FALSE)
    }

    actual_consts[[pos_cst]]$value <- const_list[[cst]]
  }

  actual_consts
}
