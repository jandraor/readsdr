extract_structure_from_XMILE <- function(filepath) {

  raw_xml <- safe_read(filepath)
  vendor  <- which_vendor(raw_xml)

  sim_specs  <- xml2::xml_find_all(raw_xml, ".//d1:sim_specs")
  parameters <- create_param_obj_xmile(sim_specs)

  #-----------------------------------------------------------------------------
  dims_obj <- NULL

  cld_xml      <- xml2::xml_children(raw_xml)
  child_names  <- xml2::xml_name(cld_xml)

  if("dimensions" %in% child_names) {
    dims_obj <- create_dims_obj(raw_xml)
  }
  #-----------------------------------------------------------------------------

  variables_xml   <- xml2::xml_find_first(raw_xml, ".//d1:variables")

  auxs_xml        <- xml2::xml_find_all(variables_xml, ".//d1:flow|.//d1:aux")

  vars_and_consts <- create_vars_consts_obj_xmile(auxs_xml, vendor, dims_obj)
  variables       <- arrange_variables(vars_and_consts$variables)
  constants       <- vars_and_consts$constants

  stocks_xml     <-  xml2::xml_find_all(variables_xml, ".//d1:stock")

  args_fun       <- list(stocks_xml = stocks_xml, variables = variables,
                         constants = constants, dims_obj = dims_obj)

  if("builtin_stocks" %in% names(vars_and_consts)) {
    args_fun$builtin_stocks <- vars_and_consts$builtin_stocks
  }

  levels         <- do.call("create_level_obj_xmile", args_fun)

  list(parameters = parameters,
       levels = levels,
       variables = variables,
       constants = constants)
}

compute_init_value <- function(var_name, equation, auxs) {

  tryCatch(
    error = function(cnd) {
      stop(stringr::str_glue("Can't compute the init value of '{var_name}'"),
           call. = FALSE)
    }, {
      vars_in_equation <- extract_variables(var_name, equation)
      newEquation      <- equation
      auxs_names       <- sapply(auxs, function(aux) aux$name)

      for(var_in_equation in vars_in_equation) {
        pos_aux     <- which(auxs_names == var_in_equation)
        aux_obj     <- auxs[[pos_aux]]
        rpl_val     <- aux_obj$equation # replacement value


        if(!is.null(aux_obj$graph)){
          input_equation <- stringr::str_match(rpl_val, "f.+\\((.+)\\)")[[2]]
          input          <- compute_init_value("", input_equation, auxs)
          assign(aux_obj$graph_fun$name, aux_obj$graph_fun$fun)
          rpl_val        <- do.call(aux_obj$graph_fun$name, list(input))
        }

        replacement <- paste0("(", rpl_val, ")")
        pattern     <- paste0("\\b", var_in_equation, "\\b")
        newEquation <- gsub(pattern, replacement, newEquation)
      }

      contains_characters <- stringr::str_detect(newEquation, "[A-Za-z]")

      if(contains_characters) {
        initValue <- compute_init_value(var_name, newEquation, auxs)
        return(initValue)
      }

      if(!contains_characters) {
        newEquation <- parse(text = newEquation)
        initValue   <- eval(newEquation)
      }

      initValue
    }
  )
}

sanitise_elem_name <- function(elem_name) {
  elem_name %>%
    stringr::str_replace_all("\n|\t|~","") %>%
    stringr::str_replace_all(" |\\\\n", "_")
}

sanitise_init_value <- function(init_value) {
  init_value %>%
    stringr::str_replace_all("\\{.*?\\}", "") %>%  # removes commentaries
    stringr::str_replace_all("\n|\t|~","")
}

sanitise_aux_equation <- function(equation, vendor) {
  sanitised_equation <- sanitise_arrays(equation, vendor)

  sanitised_equation %>%
    translate_if_else_functions(vendor) %>%
    stringr::str_replace_all("\n|\t|~| ","") %>%
    stringr::str_replace_all("\\{.*?\\}", "") %>%  # removes commentaries
    translate_extrema() %>%
    translate_math_funs() %>%
    translate_stat_funs(vendor) %>%
    translate_logical_operators(vendor) %>%
    translate_comparison_operators() %>%
    translate_time_builtins() %>%
    eval_constant_expr() # Must go at the end
}

#' Evaluate constant expression
#'
#' eval_constant_expr(3 + 3) # returns 6
#' eval_constant_expr(a + 3) # returns a + 3
#'
#' @param equation A string
#'
#' @return A string
#' @noRd
eval_constant_expr <- function(equation) {
  tryCatch(
    error = function(cnd) equation,
    {
      evaluated_expr <- eval(parse(text = equation), envir = baseenv())
      as.character(evaluated_expr)
    }
  )
}

check_elem_name <- function(elem_name) {
  is_valid <- make.names(elem_name) == elem_name

  if(!is_valid) {
    error_message <- paste0(elem_name , " is not a valid name for a variable")
    stop(error_message, call. = FALSE)
  }

  elem_name
}

which_vendor <- function(raw_xml) {
  vendor_raw <- xml2::xml_find_first(raw_xml, ".//d1:vendor") %>%
    xml2::xml_text()

  is_Vensim <- stringr::str_detect(vendor_raw, "Ventana")
  is_isee   <- stringr::str_detect(vendor_raw, "isee")

  if(is_Vensim) vendor <- "Vensim"
  if(is_isee)   vendor <- "isee"

  vendor
}

safe_read <- function(filepath) {

  tryCatch(
    error = function(cnd) {

      tryCatch(
        error = function(cnd) {
          stop("Invalid XML file", call. = FALSE)

        },
        readChar(filepath, file.info(filepath)$size) %>%
          sanitise_xml() %>% xml2::read_xml()
      )

    },

    xml2::read_xml(filepath)
  )
}

sanitise_arrays <- function(equation, vendor) {

 if(vendor == "isee") {
   pattern        <- "\\[(.+?)\\]"
   there_is_array <- stringr::str_detect(equation, pattern)

   if(there_is_array) {
     equation <- stringr::str_replace(equation, "\\[(.+?)\\]", "_\\1")
     equation <- sanitise_arrays(equation, vendor)
   }
 }

  equation
}
