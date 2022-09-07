array_equations <- function(aux_obj, dims_obj, dim_names, vendor) {

  dims_dict <- dims_obj$dictionary
  glob_dims <- dims_obj$global_dims

  n_dims <- length(dim_names)

  dims_list        <- lapply(dim_names, function(dim_name) glob_dims[[dim_name]])
  names(dims_list) <- dim_names
  elems            <- combine_dims(dims_list)

  raw_equation <- aux_obj$equation
  aux_name     <- aux_obj$name

  if(vendor == "Vensim") {

    vector_pattern <- create_array_pattern(dims_list)
    is_an_array    <- stringr::str_detect(raw_equation, vector_pattern)

    if(is_an_array) {

      clean_equation <- raw_equation %>%
         stringr::str_replace_all(";",",")

      clean_equation <- substr(clean_equation,1, nchar(clean_equation) - 1)
      equations      <- stringr::str_split(clean_equation, ",")[[1]]
      are_const      <- !is.na(suppressWarnings(as.numeric(equations)))
    }

    if(!is_an_array) {

      devec_eqs <- devectorise_equation(raw_equation, dims_list)

      equations <- sapply(devec_eqs, sanitise_aux_equation, vendor,
                          USE.NAMES = FALSE)

      are_const <- !is.na(suppressWarnings(as.numeric(equations)))
    }
  }

  if(vendor == "isee") {

    equations    <- sanitise_aux_equation(raw_equation, vendor)
    are_const    <- !is.na(suppressWarnings(as.numeric(equations)))

    if(!are_const) {

      eq_vars <- extract_variables(lhs = aux_name, equations)

      arrayed_vars <- names(dims_dict)

      for(var_in_eq in eq_vars) {

        if(var_in_eq %in% arrayed_vars) {

          var_dims <- dims_dict[[var_in_eq]]
          dims_idx <- paste(var_dims, collapse = ",")

          replacement       <- stringr::str_glue("{var_in_eq}[{dims_idx}]")
          pattern           <- stringr::str_glue("\\b{var_in_eq}\\b")
          unvectorised_eq   <- stringr::str_replace_all(equations, pattern,
                                                  replacement)
          devec_eqs <- devectorise_equation(unvectorised_eq, dims_list)

          equations <- sapply(devec_eqs, sanitise_aux_equation, vendor,
                              USE.NAMES = FALSE)

          are_const <- !is.na(suppressWarnings(as.numeric(equations)))

        }

      }
    }

  }

  list(equations  = equations,
       are_const  = are_const,
       elems      = elems)
}


devectorise_equation <- function(raw_equation, dims_list) {

  dim_names   <- names(dims_list)
  pattern     <- paste0("\\[", paste(dim_names, collapse = ","), "\\]")

  elems       <- combine_dims(dims_list)
  replacement <- paste0("_", elems)

  stringr::str_replace_all(raw_equation, pattern, replacement)

}

combine_dims <- function(dims_list) {

  rev_dims_list <- rev(dims_list)
  rev_combs_df  <- expand.grid(rev_dims_list, stringsAsFactors = FALSE)
  combs_df      <- rev(rev_combs_df)
  do.call(paste, c(combs_df, sep = "_"))

}

create_array_pattern <- function(dims_list) {

  n_dims      <- length(dims_list)
  dim1_length <- length(dims_list[[1]])
  rgx_elems   <- rep(".+?", dim1_length)
  rgx_array   <- paste(rgx_elems, collapse = ",")

  if(n_dims == 2) {
    rgx_row    <- paste0(rgx_array, ";")
    rgx_matrix <- rep(rgx_row, length(dims_list[[2]]))
    rgx_array  <- paste(rgx_matrix, collapse = "")
  }

  rgx_array

}
