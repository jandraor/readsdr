
stan_params <- function(prior) {

  prior <- sort_estimated_pars(prior)

  parameter_lines <- sapply(prior, build_parameter_line) |>
    paste(collapse = "\n")

  paste(
    "parameters {",
    parameter_lines,
    "}", sep = "\n")
}

sort_estimated_pars <- function(prior) {

  par_names <- get_names(prior, "par_name")

  bound_dependencies <- lapply(prior, \(prior_obj) {

    bounds <- c(prior_obj$min, prior_obj$max)

    eq <- ".placeholder"

    if(is.character(bounds)) eq <- paste(bounds, collapse = "+")

    list(name     = prior_obj$par_name,
         equation = eq) # For arrange_variables()
  })

  sorted_pars <- arrange_variables(bound_dependencies) |> get_names()

  prior[match(sorted_pars, par_names)]
}


build_parameter_line <- function(prior_obj) {

  obj_elems <- names(prior_obj)

  decl <- "  real" # declaration

  if(all(c("min", "max") %in% obj_elems)) {

    #bounds
    bds <- stringr::str_glue("<lower = {prior_obj$min}, upper = {prior_obj$max}>")

    decl <- paste0(decl, bds)
  }

  if("min" %in% obj_elems & !"max" %in% obj_elems) {
    bds <- stringr::str_glue("<lower = {prior_obj$min}>")

    decl <- paste0(decl, bds)
  }

  if(!"min" %in% obj_elems & "max" %in% obj_elems) {
    bds <- stringr::str_glue("<upper = {prior_obj$max}>")

    decl <- paste0(decl, bds)
  }

  par_name <- prior_obj$par_name

  stringr::str_glue("{decl} {par_name};")
}
