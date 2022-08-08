
stan_params <- function(prior, lvl_obj) {

  parameter_lines <- sapply(prior, build_parameter_line) %>%
    paste(collapse = "\n")

  paste(
    "parameters {",
    parameter_lines,
    "}", sep = "\n")
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
