
stan_trans_params <- function(unk_list, stock_list){

  unk_elems <- names(unk_list)

  if("measurement" %in% unk_elems) {

    meas_obj <- lapply(unk_list$measurement, function(par_obj) {

      obj_elems <- names(par_obj)

      if("par_trans" %in% obj_elems) {

        decl  <- stringr::str_glue("  real {par_obj$name};")
        asnmt <- get_trans_line(par_obj)


        return( list(decl  = decl,
                     asnmt = asnmt))
      }

      NULL

    }) %>% remove_NULL()

  }

  heading <- "transformed parameters{"

  var_decl <- paste(
    "  array[n_obs] vector[n_difeq] x; // Output from the ODE solver",
    "  vector[n_difeq] x0; // init values",
    "  array[n_params] real params;", sep = "\n")

  assignments <- "  //assignments"

  if(length(meas_obj) > 0) {

    meas_decl_lines <- purrr::map_chr(meas_obj, "decl") %>% paste(sep = "\n")
    var_decl        <- paste(var_decl, meas_decl_lines, sep = "\n")

    asnmt_lines <- purrr::map_chr(meas_obj, "asnmt") %>% paste(sep = "\n")
    assignments <- paste(assignments, asnmt_lines, sep = "\n")
  }

  stock_inits <- construct_stock_init_lines(stock_list, unk_list)
  assignments <- paste(assignments, stock_inits, sep = "\n")

  perro <- "  array[n_obs] real delta_x;" # this is optional


  paste(heading,
        var_decl,
        assignments,
        "}", sep = "\n")
}

get_trans_line <- function(par_obj) {

  par_trans <- par_obj$par_trans
  par_name  <- par_obj$name

  if(par_trans == "inv") {

    return(stringr::str_glue("  {par_name} = 1 / inv_{par_name};"))
  }
}

construct_stock_init_lines <- function(stock_list, unk_list) {

  lines_list <- purrr::imap_chr(stock_list, function(stk_obj, i) {
    stk_name <- stk_obj$name
    stringr::str_glue("  x0[{i}] = {stk_obj$initValue}; // {stk_name}")
  })

  paste(lines_list, collapse = "\n")
}
