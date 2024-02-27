#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom deSolve timestep
#' @export
deSolve::timestep

remove_NULL  <-  function(x.list) {
  x.list[unlist(lapply(x.list, length) != 0)]
}

get_names <- function(obj_list, name_var = "name") {
  sapply(obj_list, function(obj) obj[[name_var]])
}

get_raw_names <- function(obj_list, name_var) {

  purrr::map_chr(obj_list, function(obj) {

    name <- obj[[name_var]]

    if("par_trans" %in% names(obj)) {

      name <- stringr::str_remove(name, paste0(obj$par_trans, "_"))
    }

    name
  })
}

as_row_list <- function(df) do.call(function(...) Map(list,...), df)

execute_trans <- function(val, trans_type, return_type = "numeric") {

  if(trans_type == "inv" & return_type == "numeric") return(1/val)

  if(trans_type == "inv" & return_type == "text") return(paste0("1/", val))
}

is_string_numeric <- function(x) suppressWarnings(ifelse(!is.na(as.numeric(x)),
                                                         TRUE, FALSE))

# This function guarantees that consts and auxs have identical properties
format_consts_as_vars <- function(constants) {

  lapply(constants, function(const) {
    list(name = const$name, equation = const$value)
  })
}

df2list <- function(df) do.call(function(...) Map(list,...), df)



