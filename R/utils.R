#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

remove_NULL  <-  function(x.list) {
  x.list[unlist(lapply(x.list, length) != 0)]
}

get_names <- function(obj_list, name_var = "name") {
  sapply(obj_list, function(obj) obj[[name_var]])
}

as_row_list <- function(df) do.call(function(...) Map(list,...), df)

execute_trans <- function(val, trans_type, return_type = "numeric") {

  if(trans_type == "inv" & return_type == "numeric") return(1/val)

  if(trans_type == "inv" & return_type == "text") return(paste0("1/", val))
}

is_string_numeric <- function(x) suppressWarnings(ifelse(!is.na(as.numeric(x)),
                                                         TRUE, FALSE))
