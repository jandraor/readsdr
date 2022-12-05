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

execute_trans <- function(val, trans_type) if(trans_type == "inv") return(1/val)
