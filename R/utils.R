#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

remove_NULL  <-  function(x.list) {
  x.list[unlist(lapply(x.list, length) != 0)]
}
