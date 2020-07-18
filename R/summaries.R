
#' Summarise the information of a model's constants in a data frame
#'
#' @param mdl A list which is the output from read_xmile.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' path <- system.file("models", "SIR.stmx", package = "readsdr")
#' mdl  <- read_xmile(path)
#' sd_constants(mdl)
sd_constants <- function(mdl) {
 consts_list <- lapply(mdl$description$constants, function(const_list) {
   data.frame(name = const_list$name, value = const_list$value)
 })

 do.call("rbind", consts_list)
}

#' Summarise the information of a model's stocks in a data frame
#'
#' @inheritParams sd_constants
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' path <- system.file("models", "SIR.stmx", package = "readsdr")
#' mdl  <- read_xmile(path)
#' sd_stocks(mdl)
sd_stocks <- function(mdl) {
  lvls_list <- lapply(mdl$description$levels, function(lvl_list) {
    data.frame(name = lvl_list$name, init_value = lvl_list$initValue)
  })
  do.call("rbind", lvls_list)
}
