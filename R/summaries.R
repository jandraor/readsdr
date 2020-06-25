
#' Summarise the information of the model's constants in a data frame
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

 consts_df <- do.call("rbind", consts_list)
}
