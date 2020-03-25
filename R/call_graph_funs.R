#' Call graphical functions
#'
#' Puts graphical functions of a model in the global environment.
#'
#' @param mdl_obj The output of \code{read_xmile}
#'
#' @export
#'
#' @examples
#' \donttest{
#' mdl <- read_xmile("my_model.xmile")
#' call_graphical_funs(mdl)
#'}
call_graphical_funs <- function(mdl_obj) {

  for(var_obj in mdl_obj$description$variables) {
    if(!is.null(var_obj$graph_fun)) {
      assign(var_obj$graph_fun$name, var_obj$graph_fun$fun, envir = .GlobalEnv)
    }
  }
  invisible(NULL)
}
