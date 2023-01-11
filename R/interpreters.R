#' Interpret estimates
#'
#' @param estimates_list A list
#' @param par_list A list
#'
#' @return A list
#' @export
#'
#' @examples
#'   estimates_list <- c(par_beta = 0,
#'                       par_rho  = 0.8472979,
#'                       I0       = 0,
#'                       inv_phi  = -2.302585)
#'
#'   par_list <- list(list(par_name  = "par_beta",
#'                         par_trans = "exp"),
#'                    list(par_name  = "par_rho",
#'                         par_trans = "expit"),
#'                    list(par_name  = "I0",
#'                         par_trans = "exp"),
#'                    list(par_name  = "phi",
#'                         par_trans = c("exp", "inv")))
#'   sd_interpret_estimates(estimates_list, par_list)
sd_interpret_estimates <- function(estimates_list, par_list) {

  lapply(seq_along(par_list), function(i) {

    par_obj <- par_list[[i]]

    if(length(par_obj$par_trans) == 1L) val <- do.call(par_obj$par_trans,
                                                       list(estimates_list[[i]]))

    if(length(par_obj$par_trans) > 1L) {

      val <- estimates_list[[i]]

      for(j in seq_along(par_obj$par_trans)) {

        val <- do.call(par_obj$par_trans[[j]], list(val))
      }
    }

    val <- list(val)
    names(val) <- par_obj$par_name
    val
  }) -> output_list

  unlist(output_list)
}
