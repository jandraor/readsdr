#' Interpret estimates
#'
#' @param estimates A list or data frame
#' @param par_list A list
#'
#' @return A data frame
#' @export
#'
#' @examples
#'   estimates <- c(par_beta = 0,
#'                  par_rho  = 0.8472979,
#'                  I0       = 0,
#'                  inv_phi  = -2.302585)
#'
#'   par_list <- list(list(par_name  = "par_beta",
#'                         par_trans = "exp"),
#'                    list(par_name  = "par_rho",
#'                         par_trans = "expit"),
#'                    list(par_name  = "I0",
#'                         par_trans = "exp"),
#'                    list(par_name  = "phi",
#'                         par_trans = c("exp", "inv")))
#'   sd_interpret_estimates(estimates, par_list)
sd_interpret_estimates <- function(estimates, par_list) {

  if(is.numeric(estimates))    estimate_list <- list(estimates)

  if(is.data.frame(estimates)) estimate_list <- df2list(estimates)

  purrr::map_dfr(estimate_list, decode_estimates, par_list)
}

decode_estimates <- function(estimate_list, par_list) {

  lapply(seq_along(par_list), function(i) {

    par_obj <- par_list[[i]]

    if(length(par_obj$par_trans) == 1L) val <- do.call(par_obj$par_trans,
                                                       list(estimate_list[[i]]))

    if(length(par_obj$par_trans) > 1L) {

      val <- estimate_list[[i]]

      for(j in seq_along(par_obj$par_trans)) {

        val <- do.call(par_obj$par_trans[[j]], list(val))
      }
    }

    val <- list(val)
    names(val) <- par_obj$par_name
    val
  }) -> output_list

  data.frame(output_list)
}

