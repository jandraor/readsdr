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

#' Calculate confidence intervals
#'
#' @param hsn Hessian matrix
#' @param conf_level A numeric input indicating the confidence level
#'
#' @inheritParams sd_interpret_estimates
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' estimates <- c(-0.2630303, 1.5788579)
#' par_list  <- list(list(par_name  = "par_inv_R0",
#'                        par_trans = "expit"),
#'                   list(par_name  = "I0",
#'                        par_trans = "exp"))
#' hsn <- matrix(c(3513.10521, -493.5469626,
#'                 -493.5469626, 88.4871290), ncol = 2)
#' sd_conf_intervals(estimates, par_list, hsn)
sd_conf_intervals <- function(estimates, par_list, hsn, conf_level = 0.95) {

  alpha <- 1 - conf_level

  cov_matrix <- solve(hsn)

  SE <- sqrt(diag(cov_matrix)) # Standard errors

  lower_bounds <- sd_interpret_estimates(estimates +
                                           stats::qnorm(alpha / 2) * SE,
                                         par_list)

  upper_bounds <- sd_interpret_estimates(estimates +
                                           stats::qnorm(1 - alpha / 2) * SE,
                                         par_list)

  df <- data.frame(lb        = as.numeric(lower_bounds[1, ,]),
                   ub        = as.numeric(upper_bounds[1, ,]))

  min_max <- apply(df, 1, function(x) c(min(x), max(x)))

  result_df <- data.frame(parameter = colnames(lower_bounds),
                          lb = min_max[1, ],
                          ub = min_max[2, ])

  colnames(result_df) <- c("parameter",
                           paste0(round(100 * alpha / 2,1), "%"),
                           paste0(round(100 * (1 - alpha / 2),2), "%"))

  result_df
}

