#' Posterior function
#'
#' @param filepath
#' @param meas_mdl
#' @param estimated_params
#'
#' @inheritParams read_xmile
#' @inheritParams sd_Bayes
#' @return A function
#' @export
#'
#' @examples
#' filepath         <- system.file("models/", "SEIR.stmx", package = "readsdr")
#' mm1              <- "y ~ neg_binomial_2(net_flow(C), phi)"
#' meas_mdl         <- list(mm1)
#' estimated_params <- list(
#'   sd_prior("par_beta", "lognormal", c(0, 1)),
#'   sd_prior("par_rho", "beta", c(2, 2)),
#'   sd_prior("I0", "lognormal", c(0, 1), "init"))
#'   sd_Bayes(filepath, meas_mdl, estimated_params)
#' fun <- sd_posterior_fun(filepath, meas_mdl, estimated_params)
sd_posterior_fun <- function(filepath, meas_mdl, estimated_params) {

}
