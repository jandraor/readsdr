#' SD prior
#'
#' @param par_name A string
#' @param dist A string
#' @param dist_pars A vector
#' @param type A string. It can be either 'constant' or 'init'. It is 'constant'
#'   by default. 'init' refers to parameters that have only affect the model at
#'   time 0.
#' @param min_0 An optional boolean indicating whether the prior has a lower
#'   bound at zero. In the current implementation, this parameter only has an
#'   effect on normal priors.
#'
#' @return A list
#' @export
#'
#' @examples
#' sd_prior("par_beta", "lognormal", c(0, 1))
#' sd_prior("par_rho", "normal", c(0, 1), min_0 = TRUE)
sd_prior <- function(par_name, dist, dist_pars, type = "constant",
                     min_0 = FALSE) {

  if(dist == "beta") {

    dist_obj <- list(par_name = par_name,
                     dist     = dist,
                     alpha    = dist_pars[[1]],
                     beta     = dist_pars[[2]],
                     min      = 0,
                     max      = 1,
                     type     = type)

    return(dist_obj)
  }

  if(dist == "exponential") {

    dist_obj <- list(par_name = par_name,
                     dist     = dist,
                     beta     = dist_pars[[1]],
                     min      = 0,
                     type     = type)

    return(dist_obj)
  }

  if(dist == "lognormal") {

    dist_obj <- list(par_name = par_name,
                     dist     = dist,
                     mu       = dist_pars[[1]],
                     sigma    = dist_pars[[2]],
                     min      = 0,
                     type     = type)

    return(dist_obj)
  }

  if(dist == "normal") {

    dist_obj <- list(par_name = par_name,
                     dist     = dist,
                     mu       = dist_pars[[1]],
                     sigma    = dist_pars[[2]],
                     type     = type)

    if(min_0) dist_obj$min <- 0

    return(dist_obj)
  }

  msg <- paste0("sd_prior() does not support the '", dist, "' distribution.")

  stop(msg, call. = FALSE)
}
