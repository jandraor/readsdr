#' SD prior
#'
#' @param par_name A string
#' @param dist A string
#' @param dist_pars A vector
#' @param type A string. It can be either 'constant' or 'init'. It is 'constant'
#' by default.
#'
#' @return A list
#' @export
#'
#' @examples
#' sd_prior("par_beta", "lognormal", c(0, 1))
sd_prior <- function(par_name, dist, dist_pars, type = "constant") {

  if(dist == "beta") {

    dist_obj <- list(par_name = par_name,
                     dist     = "beta",
                     alpha    = dist_pars[[1]],
                     beta     = dist_pars[[2]],
                     min      = 0,
                     max      = 1,
                     type     = type)

    return(dist_obj)
  }

  if(dist == "exponential") {

    dist_obj <- list(par_name = par_name,
                     dist     = "exponential",
                     beta     = dist_pars[[1]],
                     min      = 0,
                     type     = type)

    return(dist_obj)
  }

  if(dist == "lognormal") {

    dist_obj <- list(par_name = par_name,
                     dist     = "lognormal",
                     mu       = dist_pars[[1]],
                     sigma    = dist_pars[[2]],
                     min      = 0,
                     type     = type)

    return(dist_obj)
  }

  msg <- paste0("sd_prior() does not support the '", dist, "' distribution.")

  stop(msg, call. = FALSE)


}
