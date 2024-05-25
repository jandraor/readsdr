#' Specify priors for the estimated parameters.
#'
#'\code{sd_prior} returns a list characterising the features of the prior.
#'
#' @param par_name A string indicating the name of the estimated parameter.
#' @param dist A string indicating the name of the prior distribution. This name
#' should be consistent with Stan language. For instance, "normal" indicates
#' the normal distribution in Stan language.
#' @param dist_pars A numeric vector. For instance, if \code{dist} = "normal",
#'   then \code{dist_pars} will be a vector of size 2 corresponding to
#'   the \emph{location} (mean) and \emph{scale} (standard deviation).
#' @param type A string. It can be either 'constant' or 'init'. It is 'constant'
#'   by default. 'init' refers to parameters that have only affect the model at
#'   time 0.
#' @param min An optional numeric or a string value indicating the estimated
#'   parameter's lower bound. This value overrides the inferred bound from the
#'   prior distribution. For instance, specifying a beta distribution for the
#'   estimated parameter inherently sets the lower bound to 0. Providing a
#'   value to \code{min} will override this default with the supplied value. If
#'   the supplied value is a string, it should be the name of another estimated
#'   parameter.
#' @param max An optional numeric value or a string indicating the estimated
#'   parameter's upper bound. This value overrides the inferred bound from the
#'   prior distribution. For instance, specifying a beta distribution for the
#'   estimated parameter inherently sets the upper bound to \code{1}. Providing
#'   a value to \code{max} will override this default with the supplied value.
#'   If the supplied value is a string, it should be the name of another
#'   estimated parameter.
#' @return A list
#' @export
#'
#' @examples
#' sd_prior("par_beta", "lognormal", c(0, 1))
#' sd_prior("par_rho", "normal", c(0, 1), min = 0)
sd_prior <- function(par_name, dist, dist_pars, type = "constant", min = NULL,
                     max = NULL) {

  supported_dists <- c("beta", "exponential", "lognormal", "normal")

  if(!dist %in% supported_dists) {

    msg <- paste0("sd_prior() does not support the '", dist, "' distribution.")
    stop(msg, call. = FALSE)
  }

  dist_obj <- list(par_name = par_name,
                   dist     = dist,
                   type     = type)

  if(dist == "beta") {

    dist_obj$alpha <- dist_pars[[1]]
    dist_obj$beta  <- dist_pars[[2]]
    dist_obj$min   <- 0
    dist_obj$max   <- 1
  }

  if(dist == "exponential") {

    dist_obj$beta <- dist_pars[[1]]
    dist_obj$min  <- 0
  }

  if(dist == "lognormal") {

    dist_obj$mu    <- dist_pars[[1]]
    dist_obj$sigma <- dist_pars[[2]]
    dist_obj$min   <- 0
  }

  if(dist == "normal") {

    dist_obj$mu    <- dist_pars[[1]]
    dist_obj$sigma <- dist_pars[[2]]
  }

  if(!is.null(min)) dist_obj$min <- min
  if(!is.null(max)) dist_obj$max <- max

  dist_obj
}
