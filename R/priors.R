sd_prior <- function(par_name, dist, dist_pars, type = "constant") {

  if(dist == "beta") {

    dist_obj <- list(par_name = par_name,
                     dist     = "beta",
                     alpha    = dist_pars[[1]],
                     beta     = dist_pars[[2]],
                     min      = 0,
                     max      = 1,
                     type     = type)
  }

  if(dist == "lognormal") {

    dist_obj <- list(par_name = par_name,
                     dist     = "lognormal",
                     mu       = dist_pars[[1]],
                     sigma    = dist_pars[[2]],
                     min      = 0,
                     type     = type)
  }

 dist_obj
}
