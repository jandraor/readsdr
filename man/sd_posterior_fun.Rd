% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posterior_fun.R
\name{sd_posterior_fun}
\alias{sd_posterior_fun}
\title{Posterior function}
\usage{
sd_posterior_fun(
  filepath,
  meas_data_mdl,
  estimated_params,
  start_time = NULL,
  stop_time = NULL,
  timestep = NULL,
  integ_method = "euler",
  const_list = NULL
)
}
\arguments{
\item{filepath}{A string that indicates a path to a file with extension .stmx
or .xmile. Vensim files (.mdl) are not xmile files. They must be exported
from Vensim with extension .xmile}

\item{meas_data_mdl}{A list of lists. Each second-level list corresponds to
a sampling statement along with its measurements. Here is an example: \cr
\code{list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
           measurements = 1:10))}}

\item{estimated_params}{A list of lists. Each sublist describes each
parameter that will be estimated in the inference stage. To construct this
description, the user can avail of the function `sd_prior`.}

\item{start_time}{A number indicating the time at which the simulation begins.}

\item{stop_time}{A number indicating the time at which the simulation ends.}

\item{timestep}{A number indicating the time interval for the simulation.
Also known as \code{dt}.}

\item{integ_method}{A string indicating the integration method. It can be
either "euler" or "rk4"}

\item{const_list}{A list in which each element's name is the name of the
constant to override and the element's value correspond to the new value.}
}
\value{
A function
}
\description{
Posterior function
}
\examples{
filepath         <- system.file("models/", "SEIR.stmx", package = "readsdr")
meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                            measurements = 1:10))
estimated_params <- list(
  sd_prior("par_beta", "lognormal", c(0, 1)),
  sd_prior("par_rho", "beta", c(2, 2)),
  sd_prior("I0", "lognormal", c(0, 1), "init"))
fun <- sd_posterior_fun(filepath, meas_data_mdl, estimated_params)
}
