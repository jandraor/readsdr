% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_utils.R
\name{discrete_net_change}
\alias{discrete_net_change}
\title{Estimate discrete change for a stock}
\usage{
discrete_net_change(sim_df, cumulative_var)
}
\arguments{
\item{sim_df}{A data frame with the simulation output}

\item{cumulative_var}{A string that indicates to which variable the discrete
change will be estimated}
}
\value{
A dataframe.
}
\description{
Estimate discrete change for a stock
}
\examples{
test_output <- data.frame(time = seq(0, 2, by = 0.25),
                          C    = c(0, rep(5,4), rep(20, 4)))
discrete_net_change(test_output, "C")
}
