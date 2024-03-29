% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stan_postprocessing.R
\name{extract_timeseries_stock}
\alias{extract_timeseries_stock}
\title{Extract the values over time of a stock from a Stan fit}
\usage{
extract_timeseries_stock(stock_name, posterior_df, all_stocks, ODE_output)
}
\arguments{
\item{stock_name}{A string that indicates the stock's name for which the
function will construct the timeseries.}

\item{posterior_df}{A Stan fit object converted into a data frame}

\item{all_stocks}{A vector of strings that contains the names of all the
stocks in the model. This vector must have the same order as the differential
equations in the Stan code.}

\item{ODE_output}{A string that indicates the name of the variable where
model's output in stored in Stan.}
}
\value{
A data frame
}
\description{
Extract the values over time of a stock from a Stan fit
}
\examples{
posterior_df <- data.frame(`yhat[1,2]` = rep(0, 2), `yhat[2,2]` = rep(1, 2),
                            check.names = FALSE)
stocks       <- c("S1", "S2")
extract_timeseries_stock("S2", posterior_df, stocks, "yhat")
}
