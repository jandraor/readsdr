
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readsdr

<!-- badges: start -->

[![R build
status](https://github.com/jandraor/readsdr/workflows/R-CMD-check/badge.svg)](https://github.com/jandraor/readsdr/actions?workflow=R-CMD-check)
[![Codecov test
coverage](https://codecov.io/gh/jandraor/readsdr/branch/master/graph/badge.svg)](https://codecov.io/gh/jandraor/readsdr?branch=master)
<!-- badges: end -->

## Overview

The goal of readsdr is to bridge the design capabilities from
specialised System Dynamics software with the powerful numerical tools
offered by R libraries. The package accomplishes this goal by parsing
.xmile files ([Vensim](https://vensim.com/) and
[Stella](https://www.iseesystems.com/) models) into R objects to
construct [networks](http://igraph.org) (graph theory), ODE functions
for [deSolve](http://desolve.r-forge.r-project.org/) and
[Stan](https://mc-stan.org/)
.

## Installation

<!-- You can install the released version of readsdr from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("readsdr") -->

<!-- ``` -->

<!-- And the development version from [GitHub](https://github.com/) with: -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jandraor/readsdr")
```

## Usage

``` r
library(readsdr)
filepath <- system.file("models/", "SIR.stmx", package = "readsdr")
mdl      <- read_xmile(filepath) 
summary(mdl)
#>                    Length Class  Mode
#> description        4      -none- list
#> deSolve_components 3      -none- list
#> graph_dfs          2      -none- list
```

For reading Vensim models, they must be exported as .xmile.

`vignette("Introduction_to_readsdr")` gives more detail on how to use
the package.

<!-- ## Acknowledgments -->

<!-- Thanks to: -->
