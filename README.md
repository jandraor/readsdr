
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readsdr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/jandraor/readsdr/branch/master/graph/badge.svg)](https://codecov.io/gh/jandraor/readsdr?branch=master)
[![R build
status](https://github.com/jandraor/readsdr/workflows/R-CMD-check/badge.svg)](https://github.com/jandraor/readsdr/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/readsdr)](https://CRAN.R-project.org/package=readsdr)
<!-- badges: end -->

## Overview

The goal of readsdr is to bridge the design capabilities from
specialised System Dynamics software with the powerful numerical tools
offered by R libraries. The package accomplishes this goal by parsing
.xmile files (Vensim and [Stella](https://www.iseesystems.com/) models)
into R objects to construct [networks](https://igraph.org) (graph
theory), ODE functions for
[deSolve](http://desolve.r-forge.r-project.org/) and
[Stan](https://mc-stan.org/).

## Installation

You can install the released version of readsdr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("readsdr")
```

And the development version from [GitHub](https://github.com/) with:

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
#> deSolve_components 4      -none- list
#> graph_dfs          2      -none- list
```

For reading Vensim models, they must be exported as .xmile.

For information on how to use this package, see the
[vignette](https://CRAN.R-project.org/package=readsdr/vignettes/Introduction_to_readsdr.html)
or type `vignette("Introduction_to_readsdr")` in R.

## Supported builtins

  - **Stella**
      - Comparison operators (`=`, `<>`)
      - Logical operators (`AND`, `OR`, `NOT`)
      - `If Else Then`
      - Inequality operators (`<`, `>`)
      - `Pulse` <sup>1</sup>
      - `Step` <sup>1</sup>
      - Endogenous table functions
      - Smoothing functions: `SMTH1`, `SMTH3`, `SMTHN`
      - Limited support to *uni-dimensional* arrays. *Apply all*
        translation is restricted to numeric values.
      - Math functions: `ABS`, `SQRT`
      - Stats functions: `NORMAL`
  - **Vensim**
      - Comparison operators (`=`, `<>`)
      - Logical operators (`:AND:`, `:OR:`, `:NOT:`)
      - `IF_THEN_ELSE`
      - Inequality operators (`<`, `>`)
      - `Pulse` <sup>1</sup>
      - `Pulse Train` <sup>1</sup>
      - `Step` <sup>1</sup>
      - Endogenous table functions
      - Smoothing functions: `SMOOTH`, `SMOOTH3`, `SMOOTH3I`, `SMOOTHI`
      - Math functions: `ABS`, `SQRT`

<sup>1</sup> Restricted to Euler integration.

## Notes

  - *uniflow* and *non-negative* stock features from *Stella* are
    **not** supported.

  - No built-in is supported for translations to *Stan* code.

  - Modules from *Stella* are **not** supported.

## Acknowledgments

Thanks to:

  - [Rogelio Oliva](http://people.tamu.edu/~roliva/) for advocating the
    need to create an open-source tool for the System Dynamics community
    and push forward this endeavour.
  - Sergey Naumov for his ideas to implement several functions in this
    package.
  - [Jim
    Duggan](http://www.nuigalway.ie/our-research/people/engineering-and-informatics/jamesduggan/)
    from whom I borrowed ideas to implement this package.

## References

[Duggan, J. (2016). *System Dynamics Modeling with R*.
Springer.](https://www.springer.com/us/book/9783319340418)
