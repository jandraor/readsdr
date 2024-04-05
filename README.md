
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readsdr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/jandraor/readsdr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jandraor/readsdr?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/readsdr)](https://CRAN.R-project.org/package=readsdr)
[![R-CMD-check](https://github.com/jandraor/readsdr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jandraor/readsdr/actions/workflows/R-CMD-check.yaml)
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
```

For reading Vensim models, they must be exported as .xmile.

## Tutorials

For information on how to use this package, please check:

- [Basics](https://jandraor.github.io/tutorials/Basics.html)

- [Inference](https://jandraor.github.io/tutorials/Inference.html)

## Supported builtins

- **Stella**
  - Comparison operators (`=`, `<>`)
  - Logical operators (`AND`, `OR`, `NOT`)
  - `If Else Then`
  - Inequality operators (`<`, `>`)
  - `Pulse` <sup>1</sup>
  - `Step` <sup>1</sup>
  - Endogenous table functions
  - Smoothing functions<sup>2</sup>: `SMTH1`, `SMTH3`, `SMTHN`
  - Delay functions: `DELAYN`
  - Limited support to *uni-dimensional* arrays. *Apply all* translation
    is restricted to numeric values.
  - Math functions: `ABS`, `SQRT`
  - Stats functions: `NORMAL`<sup>3</sup>
- **Vensim**
  - Comparison operators (`=`, `<>`)
  - Logical operators (`:AND:`, `:OR:`, `:NOT:`)
  - `IF_THEN_ELSE`
  - Inequality operators (`<`, `>`)
  - `Pulse` <sup>1</sup>
  - `Pulse Train` <sup>1</sup>
  - `Step` <sup>1</sup>
  - Endogenous table functions
  - Smoothing functions<sup>2</sup>: `SMOOTH`, `SMOOTH3`, `SMOOTH3I`,
    `SMOOTHI`
  - Delay functions: `DELAY N`
  - Math functions: `ABS`, `SQRT`
  - Limited support to *bi-dimensional* arrays.
  - Stats functions: `RANDOM NORMAL`<sup>3</sup>

<sup>1</sup> Restricted to Euler integration.

<sup>2</sup> These functions cannot be part of more complex mathematical
expressions. That is, the auxiliary variable must only contain one
smoothing function and **nothing else**.

<sup>3</sup> Seed is ignored.

## Notes

- *uniflow* and *non-negative* stock features from *Stella* are **not**
  supported.

- No built-in is supported for translations to *Stan* code.

- Modules from *Stella* are **not** supported.

## Applications

This package has been instrumental in the following works:

- [Andrade & Duggan (2023)](https://doi.org/10.1098/rsos.230515).
  *Anchoring the mean generation time in the SEIR to mitigate biases in*
  $\Re_0$ *estimates due to uncertainty in the distribution of the
  epidemiological delays*. **Royal Society Open Science**.

- [Andrade & Duggan
  (2022)](https://doi.org/10.1371/journal.pcbi.1010206). *Inferring the
  effective reproductive number from deterministic and
  semi-deterministic compartmental models using incidence and mobility
  data*. **PLOS Computational Biology**.

- [Andrade & Duggan (2021)](https://doi.org/10.1002/sdr.1693). *A
  Bayesian approach to calibrate system dynamics models using
  Hamiltonian Monte Carlo*. **System Dynamics Review**.

- [Andrade & Duggan
  (2020)](https://doi.org/10.1016/j.epidem.2020.100415). *An evaluation
  of Hamiltonian Monte Carlo performance to calibrate age-structured
  compartmental SEIR models to incidence data*. **Epidemics**.

## Acknowledgments

Thanks to:

- [Rogelio Oliva](http://people.tamu.edu/~roliva/) for advocating the
  need to create an open-source tool for the System Dynamics community
  and push forward this endeavour.
- Sergey Naumov for his ideas to implement several functions in this
  package.
- [Jim Duggan](https://github.com/JimDuggan) from whom I borrowed ideas
  to implement this package.

## References

[Duggan, J. (2016). *System Dynamics Modeling with R*.
Springer.](https://link.springer.com/book/10.1007/978-3-319-34043-2)
