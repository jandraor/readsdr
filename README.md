
# readSDr

## Overview

The goal of ‘readSDr’ is to bridge the design capabilities from
specialised System Dynamics software with the powerful numerical tools
offered by R libraries. The package accomplishes this goal by parsing
.xmile files ([Vensim](https://vensim.com/) and
[Stella](https://www.iseesystems.com/) models) into R objects to
construct [networks](http://igraph.org) (graph theory), ODE functions
for [deSolve](http://desolve.r-forge.r-project.org/) and
[STAN](https://mc-stan.org/) .
