# readsdr 0.3.0

## Breaking changes

* Deprecate **stan_data()** & **stan_transformed_data()**.

## New features

* Add Maryland data.

* Add **sd_posterior_fun()**.

* Add **sd_Bayes()**.

* Support the translation of Vensim's *DELAY_N*.

* Support the translation of Stella's *DELAYN* with four parameters.

* Add **sd_measurements()**.

* Support the translation of *RANDOM NORMAL* from Vensim.

* Add support to bidimensional vectors in Vensim.

* *read_xmile()* now translates Vensim's *DELAY FIXED*.

* Add **sd_what_if_from_time()**

* Add **sd_impact_inputs()**

* Add **sd_loglik_fun()**

* Add **sd_net_change()**

## Minor improvements and fixes

* Fix bug in the translation of graphical functions.

* Parallelisation of *sd_sensitivity_run()* supported in Windows.

* Support Stella's *apply all* for uni-dimensional vectors.

* *read_xmile()* returns the element graph only if the argument *graph* is set to *TRUE*.

* Fix bug (incorrect division due to missing parentheses) in the translation of *SMOOTH*.

# readsdr 0.2.0

* Add support to translation of SMOOTH functions from Vensim & Stella.
* Add sd_sensitivity_run(). This function supports parallelisation in 
  Unix-based systems.
* Add flexibility to read_xmile() by allowing the user to override values of
  constants and initial values of stocks.
* Add sd_constants() and sd_stocks() to summarise model's information in data frames. 
* Add stan_ode_function()
* Add stan_data()
* Add support to unidimensional vectors in Stella.
* Support the translation of the following math functions: ABS & SQRT.
