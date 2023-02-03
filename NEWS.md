# Development version

* Support the translation of Stella's *DELAYN* with four parameters.
* Support Stella's *apply all* for uni-dimensional vectors.
* Add **sd_Bayes()**.
* Deprecate **stan_data()** & **stan_transformed_data()**
* Add **sd_measurements()**.
* Support the translation of *RANDOM NORMAL* from Vensim.
* Add support to bidimensional vectors in Vensim.
* read_xmile() returns the element graph only if the parameter *graph* is set to *TRUE*.
* read_xmile() now translates Vensim's *DELAY FIXED*.
* Add sd_what_if_from_time()
* Add sd_impact_inputs()
* Add sd_loglik_fun()
* Add sd_net_change()
* Fix bug (incorrect division due to missing parentheses) in the translation of *SMOOTH*.

# 0.2.0

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
