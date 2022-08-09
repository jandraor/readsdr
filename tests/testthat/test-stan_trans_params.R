#------------------stan_trans_params()------------------------------------------

test_that("stan_trans_params() returns the expected string", {

  prior <- list(sd_prior("par_beta", "lognormal", c(0, 1)),
                sd_prior("par_rho", "beta", c(2, 2)),
                sd_prior("I0", "lognormal", c(0, 1), "init"))

  filepath        <- system.file("models/", "SEIR.stmx", package = "readsdr")
  model_structure <- extract_structure_from_XMILE(filepath)

  actual <- stan_trans_params(prior, model_structure$levels, TRUE)

  expected <-  paste(
    "transformed parameters{",
    "  array[n_obs] vector[n_difeq] x; // Output from the ODE solver",
    "  array[n_params] real params;",
    "  vector[n_difeq] x0; // init values",
    "  array[n_obs] real delta_x;",
    "  real phi;",
    "  //assignments",
    "  phi = 1 / inv_phi;",
    "  x0[1] = 10000 - 1 * I0; // S",
    "  x0[2] = 0; // E",
    "  x0[3] = I0; // I",
    "  x0[4] = 0; // R",
    "  x0[5] = I0; // C",
    "  params[1] = par_beta;",
    "  params[2] = par_rho;",
    "  x = ode_rk45(X_model, x0, t0, ts, params);",
    "  delta_x[1] =  x[1, 5]  - x0[5];",
    "  for (i in 1:n_obs-1) {",
    "    delta_x[i + 1] = x[i + 1, 5] - x[i, 5] + 1e-5;",
    "  }",
    "}", sep = "\n")

  expect_equal(actual, expected)

})

test_that("construct_pars_asg() ignores inits & meas pars", {

  prior <- list(list(par_name = "par_beta", type = "constant"),
                list(par_name = "par_rho", type = "constant"),
                list(par_name = "I0", type = "init"),
                list(par_name = "inv_phi", type = "meas_par"))

  actual   <- construct_pars_asg(prior)
  expected <- paste("  params[1] = par_beta;",
                    "  params[2] = par_rho;", sep = "\n")

  expect_equal(actual, expected)
})

test_that("extract_par_trans() returns the expected list", {

  prior <- list(list(par_name = "par_beta", type = "constant"),
                list(par_name = "par_rho", type = "constant"),
                list(par_name = "I0", type = "init"),
                list(par_name = "inv_phi", type = "meas_par",
                     par_trans = "inv"))

  actual <- extract_par_trans(prior)

  expected <- list(list(decl  = "  real phi;",
                        trans = "  phi = 1 / inv_phi;"))

  expect_equal(actual, expected)
})
