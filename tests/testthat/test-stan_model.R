test_that("stan_model() returns the expected string", {

  m1      <- "y ~ normal(net_flow(C), tau)"
  meas_mdl <- list(m1)

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"),
    list(par_name = "tau", dist = "exponential", beta = 0.1, min = 0,
         type = "meas_par"))

  lvl_names <- c("S", "E", "I", "R", "C")

  actual <- stan_model(estimated_params, meas_mdl, lvl_names)

  expected <- paste(
    "model {",
    "  par_beta ~ lognormal(0, 1);",
    "  par_rho ~ beta(2, 2);",
    "  I0 ~ lognormal(0, 1);",
    "  tau ~ exponential(0.1);",
    "  y ~ normal(delta_x_1, tau);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("construct_prior_line() returns the expected string", {

  prior_obj <- list(par_name = "par_beta",
                    dist     = "lognormal",
                    mu       =  0,
                    sigma    =  1,
                    min      =  0,
                    type     =  "constant")

  actual   <- construct_prior_line(prior_obj)

  expected <- "  par_beta ~ lognormal(0, 1);"

  expect_equal(actual, expected)
})

test_that("construct_likelihood_line() returns the expected string", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"

  actual   <- construct_likelihood_line(meas_obj, 1)

  expected <- "  y ~ neg_binomial_2(delta_x_1, phi);"

  expect_equal(actual, expected)

  meas_obj <- "y ~ poisson(C)"

  lvl_names <- c("S", "E", "I", "R", "C")

  actual   <- construct_likelihood_line(meas_obj, 1, lvl_names)

  expected <- "  y ~ poisson(x[:, 5]);"

  expect_equal(actual, expected)
})
