test_that("sd_posterior_fun() returns the expected function", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual_fun <- sd_posterior_fun(filepath, meas_mdl, estimated_params)

  actual_val <- actual_fun(log(1), logit(0.75), log(1), log(3))

  expected_val <- 1

  expect_equal(actual_val, expected)
})

test_that("log_prior_fun_generator() returns the expected function", {

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  log_prior_fun <- log_prior_fun_generator(estimated_params)

  actual_val <- actual_fun(exp(1), expit(0.75), exp(1), exp(3))

  expected_val <- dlnorm(1, 0, 1, log = TRUE) + dbeta(0.75, 2, 2, log = TRUE) +
    dlnorm(1, 0, 1, log = TRUE)

  expect_equal(actual_val, expected_val)
})
