test_that("sd_posterior_fun() returns the expected function", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual_obj <- sd_posterior_fun(filepath, meas_data_mdl, estimated_params)

  actual_fun <- actual_obj$fun

  actual_val <- actual_fun(c(log(1), logit(0.75), log(1), log(3)))

  expected_val <- -302.0369419999466345

  expect_equal(actual_val, expected_val)
})

test_that("log_prior_fun_generator() returns the expected function", {

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  log_prior_fun <- log_prior_fun_generator(estimated_params, meas_data_mdl)

  actual_val <- log_prior_fun(c(log(1), logit(0.75), log(1), log(3)))

  expected_val <- dlnorm(1, 0, 1, log = TRUE) + dbeta(0.75, 2, 2, log = TRUE) +
    dlnorm(1, 0, 1, log = TRUE) + dexp(1/3, rate = 5, log = TRUE)

  expect_equal(actual_val, expected_val)
})

test_that("build_prior_expr() returns the expected string", {

  par_obj <- list(par_name  = "par_beta",
                  dist      = "lognormal",
                  mu        = 0,
                  sigma     = 1,
                  par_trans = "exp",
                  index     = 1)

  actual <- build_prior_expr(par_obj)

  expected <- "  dlnorm(exp(pars[[1]]), meanlog = 0, sdlog = 1, log = TRUE)"

  expect_equal(actual, expected)

})

test_that("constrain_pars() returns the expected string", {

  par_obj <- list(par_trans = "exp",
                  index     = 1)

  actual <- constrain_pars(par_obj)

  expected <- "exp(pars[[1]])"

  expect_equal(actual, expected)


  par_obj <- list(par_trans = c("exp", "inv"),
                  index     = 4)

  actual <- constrain_pars(par_obj)

  expected <- "inv(exp(pars[[4]]))"

  expect_equal(actual, expected)
})
