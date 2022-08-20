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
