#------------------stan_params()------------------------------------------------

test_that("stan_params() returns the expected string", {

  prior <- list(sd_prior("par_beta", "lognormal", c(0, 1)),
                sd_prior("par_rho", "beta", c(2, 2)),
                sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual <- stan_params(prior)

  expected <- paste(
    "parameters {",
    "  real<lower = 0> par_beta;",
    "  real<lower = 0, upper = 1> par_rho;",
    "  real<lower = 0> I0;",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_params() allows constraining parameters based on the prior", {

  actual <- stan_params(list(list(par_name = "par_alpha",
                             dist     = "normal",
                             mu       = 0,
                             sigma    = 1,
                             type     = "constant",
                             min      = 0)))

  expected <- paste(
    "parameters {",
    "  real<lower = 0> par_alpha;",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_params() allows constraining parameters based on other parameters", {

  prior_list <- list(sd_prior("par_alpha", "beta", c(2, 2)),
                     sd_prior("par_beta", "beta", c(2, 2), max = "par_alpha"))

  actual <- stan_params(prior_list)

  expected <- paste(
    "parameters {",
    "  real<lower = 0, upper = 1> par_alpha;",
    "  real<lower = 0, upper = par_alpha> par_beta;",
    "}", sep = "\n")

  expect_equal(actual, expected)

  # Test 2

  prior_list <- list(sd_prior("par_beta", "beta", c(2, 2), max = "par_alpha"),
                     sd_prior("par_alpha", "beta", c(2, 2)))

  actual <- stan_params(prior_list)

  expected <- paste(
    "parameters {",
    "  real<lower = 0, upper = 1> par_alpha;",
    "  real<lower = 0, upper = par_alpha> par_beta;",
    "}", sep = "\n")

  expect_equal(actual, expected)
})
