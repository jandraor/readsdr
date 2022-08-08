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
