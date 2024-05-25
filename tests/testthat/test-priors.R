test_that("sd_prior() returns the expected list", {

  actual <- sd_prior("par_beta", "lognormal", c(0, 1))

  expected <- list(par_name = "par_beta",
                   dist     = "lognormal",
                   type     = "constant",
                   mu       = 0,
                   sigma    = 1,
                   min      = 0)

  expect_equal(actual, expected)

  actual <- sd_prior("par_rho", "normal", c(0, 1), min = 0)

  expected <- list(par_name = "par_rho",
                   dist = "normal",
                   type = "constant",
                   mu = 0,
                   sigma = 1,
                   min = 0)

  expect_equal(actual, expected)
})


