test_that("sd_prior() returns the expected list", {

  actual <- sd_prior("par_beta", "lognormal", c(0, 1))

  expected <- list(par_name = "par_beta",
                   dist     = "lognormal",
                   mu       = 0,
                   sigma    = 1,
                   min      = 0,
                   type     = "constant")

  expect_equal(actual, expected)
})


