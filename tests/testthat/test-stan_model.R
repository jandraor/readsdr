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
