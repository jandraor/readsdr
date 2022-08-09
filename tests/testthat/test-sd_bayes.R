test_that("sd_Bayes() returns the expected file", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  prior <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual   <- sd_Bayes(filepath, prior, meas_mdl)

  fileName <- "SEIR_nbinom.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  expect_equal(actual, expected)

})

test_that("extract_extra_prior() returns the expected list", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual   <- extract_extra_prior(meas_obj)

  expected <- list(par_name  = "inv_phi",
                   dist      = "exponential",
                   beta      = 5,
                   min       = 0,
                   type      = "meas_par",
                   par_trans = "inv")

  expect_equal(actual, expected)
})
