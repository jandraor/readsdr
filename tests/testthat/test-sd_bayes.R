test_that("sd_Bayes() returns the expected file", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  prior <- list(
    sd_prior("par_beta", "lognormal", 0, 1),
    sd_prior("par_rho", "beta", 2, 2),
    sd_prior("I", "lognormal", 0, 1))

  actual   <- sd_Bayes(filepath, prior, meas_mdl)

  fileName <- "SEIR_nbinom.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  expect_equal(actual, expected)

})
