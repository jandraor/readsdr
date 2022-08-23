test_that("sd_Bayes() returns the expected file for a measured net flow", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  prior <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual   <- sd_Bayes(filepath, meas_mdl, prior)

  fileName <- "SEIR_nbinom.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  expect_equal(actual, expected)

})

test_that("sd_Bayes() returns the expected file for a measured stock", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  m1      <- "y ~ poisson(C)"
  meas_mdl <- list(m1)

  actual <- sd_Bayes(filepath, meas_mdl, estimated_params)

  fileName <- "SEIR_C_meas.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  expect_equal(actual, expected)
})

test_that("sd_Bayes() returns the expected file with data parameters defined", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual <- sd_Bayes(filepath = filepath,
                     meas_mdl = meas_mdl,
                     estimated_params = estimated_params,
                     data_params = c("par_gamma"))

  fileName <- "SEIR_nbinom_data_param.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  expect_equal(actual, expected)
})

test_that("extract_extra_params() returns the expected list", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual   <- extract_extra_params(meas_obj)

  expected <- list(par_name  = "inv_phi",
                   dist      = "exponential",
                   beta      = 5,
                   min       = 0,
                   type      = "meas_par",
                   par_trans = "inv")

  expect_equal(actual, expected)
})
