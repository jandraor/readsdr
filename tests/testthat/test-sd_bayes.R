test_that("sd_Bayes() returns the expected file for a measured net flow", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual   <- sd_Bayes(filepath, meas_mdl, estimated_params)

  fileName <- "SEIR_nbinom.stan"
  expected <- readChar(fileName, file.info(fileName)$size)

  skip_on_os("windows")

  expect_equal(actual, expected)

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"),
    sd_prior("tau", "exponential", 0.2, "meas_par"))

  m1       <- "y ~ normal(net_flow(C), tau)"
  meas_mdl <- list(m1)

  actual <- sd_Bayes(filepath, meas_mdl, estimated_params)

  fileName <- "SEIR_normal.stan"
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

  skip_on_os("windows")
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

  skip_on_os("windows")
  expect_equal(actual, expected)

  # N is the data param

  mm1      <- "y ~ poisson(net_flow(C))"
  meas_mdl <- list(mm1)

  actual <- sd_Bayes(filepath = filepath,
                     meas_mdl = meas_mdl,
                     estimated_params = estimated_params,
                     data_params = c("N"))

  fileName <- "./test_stan_files/SEIR_pois_N.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  expect_equal(actual, expected)
})

test_that("sd_Bayes() returns the expected file with data inits defined", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)))

  actual <- sd_Bayes(filepath = filepath,
                     meas_mdl = meas_mdl,
                     estimated_params = estimated_params,
                     data_inits = "I0")

  fileName <- "./test_stan_files/SEIR_nbin_data_init.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  skip_on_os("windows")
  expect_equal(actual, expected)
})

test_that("sd_Bayes() returns the expected file for vectorised model", {

  filepath <- system.file("models/", "SEIR_age.stmx", package = "readsdr")

  ag <- c("A", "B", "C", "D") # age_groups

  measurements <- stringr::str_glue("y_{ag} ~ poisson(net_flow(C_{ag}))")
  meas_mdl     <- as.list(measurements)

  estimated_params <- list(
    sd_prior("k_AA", "normal", c(0, 10), min_0 = TRUE),
    sd_prior("par_rho", "beta", c(2, 2)))

  actual <- sd_Bayes(filepath = filepath,
                     meas_mdl = meas_mdl,
                     estimated_params = estimated_params)

  fileName <- "./test_stan_files/SEIR_age_pois.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  skip_on_os("windows")
  expect_equal(actual, expected)

  # nbin version

  measurements <- stringr::str_glue("y_{ag} ~ neg_binomial_2(net_flow(C_{ag}), phi)")
  meas_mdl     <- as.list(measurements)

  actual <- sd_Bayes(filepath = filepath,
                     meas_mdl = meas_mdl,
                     estimated_params = estimated_params)

  fileName <- "./test_stan_files/SEIR_age_nbin.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  expect_equal(actual, expected)
})

test_that("sd_Bayes() allows users to override delay metaparameters", {

  filepath <- system.file("models/", "SEjIkR.stmx", package = "readsdr")

  meas_mdl   <- list("y ~ poisson(net_flow(C))")

  estimated_params <- list(sd_prior("par_beta", "lognormal", c(0, 1)),
                           sd_prior("par_rho", "beta", c(2,2)),
                           sd_prior("I0", "lognormal", c(0, 1), "init"))

  const_list <- list(j = 3, k = 3)

  actual <- sd_Bayes(filepath, meas_mdl, estimated_params,
                     const_list = const_list)

  fileName <- "./test_stan_files/SE3I3R_pois.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  skip_on_os("windows")
  expect_equal(actual, expected)
})

test_that("sd_Bayes() handles the LV model", {

  filepath <- system.file("models/", "LV.stmx", package = "readsdr")

  est_pars <- list(sd_prior("par_alpha", "normal", c(1, 0.5), min_0 = TRUE),
                   sd_prior("par_gamma", "normal", c(1, 0.5), min_0 = TRUE),
                   sd_prior("par_beta", "normal", c(0.5, 0.5), min_0 = TRUE),
                   sd_prior("par_delta", "normal", c(0.5, 0.5), min_0 = TRUE),
                   sd_prior("sigma_1", "lognormal", c(-1, 1)),
                   sd_prior("sigma_2", "lognormal", c(-1, 1)),
                   sd_prior("H0", "lognormal", c(log(10), 1), type = "init"),
                   sd_prior("L0", "lognormal", c(log(10), 1), type = "init"))

  meas_mdl <- list("y ~ lognormal(log(Hares), sigma_1)",
                   "z ~ lognormal(log(Lynx), sigma_2)",
                   "y0 ~ lognormal(log(Hares[0]), sigma_1)",
                   "z0 ~ lognormal(log(Lynx[0]), sigma_1)")

  actual <- sd_Bayes(filepath, estimated_params = est_pars, meas_mdl,
                     forecast = TRUE)

  fileName <- "./test_stan_files/LV.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  skip_on_os("windows")
  expect_equal(actual, expected)
})

test_that("sd_Bayes() checks the prior of a normal distribution", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  mm1      <- "y ~ normal(net_flow(C), tau)"
  meas_mdl <- list(mm1)

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  expect_error(sd_Bayes(filepath, meas_mdl, estimated_params),
               "Please add a prior for `tau`.")
})
