test_that("sd_data_generator_fun() returns the expected function", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  meas_mdl <- list("y ~ poisson(net_flow(C))")

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual_fun  <- sd_data_generator_fun(filepath, estimated_params, meas_mdl,
                                       start_time = 0, stop_time = 10,
                                       timestep = 1/32, integ_method = "rk4")

  set.seed(200)
  actual_list <- actual_fun()

  expected <- list(
    variables = list(
      par_beta   = 1.088452,
      par_rho    = 0.5636847,
      I0         = 1.541193),
    generated =   list(
      y        = c(0, 0, 0, 0, 1, 1, 0, 2, 2, 2),
      n_obs    = 10,
      n_params = 2,
      n_difeq  = 5,
      t0       = 0,
      ts       = 1:10))

  expect_equal(actual_list, expected, tolerance = 1e-6)

  # Negative Binomial test
  meas_mdl   <- list("y ~ neg_binomial_2(net_flow(C), phi)")

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual_fun  <- sd_data_generator_fun(filepath, estimated_params, meas_mdl,
                                       start_time = 0, stop_time = 10,
                                       timestep = 1/32, integ_method = "rk4")

  set.seed(300)
  actual_list <- actual_fun()

  expected <- list(
    variables = list(
      par_beta   = 3.950297,
      par_rho    = 0.7321697,
      I0         = 1.605586,
      inv_phi    = 0.1033722),
    generated =   list(
      y        = c(3, 3, 15, 19, 71, 155, 332, 193, 803, 2453),
      n_obs    = 10,
      n_params = 2,
      n_difeq  = 5,
      t0       = 0,
      ts       = 1:10))

  expect_equal(actual_list, expected, tolerance = 1e-6)
})

test_that("sd_data_generator_fun() returns the expected function for a vectorised model", {

  filepath <- system.file("models/", "SEIR_age.stmx", package = "readsdr")

  ag <- c("A", "B", "C", "D") # age_groups

  measurements <- stringr::str_glue("y_{ag} ~ poisson(net_flow(C_{ag}))")
  meas_mdl     <- as.list(measurements)

  estimated_params <- list(
    sd_prior("k_AA", "normal", c(0, 10), min_0 = TRUE),
    sd_prior("par_rho", "beta", c(2, 2)))

  actual_fun  <- sd_data_generator_fun(filepath, estimated_params, meas_mdl,
                                       start_time = 0, stop_time = 10,
                                       timestep = 1/32, integ_method = "rk4")

  set.seed(111)
  actual_list <- actual_fun()

  expected <- list(
    variables = list(
      k_AA    = 2.352207,
      par_rho = 0.4073203),
    generated =   list(
      y_A      = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      y_B      = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
      y_C      = c(1, 0, 0, 0, 1, 0, 0, 0, 0, 1),
      y_D      = c(0, 0, 0, 0, 0, 1, 0, 0, 1, 2),
      n_obs    = 10,
      n_params = 2,
      n_difeq  = 20,
      t0       = 0,
      ts       = 1:10))

  expect_equal(actual_list, expected, tolerance = 1e-6)

})

test_that("prior_fun_factory() returns the expected list of functions", {

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  fun_list <- prior_fun_factory(estimated_params, 1)

  set.seed(123)

  val1 <- fun_list[[1]]()
  val2 <- fun_list[[2]]()
  val3 <- fun_list[[3]]()

  actual <- c(val1, val2, val3)

  factory1 <- function() function() rlnorm(1)
  factory2 <- function() function() rbeta(1, 2, 2)

  fun1 <- factory1()
  fun2 <- factory2()
  fun3 <- factory1()

  set.seed(123)

  val1 <- fun1()
  val2 <- fun2()
  val3 <- fun3()

  expected <- c(val1, val2, val3)

  expect_equal(actual, expected)
})
