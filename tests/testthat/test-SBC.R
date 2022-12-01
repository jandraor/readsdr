test_that("sd_data_generator_fun() returns the expected function", {

  meas_mdl <- list("y ~ poisson(net_flow(C))")

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  actual   <- sd_data_generator_fun(estimated_params, meas_mdl)

expected <- function() {

  par_beta <- prior_fun_list$par_beta()
  par_rho  <- prior_fun_list$par_rho()
  I0       <- prior_fun_list$I0()

  ds_inputs$consts[["par_beta"]] <- par_beta
  ds_inputs$consts[["par_rho"]]    <- par_rho

  N_val  <- ds_inputs$consts[["N"]]

  ds_inputs$stocks[["S"]] <- N_val - par_I0
  ds_inputs$stocks[["I"]] <- par_I0

  measurement_df <- sd_measurements(1, meas_mdl, ds_inputs, 0, 80,
                                    timestep = 1/32, integ_method = "rk4")

  n_obs <- nrow(measurement_df)

  list(
    variables = list(
      par_beta   = par_beta,
      par_rho    = par_rho,
      I0         = par_I0),
    generated =   list(
      n_obs    = n_obs,
      y        = measurement_df$measurement,
      n_params = 2,
      n_difeq  = 5,
      t0       = 0,
      ts       = 1:n_obs))
}

comparison_result <- all.equal(actual, expected, check.environment = FALSE)

expect_equal(comparison_result, TRUE)
})

test_that("prior_fun_factory() returns the expected list of functions", {

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  fun_list <- prior_fun_factory(estimated_params)

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
