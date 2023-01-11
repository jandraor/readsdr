test_that("prior_checks() returns the expected list", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  meas_mdl   <- list("y ~ neg_binomial_2(net_flow(C), phi)")

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"))

  set.seed(666)

  actual <- sd_prior_checks(filepath, meas_mdl, estimated_params, n_draws = 2,
                            start_time = 0, stop_time = 5,
                            integ_method = "rk4", timestep = 1/32)

  df1 <- data.frame(iter     = c(1,2),
                    par_beta = c(2.124021, 7.495888),
                    par_rho  = c(0.4005852, 0.9374636),
                    I0       = c(0.1089491, 2.1348496),
                    inv_phi  = c(0.5222716, 0.3047205))

  df2 <- data.frame(iter        = rep(c(1, 2), each = 5),
                    time        = rep(1:5, 2),
                    var_name    = "y",
                    measurement = c(0, 0, 0, 0, 2, 5, 27, 30, 406, 544))

  expected <- list(parameters   = df1,
                   measurements = df2)

  expect_equal(actual, expected, tolerance = 1e-7)
})
