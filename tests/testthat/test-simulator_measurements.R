test_that("sd_measurements() returns the expected data frame", {

  filepath       <- system.file("models/", "SEIR.stmx", package = "readsdr")
  mdl            <- read_xmile(filepath)

  set.seed(123)

  mm1      <- "y ~ poisson(C)"
  meas_mdl <- list(mm1)

  actual <- sd_measurements(n_meas       = 2,
                            meas_model   = meas_mdl,
                            ds_inputs    = mdl$deSolve_components,
                            start_time   = 0,
                            stop_time    = 10,
                            timestep     = 1/16,
                            integ_method = "rk4")

  output <- sd_simulate(mdl$deSolve_components, start_time = 0,
                        stop_time = 10, timestep = 1/16,
                        integ_method = "rk4")

  output <- output[output$time %in% 0:10, ]

  set.seed(123)

  y1 <- rpois(n = nrow(output), lambda = output$C)
  y2 <- rpois(n = nrow(output), lambda = output$C)

  expected <- data.frame(iter        = rep(1:2, each = 11),
                         time        = rep(0:10, 2),
                         var_name    = "y",
                         measurement = c(y1, y2))

  expect_equal(actual, expected)

  # Nbin

  set.seed(321)

  mm1      <- "y ~ neg_binomial_2(C, 3)"
  meas_mdl <- list(mm1)

  actual <- sd_measurements(n_meas       = 2,
                            meas_model   = meas_mdl,
                            ds_inputs    = mdl$deSolve_components,
                            start_time   = 0,
                            stop_time    = 10,
                            timestep     = 1/16,
                            integ_method = "rk4")

  set.seed(321)

  y1 <- rnbinom(n = nrow(output), mu = output$C, size = 3)
  y2 <- rnbinom(n = nrow(output), mu = output$C, size = 3)

  expected <- data.frame(iter        = rep(1:2, each = 11),
                         time        = rep(0:10, 2),
                         var_name    = "y",
                         measurement = c(y1, y2))

  expect_equal(actual, expected)
})

test_that("sd_measurements() returns the expected data frame for the measurements of a stock's net change", {

  filepath       <- system.file("models/", "SEIR.stmx", package = "readsdr")
  mdl            <- read_xmile(filepath)

  set.seed(123)

  mm1      <- "y ~ poisson(net_flow(C))"
  meas_mdl <- list(mm1)

  actual <- sd_measurements(n_meas       = 2,
                            meas_model   = meas_mdl,
                            ds_inputs    = mdl$deSolve_components,
                            start_time   = 0,
                            stop_time    = 10,
                            timestep     = 1/16,
                            integ_method = "rk4")

  output          <- sd_simulate(mdl$deSolve_components,
                                 start_time = 0,
                                 stop_time = 10, timestep = 1/16,
                                 integ_method = "rk4")

  net_change_df <- sd_net_change(output, "C")

  set.seed(123)

  y1 <- rpois(n = nrow(net_change_df), lambda = net_change_df$value)
  y2 <- rpois(n = nrow(net_change_df), lambda = net_change_df$value)

  expected <- data.frame(iter        = rep(1:2, each = 10),
                         time        = rep(1:10, 2),
                         var_name    = "y",
                         measurement = c(y1, y2))

  expect_equal(actual, expected)

  set.seed(321)

  mm1      <- "y ~ neg_binomial_2(net_flow(C), 0.1)"
  meas_mdl <- list(mm1)

  actual <- sd_measurements(n_meas       = 2,
                            meas_model   = meas_mdl,
                            ds_inputs    = mdl$deSolve_components,
                            start_time   = 0,
                            stop_time    = 10,
                            timestep     = 1/16,
                            integ_method = "rk4")

  set.seed(321)

  y1 <- rnbinom(n = nrow(net_change_df), mu = net_change_df$value, size = 0.1)
  y2 <- rnbinom(n = nrow(net_change_df), mu = net_change_df$value, size = 0.1)

  expected <- data.frame(iter        = rep(1:2, each = 10),
                         time        = rep(1:10, 2),
                         var_name    = "y",
                         measurement = c(y1, y2))

  expect_equal(actual, expected)
})


test_that("sampling_statement_to_list() returns the expected list", {

  sampling_statement <- "y ~ poisson(C)"

  actual   <- sampling_statement_to_list(sampling_statement)

  expected <- list(meas_type  = "as_is",
                   meas_name  = "y",
                   dist       = list(dist_name = "rpois",
                                     lambda    = "C"))

  expect_equal(actual, expected)

  sampling_statement <- "y ~ poisson(net_flow(C))"
  actual             <- sampling_statement_to_list(sampling_statement)

  expected <- list(meas_type  = "net_flow",
                   meas_name  = "y",
                   dist       = list(dist_name = "rpois",
                                     lambda    = "net_flow(C)"))

  expect_equal(actual, expected)
})
