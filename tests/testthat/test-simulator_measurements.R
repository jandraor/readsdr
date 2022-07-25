test_that("sd_measurements() returns the expected data frame", {

  filepath       <- system.file("models/", "SEIR.stmx", package = "readsdr")
  mdl            <- read_xmile(filepath)

  set.seed(123)

  mm1 <- list(stock_name = "C",
              meas_type  = "as_is",
              dist       = list(name       = "rnbinom",
                                stock_par  = "mu",
                                extra_pars = list(size = 0.1)))

  meas_model <- list(mm1)

  actual <- sd_measurements(n_meas       = 2,
                            meas_model   = meas_model,
                            ds_inputs    = mdl$deSolve_components,
                            start_time   = 0,
                            stop_time    = 10,
                            timestep     = 1/16,
                            integ_method = "rk4")

  filepath       <- system.file("models/", "SEIR.stmx", package = "readsdr")
  mdl            <- read_xmile(filepath)

  output          <- sd_simulate(mdl$deSolve_components, start_time = 0,
                                 stop_time = 10, timestep = 1/16,
                                 integ_method = "rk4")

  output <- output[output$time %in% 0:10, ]

  set.seed(123)

  y1 <- rnbinom(n = nrow(output), mu = output$C, size = 0.1)
  y2 <- rnbinom(n = nrow(output), mu = output$C, size = 0.1)

  expected <- data.frame(iter        = rep(1:2, each = 11),
                         time        = rep(0:10, 2),
                         var_name    = "C",
                         measurement = c(y1, y2))

  expect_equal(actual, expected)
})

test_that("sd_measurements() returns the expected data frame for the measurements of a stock's net change", {

  filepath       <- system.file("models/", "SEIR.stmx", package = "readsdr")
  mdl            <- read_xmile(filepath)

  set.seed(123)

  mm1 <- list(stock_name = "C",
              meas_type  = "net_change",
              dist       = list(name       = "rnbinom",
                                stock_par  = "mu",
                                extra_pars = list(size = 0.1)))

  meas_model <- list(mm1)

  actual <- sd_measurements(n_meas       = 2,
                            meas_model   = meas_model,
                            ds_inputs    = mdl$deSolve_components,
                            start_time   = 0,
                            stop_time    = 10,
                            timestep     = 1/16,
                            integ_method = "rk4")

  filepath       <- system.file("models/", "SEIR.stmx", package = "readsdr")
  mdl            <- read_xmile(filepath)

  output          <- sd_simulate(mdl$deSolve_components,
                                 start_time = 0,
                                 stop_time = 10, timestep = 1/16,
                                 integ_method = "rk4")

  net_change_df <- sd_net_change(output, "C")

  set.seed(123)

  y1 <- rnbinom(n = nrow(net_change_df), mu = net_change_df$value, size = 0.1)
  y2 <- rnbinom(n = nrow(net_change_df), mu = net_change_df$value, size = 0.1)

  expected <- data.frame(iter        = rep(1:2, each = 10),
                         time        = rep(1:10, 2),
                         var_name    = "delta_C",
                         measurement = c(y1, y2))

  expect_equal(actual, expected)

})
