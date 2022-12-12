test_that("transform_pars() returns the expected text", {

  unknown_pars <- list(list(par_name = "par_beta", min = 0),
                       list(par_name = "inv_phi", min = 0))

  actual_text <-  transform_pars(unknown_pars)

  expected_text <- paste(
    "  pars[[1]] <- exp(pars[[1]])",
    "  pars[[2]] <- exp(pars[[2]])", sep = "\n")

  expect_equal(actual_text, expected_text)

  unknown_pars <- list(list(par_name = "par_beta"),
                       list(par_name = "inv_phi"))

  actual_text <- transform_pars(unknown_pars)

  expected_text <- paste(
    "  pars[[1]] <- pars[[1]]",
    "  pars[[2]] <- pars[[2]]", sep = "\n")

  expect_equal(actual_text, expected_text)

  unknown_pars <- list(list(par_name = "par_beta"),
                       list(par_name = "inv_phi", min = 0))

  expected_text <- paste(
    "  pars[[1]] <- pars[[1]]",
    "  pars[[2]] <- exp(pars[[2]])", sep = "\n")

  actual_text <- transform_pars(unknown_pars)

  expect_equal(actual_text, expected_text)

  # Expit

  unknown_pars <- list(list(par_name = "par_beta", min = 0),
                       list(par_name = "par_rho", min = 0, max = 1),
                       list(par_name = "inv_phi", min = 0))

  actual_text <- transform_pars(unknown_pars)

  expected_text <- paste(
    "  pars[[1]] <- exp(pars[[1]])",
    "  pars[[2]] <- expit(pars[[2]])",
    "  pars[[3]] <- exp(pars[[3]])",
    sep = "\n")


  expect_equal(actual_text, expected_text)
})

# assign_pars_text ()-------------------------------------------------------------
test_that("assign_pars_text() returns the expected text", {

  unk_constants <- list(list(par_name = "par_beta", min = 0),
                        list(par_name = "par_rho", min = 0, max = 1))

  actual_text <- assign_pars_text(unk_constants)

  expected_text <- paste('  ds_inputs$consts["par_beta"] <- pars[[1]]',
                         '  ds_inputs$consts["par_rho"] <- pars[[2]]', sep = "\n")

  expect_equal(actual_text, expected_text)
})

# get_model_run_text() ---------------------------------------------------------

test_that("get_model_run_text returns the expected text", {
  sim_controls <- list(start        = 0,
                       stop         = 10,
                       step         = 0.25,
                       integ_method = "rk4")

  actual_text <- get_model_run_text(sim_controls)

  expected_text <- paste("simtime <- seq(0, 10, 0.25)",
                         paste("o <- deSolve::ode(",
                               "  y      = init_stocks,",
                               "  times  = simtime,",
                               "  func   = deSolve_components$func,",
                               "  parms  = consts,",
                               '  method = "rk4")',
                               "o_df <- data.frame(o)",
                               sep = "\n"),
                         sep = "\n")

  expect_equal(actual_text, expected_text)
})

# get_meas_model_text() --------------------------------------------------------

test_that("get_meas_model_text() returns the expected text", {

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  n_consts <- 1

  unknown_pars <- list(list(par_name = "par_beta", min = 0),
                       list(par_name = "inv_phi", min = 0, par_trans = "inv"))

  actual_text <- get_meas_model_text(meas_data_mdl, n_consts, unknown_pars, FALSE)

  expected_text <- paste(
    'sim_data_1 <- sd_net_change(o_df, "C")',
    "loglik_1   <- sum(dnbinom(data_1, mu = sim_data_1[, 'value'] + 1e-05, size = 1/pars[[2]], log = TRUE))",
    "loglik     <- loglik_1",
    "loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)
})

test_that("get_meas_model_text() returns a positive loglik", {
  mm1 <- list(stock_name = "S", stock_fit_type = "actual",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"))

  actual_text <- get_meas_model_text(list(mm1), TRUE)

  expected_text <- paste(
    "sim_data_1 <- dplyr::filter(o_df, time - trunc(time) == 0)",
    "loglik_1   <- sum(dpois(data_1, lambda = sim_data_1[, 'S'] + 1e-5, log = TRUE))",
    "loglik     <- loglik_1",
    "-loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)
})

test_that("get_meas_model_text() handles a known par in the measurement model", {
  mm1 <- list(stock_name = "S", stock_fit_type = "actual",
                      dist = list(name     = "dnorm",
                                  sim_data = "mean",
                                  known_par = list(sd = 1)))

  actual_text <- get_meas_model_text(list(mm1), FALSE)

  expected_text <- paste(
    "sim_data_1 <- dplyr::filter(o_df, time - trunc(time) == 0)",
    "loglik_1   <- sum(dnorm(data_1, mean = sim_data_1[, 'S'], sd = 1, log = TRUE))",
    "loglik     <- loglik_1",
    "loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)
})

test_that("get_meas_model_text() returns the expected text for the net change of a stock", {

  mm1 <- list(stock_name = "S", stock_fit_type = "net_change",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"))

  actual_text <- get_meas_model_text(list(mm1), FALSE)

  expected_text <- paste(
    'sim_data_1 <- sd_net_change(o_df, "S")',
    "loglik_1   <- sum(dpois(data_1, lambda = sim_data_1[, 'value'] + 1e-5, log = TRUE))",
    "loglik     <- loglik_1",
    "loglik", sep = "\n")

  expect_equal(actual_text, expected_text)

})

test_that("get_meas_model_text() returns the expected text for the net change of a stock", {

  mm1 <- list(stock_name = "S", stock_fit_type = "net_change",
                      dist = list(name     = "dnorm",
                                  sim_data = "mean",
                                  unknown  = list(name      = "sd",
                                                  par_trans = "log")))

  actual_text <- get_meas_model_text(fit_options = list(mm1),
                                     neg_log     = FALSE,
                                     n_unk_proc  = 1)

  expected_text <- paste(
    'sim_data_1 <- sd_net_change(o_df, "S")',
    "loglik_1   <- sum(dnorm(data_1, mean = sim_data_1[, 'value'], sd = pars[[2]], log = TRUE))",
    "loglik     <- loglik_1",
    "loglik", sep = "\n")

  expect_equal(actual_text, expected_text)

})

test_that("get_meas_model_text() handles multiple measurements", {

  mm1 <- list(stock_name = "Infected",
              stock_fit_type = "net_change",
              dist = list(name     = "dpois",
                          sim_data = "lambda",
                          dist_offset = "1e-5"),
              data = 1:10)

  mm2 <- list(stock_name = "Recovered",
              stock_fit_type = "net_change",
              dist = list(name     = "dpois",
                          sim_data = "lambda",
                          dist_offset = "1e-5"),
              data = 1:10)

  fit_options <- list(mm1, mm2)
  neg_log     <- FALSE

  actual_text <- get_meas_model_text(fit_options, neg_log)

  expected_text <-      paste(
    'sim_data_1 <- sd_net_change(o_df, "Infected")',
    "loglik_1   <- sum(dpois(data_1, lambda = sim_data_1[, 'value'] + 1e-5, log = TRUE))",
    'sim_data_2 <- sd_net_change(o_df, "Recovered")',
    "loglik_2   <- sum(dpois(data_2, lambda = sim_data_2[, 'value'] + 1e-5, log = TRUE))",
    'loglik     <- loglik_1 + loglik_2',
    'loglik', sep = "\n")

  expect_equal(actual_text, expected_text)
})

# sd_loglik_fun() --------------------------------------------------------------

test_that("sd_loglik_fun() returns the expected function", {

  filepath      <- system.file("models/", "SEIR.stmx", package = "readsdr")

  unknown_pars  <- list(list(par_name = "par_beta", min = 0))

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  fun_obj <- sd_loglik_fun(filepath, unknown_pars, meas_data_mdl, neg_log = FALSE,
                           start_time = 0, stop_time = 10, timestep = 1/32)

  actual_val <- fun_obj$fun(c(1, 0.1))

  expected_val <- -2315.852

  expect_equal(actual_val, expected_val, tolerance = 1e-4)

  fun_obj <- sd_loglik_fun(filepath, unknown_pars, meas_data_mdl,
                           neg_log = TRUE, start_time = 0, stop_time = 10,
                           timestep = 1/32)

  actual_val <- fun_obj$fun(c(1, 0.1))

  expected_val <- 2315.852

  expect_equal(actual_val, expected_val, tolerance = 1e-4)
})

# Multiple meas------------------------------------------------------------------
test_that("sd_loglik_fun() handles multiple measurements", {
  pars_df <- data.frame(name = "beta_var", type = "constant", par_trans = "log")

  filepath           <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl                <- read_xmile(filepath)
  deSolve_components <- mdl$deSolve_components

  sim_controls       <- list(start = 0, stop = 10, step = 0.25,
                             integ_method ="rk4")

  fit_options <- list(list(stock_name = "Infected",
                      stock_fit_type = "net_change",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"),
                      data = 1:10),
                      list(stock_name = "Recovered",
                           stock_fit_type = "net_change",
                           dist = list(name     = "dpois",
                                       sim_data = "lambda",
                                       dist_offset = "1e-5"),
                           data = 1:10))

  fun_obj  <- sd_loglik_fun(pars_df, deSolve_components,
                            sim_controls, fit_options)

  actual_fun <- fun_obj$fun

  test_gen <- function(deSolve_components, data) {
    init_stocks <- deSolve_components$stocks
    consts      <- deSolve_components$consts

    function(pars)
    {
      pars[[1]] <- exp(pars[[1]])
      consts["beta_var"] <- pars[[1]]
      simtime <- seq(0, 10, 0.25)
      o <- deSolve::ode(y = init_stocks, times = simtime, func = deSolve_components$func, parms = consts, method = "rk4")
      o_df <- data.frame(o)
      sim_data_1 <- sd_net_change(o_df, "Infected")
      loglik_1   <- sum(dpois(data_1, lambda = sim_data_1[, "value"] + 1e-05, log = TRUE))
      sim_data_2 <- sd_net_change(o_df, "Recovered")
      loglik_2   <- sum(dpois(data_2, lambda = sim_data_2[, "value"] + 1e-05, log = TRUE))
      loglik     <- loglik_1 + loglik_2
      loglik
    }
  }

  expected_fun <- test_gen(deSolve_components, fit_options$data)

  comparison_result <- all.equal(actual_fun, expected_fun,
                                 check.environment = FALSE)

  expect_equal(comparison_result, TRUE)
})
