test_that("transform_pars() returns the expected text when there are no trans", {

  expected_text <- paste(
    "pars[[1]] <- pars[[1]]",
    "pars[[2]] <- pars[[2]]", sep = "\n")

  pars_df <- data.frame(name      = c("alpha", "S"),
                        type      = c("constant", "stock"))

  actual_text <- transform_pars(pars_df)

  expect_equal(actual_text, expected_text)
})

test_that("transform_pars() returns the expected text for log trans", {

  pars_df <- data.frame(name      = c("alpha", "S"),
                        type      = c("constant", "stock"),
                        par_trans = c("", "log"))

  expected_text <- paste(
    "pars[[1]] <- pars[[1]]",
    "pars[[2]] <- exp(pars[[2]])", sep = "\n")

  actual_text <- transform_pars(pars_df)

  expect_equal(actual_text, expected_text)
})

test_that("transform_pars() returns the expected text for logit trans", {

  pars_df <- data.frame(name      = c("alpha", "S"),
                        type      = c("constant", "stock"),
                        par_trans = c("", "logit"))

  expected_text <- paste(
    "pars[[1]] <- pars[[1]]",
    "pars[[2]] <- expit(pars[[2]])", sep = "\n")

  actual_text <- transform_pars(pars_df)

  expect_equal(actual_text, expected_text)
})

test_that("transform_pars() returns the expected text for mixed trans", {

  pars_df <- data.frame(name      = c("alpha", "S"),
                        type      = c("constant", "stock"),
                        par_trans = c("logit", "log"))

  expected_text <- paste(
    "pars[[1]] <- expit(pars[[1]])",
    "pars[[2]] <- exp(pars[[2]])", sep = "\n")

  actual_text <- transform_pars(pars_df)

  expect_equal(actual_text, expected_text)
})

# assign_pars_text -------------------------------------------------------------
test_that("assign_pars_text() returns the expected text", {

  expected_text <- paste('consts["alpha"] <- pars[[1]]',
                         'init_stocks["S"] <- pars[[2]]', sep = "\n")

  pars_df <- data.frame(name      = c("alpha", "S"),
                        type      = c("constant", "stock"),
                        pos       = 1:2)

  actual_text <- assign_pars_text(pars_df)

  expect_equal(actual_text, expected_text)
})

test_that("assign_pars_text() handles stocks that share common init values", {
  expected_text <- paste('consts["alpha"] <- pars[[1]]',
                         'init_stocks["S"] <- pars[[2]]',
                         'init_stocks["S2"] <- pars[[2]]', sep = "\n")

  pars_df <- data.frame(name      = c("alpha", "S"),
                        type      = c("constant", "stock"),
                        pos       = 1:2)

  extra_stocks <- list(list(name = "S2", init = "S"))

  actual_text <- assign_pars_text(pars_df, extra_stocks)

  expect_equal(actual_text, expected_text)

  expected_text <- paste('consts["alpha"] <- pars[[1]]',
                         'init_stocks["S"] <- pars[[2]]',
                         'init_stocks["S2"] <- pars[[2]]',
                         'init_stocks["S3"] <- 1000 - pars[[2]]', sep = "\n")

  pars_df <- data.frame(name      = c("alpha", "S"),
                        type      = c("constant", "stock"),
                        pos       = 1:2)

  extra_stocks <- list(list(name = "S2", init = "S"),
                       list(name = "S3", init = "1000 - S"))

  actual_text <- assign_pars_text(pars_df, extra_stocks)

  expect_equal(actual_text, expected_text)
})



test_that("assign_pars_text() ignores measurement model's pars", {
  pars_df <- data.frame(name      = c("beta_var", "sd"),
                        type      = c("constant", "par_measure"),
                        par_trans = c("log", "log"))

  actual_text <- assign_pars_text(pars_df)

  expected_text <- 'consts["beta_var"] <- pars[[1]]'

  expect_equal(actual_text, expected_text)

  pars_df <- data.frame(name      = c("sd", "beta_var"),
                        type      = c("par_measure", "constant"),
                        par_trans = c("log", "log"))

  actual_text   <- assign_pars_text(pars_df)
  expected_text <- 'consts["beta_var"] <- pars[[1]]'

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

test_that("get_meas_model_text() returns the expected text", {
  fit_options <- list(stock_name = "S", stock_fit_type = "actual",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"))

  actual_text <- get_meas_model_text(fit_options, FALSE)

  expected_text <- paste(
    "sim_data <- dplyr::filter(o_df, time - trunc(time) == 0)",
    "loglik   <- sum(dpois(data, lambda = sim_data[, 'S'] + 1e-5, log = TRUE))",
    "loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)

})

# get_meas_model_text() --------------------------------------------------------

test_that("get_meas_model_text() returns a positive loglik", {
  fit_options <- list(stock_name = "S", stock_fit_type = "actual",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"))

  actual_text <- get_meas_model_text(fit_options, TRUE)

  expected_text <- paste(
    "sim_data <- dplyr::filter(o_df, time - trunc(time) == 0)",
    "loglik   <- sum(dpois(data, lambda = sim_data[, 'S'] + 1e-5, log = TRUE))",
    "-loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)
})

test_that("get_meas_model_text() handles a known par in the measurement model", {
  fit_options <- list(stock_name = "S", stock_fit_type = "actual",
                      dist = list(name     = "dnorm",
                                  sim_data = "mean",
                                  known_par = list(sd = 1)))

  actual_text <- get_meas_model_text(fit_options, FALSE)

  expected_text <- paste(
    "sim_data <- dplyr::filter(o_df, time - trunc(time) == 0)",
    "loglik   <- sum(dnorm(data, mean = sim_data[, 'S'], sd = 1, log = TRUE))",
    "loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)
})

test_that("get_meas_model_text() returns the expected text for the net change of a stock", {

  fit_options <- list(stock_name = "S", stock_fit_type = "net_change",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"))

  actual_text <- get_meas_model_text(fit_options, FALSE)

  expected_text <- paste(
    'sim_data <- sd_net_change(o_df, "S")',
    "loglik   <- sum(dpois(data, lambda = sim_data[, 'value'] + 1e-5, log = TRUE))",
    "loglik", sep = "\n")

  expect_equal(actual_text, expected_text)

})

test_that("get_meas_model_text() returns the expected text for the net change of a stock", {

  fit_options <- list(stock_name = "S", stock_fit_type = "net_change",
                      dist = list(name     = "dnorm",
                                  sim_data = "mean"))

  unknown_par <- c(name = "sd", type = "par_measure", par_trans = "log",
                      pos  = "2")

  actual_text <- get_meas_model_text(fit_options, FALSE,
                                     unknown_par = unknown_par)

  expected_text <- paste(
    'sim_data <- sd_net_change(o_df, "S")',
    "loglik   <- sum(dnorm(data, mean = sim_data[, 'value'], sd = pars[[2]], log = TRUE))",
    "loglik", sep = "\n")

  expect_equal(actual_text, expected_text)

})

# get_constraint_text() --------------------------------------------------------

test_that("get_constraint_text() returns the expected text", {
  pars_df <- data.frame(name = c("I", "R"),
                        type = c("stock", "stock"))

  extra_constraints <- list("I + R < 1000")

  actual_text   <- get_constraint_text(extra_constraints, pars_df)
  expected_text <- "if(pars[[1]] + pars[[2]] < 1000) return(-Inf)"

  expect_equal(actual_text, expected_text)

})

test_that("sd_loglik_fun() returns the expected function", {

  pars_df <- data.frame(name = "beta_var", type = "constant", par_trans = "log")

  filepath           <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl                <- read_xmile(filepath)
  deSolve_components <- mdl$deSolve_components

  sim_controls       <- list(start = 0, stop = 10, step = 0.25,
                             integ_method ="rk4")

  fit_options <- list(stock_name = "Infected", stock_fit_type = "net_change",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"),
                      data = 1:10)

  actual_fun <- sd_loglik_fun(pars_df, deSolve_components,
                              sim_controls, fit_options)

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
    sim_data <- sd_net_change(o_df, "Infected")
    loglik   <- sum(dpois(data, lambda = sim_data[, "value"] + 1e-05, log = TRUE))
    loglik
    }
  }

  expected_fun <- test_gen(deSolve_components, fit_options$data)

  comparison_result <- all.equal(actual_fun, expected_fun,
                                 check.environment = FALSE)

  expect_equal(comparison_result, TRUE)
})

test_that("sd_loglik_fun() returns the expected function works with extra constraints", {

  pars_df <- data.frame(name      = c("beta_var", "I", "R"),
                        type      = c("constant", "stock", "stock"),
                        par_trans = c("log", "log", "log"))

  filepath           <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl                <- read_xmile(filepath)
  deSolve_components <- mdl$deSolve_components

  sim_controls       <- list(start = 0, stop = 10, step = 0.25,
                             integ_method ="rk4")

  fit_options <- list(stock_name = "Infected", stock_fit_type = "net_change",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"),
                      data = 1:10)

  extra_constraints <- list("I + R < 1000")

  actual_fun <- sd_loglik_fun(pars_df = pars_df,
                              deSolve_components = deSolve_components,
                              sim_controls = sim_controls,
                              fit_options = fit_options,
                              extra_constraints = extra_constraints)

  test_gen <- function(deSolve_components, data) {
    init_stocks <- deSolve_components$stocks
    consts      <- deSolve_components$consts

    function(pars)
    {
      pars[[1]] <- exp(pars[[1]])
      pars[[2]] <- exp(pars[[2]])
      pars[[3]] <- exp(pars[[3]])
      if (pars[[2]] + pars[[3]] < 1000) return(-Inf)
      consts["beta_var"] <- pars[[1]]
      init_stocks["I"] <- pars[[2]]
      init_stocks["R"] <- pars[[3]]
      simtime <- seq(0, 10, 0.25)
      o <- deSolve::ode(y = init_stocks, times = simtime, func = deSolve_components$func, parms = consts, method = "rk4")
      o_df <- data.frame(o)
      sim_data <- sd_net_change(o_df, "Infected")
      loglik   <- sum(dpois(data, lambda = sim_data[, "value"] + 1e-05, log = TRUE))
      loglik
    }
  }

  expected_fun <- test_gen(deSolve_components, fit_options$data)

  comparison_result <- all.equal(actual_fun, expected_fun,
                                 check.environment = FALSE)

  expect_equal(comparison_result, TRUE)
})

test_that("sd_loglik_fun() returns the reflected loglik function", {

  pars_df <- data.frame(name = "beta_var", type = "constant", par_trans = "log")

  filepath           <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl                <- read_xmile(filepath)
  deSolve_components <- mdl$deSolve_components

  sim_controls       <- list(start = 0, stop = 10, step = 0.25,
                             integ_method ="rk4")

  fit_options <- list(stock_name = "Infected", stock_fit_type = "net_change",
                      dist = list(name     = "dpois",
                                  sim_data = "lambda",
                                  dist_offset = "1e-5"),
                      data = 1:10)

  actual_fun <- sd_loglik_fun(pars_df, deSolve_components,
                              sim_controls, fit_options, neg_log = TRUE)

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
      sim_data <- sd_net_change(o_df, "Infected")
      loglik   <- sum(dpois(data, lambda = sim_data[, "value"] + 1e-05, log = TRUE))
      -loglik
    }
  }

  expected_fun <- test_gen(deSolve_components, fit_options$data)

  comparison_result <- all.equal(actual_fun, expected_fun,
                                 check.environment = FALSE)

  expect_equal(comparison_result, TRUE)
})

test_that("sd_loglik_fun() returns the loglik function with an unknown in the measurement model", {

  pars_df <- data.frame(name      = c("beta_var", "sd"),
                        type      = c("constant", "par_measure"),
                        par_trans = c("log", "log"))

  filepath           <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl                <- read_xmile(filepath)
  deSolve_components <- mdl$deSolve_components

  sim_controls       <- list(start = 0, stop = 10, step = 0.25,
                             integ_method ="rk4")

  fit_options <- list(stock_name = "Infected", stock_fit_type = "net_change",
                      dist = list(name     = "dnorm",
                                  sim_data = "mean"),
                      data = 1:10)

  actual_fun <- sd_loglik_fun(pars_df, deSolve_components,
                              sim_controls, fit_options)

  test_gen <- function(deSolve_components, data) {
    init_stocks <- deSolve_components$stocks
    consts      <- deSolve_components$consts

    function(pars)
    {
      pars[[1]] <- exp(pars[[1]])
      pars[[2]] <- exp(pars[[2]])
      consts["beta_var"] <- pars[[1]]
      simtime <- seq(0, 10, 0.25)
      o <- deSolve::ode(y = init_stocks, times = simtime, func = deSolve_components$func, parms = consts, method = "rk4")
      o_df <- data.frame(o)
      sim_data <- sd_net_change(o_df, "Infected")
      loglik   <- sum(dnorm(data, mean = sim_data[, "value"], sd = pars[[2]], log = TRUE))
      loglik
    }
  }

  expected_fun <- test_gen(deSolve_components, fit_options$data)

  comparison_result <- all.equal(actual_fun, expected_fun,
                                 check.environment = FALSE)

  expect_equal(comparison_result, TRUE)
})

# arrange_pars() ---------------------------------------------------------------
test_that("arrange_pars() return the expected dataframe", {
  pars_df <- data.frame(name      = c("I", "R", "par_beta", "rho"),
                        type      = c("stock", "stock", "constant", "constant"),
                        par_trans = c("log", "log", "log", "logit"))

  actual_df <- arrange_pars(pars_df)

  expected_df <- data.frame(name      = c("par_beta", "rho", "I", "R"),
                            type      = c("constant", "constant", "stock", "stock"),
                            par_trans = c("log", "logit", "log", "log"),
                            pos       = 1:4)

  expect_equal(actual_df, expected_df)
})
