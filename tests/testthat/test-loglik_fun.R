test_that("get_par_list() returns the expected list", {

  unknown_pars <- list(list(par_name = "par_beta", min = 0),
                       list(par_name = "inv_phi", min = 0, type = "meas_par",
                            par_trans = "inv"))

  actual <- get_par_list(unknown_pars)

  expected <- list(list(par_name = "par_beta", par_trans = "exp"),
                   list(par_name = "phi", par_trans = c("exp", "inv")))

  expect_equal(actual, expected)
})

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

  actual_text <- get_model_run_text("rk4")

  expected_text <- paste(
    "  readsdr_env <- list2env(as.list(ds_inputs$consts))",
    "  ds_inputs$stocks <- purrr::map_dbl(ds_inputs$stocks, function(x) {",
    "    eval(parse(text = x), envir = readsdr_env)",
    "  })",
    '  o    <- sd_simulate(ds_inputs, integ_method = "rk4")',
    "  o_df <- data.frame(o)",
    sep = "\n")

  expect_equal(actual_text, expected_text)
})

# get_meas_model_text() --------------------------------------------------------

test_that("get_meas_model_text() returns the expected text", {

  # As-is measurement

  meas_data_mdl <- list(list(formula      = "y ~ poisson(C)",
                             measurements = 1:10))

  n_consts <- 1

  unknown_pars <- list(list(par_name = "par_beta", min = 0))

  actual_text <- get_meas_model_text(meas_data_mdl, n_consts, unknown_pars,
                                     FALSE)

  expected_text <- paste(
    'sim_data_1 <- dplyr::filter(o_df, time - trunc(time) == 0)',
    "loglik_1   <- sum(dpois(data_1, lambda = sim_data_1[, 'C'] + 1e-05, log = TRUE))",
    "loglik     <- loglik_1",
    "loglik", sep = "\n")

  expect_equal(actual_text, expected_text)

  # Net flow

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  n_consts <- 1

  unknown_pars <- list(list(par_name = "par_beta", min = 0),
                       list(par_name = "inv_phi", min = 0, par_trans = "inv"))

  actual_text <- get_meas_model_text(meas_data_mdl, n_consts, unknown_pars,
                                     FALSE)

  expected_text <- paste(
    'sim_data_1 <- sd_net_change(o_df, "C")',
    "loglik_1   <- sum(dnbinom(data_1, mu = sim_data_1[, 'value'] + 1e-05, size = 1/pars[[2]], log = TRUE))",
    "loglik     <- loglik_1",
    "loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)

  # Reflect log-lik

  actual_text <- get_meas_model_text(meas_data_mdl, n_consts, unknown_pars, TRUE)

  expected_text <- paste(
    'sim_data_1 <- sd_net_change(o_df, "C")',
    "loglik_1   <- sum(dnbinom(data_1, mu = sim_data_1[, 'value'] + 1e-05, size = 1/pars[[2]], log = TRUE))",
    "loglik     <- loglik_1",
    "-loglik",
    sep = "\n")
})

test_that("get_meas_model_text() handles a known par in the measurement model", {

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), 10)",
                             measurements = 1:10))

  n_consts <- 1

  unknown_pars <- list(list(par_name = "par_beta", min = 0))

  actual_text <- get_meas_model_text(meas_data_mdl, n_consts, unknown_pars,
                                     FALSE)

  expected_text <- paste(
    'sim_data_1 <- sd_net_change(o_df, "C")',
    "loglik_1   <- sum(dnbinom(data_1, mu = sim_data_1[, 'value'] + 1e-05, size = 10, log = TRUE))",
    "loglik     <- loglik_1",
    "loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)
})

test_that("get_meas_model_text() handles fixed pars", {

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  n_consts      <- 1

  unknown_pars  <- list(list(par_name = "par_beta", min = 0))

  supplied_pars <- "phi"

  actual_text <- get_meas_model_text(meas_data_mdl, n_consts, unknown_pars,
                                     FALSE, supplied_pars)

  expected_text <- paste(
    'sim_data_1 <- sd_net_change(o_df, "C")',
    "loglik_1   <- sum(dnbinom(data_1, mu = sim_data_1[, 'value'] + 1e-05, size = phi, log = TRUE))",
    "loglik     <- loglik_1",
    "loglik",
    sep = "\n")

  expect_equal(actual_text, expected_text)
})

test_that("get_meas_model_text() handles multiple measurements", {

  n_consts <- 1

  meas_data_mdl <- list(list(formula      = "y_A ~ neg_binomial_2(net_flow(C_A), phi)",
                             measurements = 1:10),
                        list(formula      = "y_B ~ neg_binomial_2(net_flow(C_B), phi)",
                             measurements = 10:20))

  unknown_pars <- list(list(par_name = "par_beta", min = 0),
                       list(par_name = "inv_phi", min = 0, par_trans = "inv"))

  actual_text <- get_meas_model_text(meas_data_mdl, n_consts, unknown_pars,
                                     FALSE)


  expected_text <-      paste(
    'sim_data_1 <- sd_net_change(o_df, "C_A")',
    "loglik_1   <- sum(dnbinom(data_1, mu = sim_data_1[, 'value'] + 1e-05, size = 1/pars[[2]], log = TRUE))",
    'sim_data_2 <- sd_net_change(o_df, "C_B")',
    "loglik_2   <- sum(dnbinom(data_2, mu = sim_data_2[, 'value'] + 1e-05, size = 1/pars[[2]], log = TRUE))",
    'loglik     <- loglik_1 + loglik_2',
    'loglik', sep = "\n")

  expect_equal(actual_text, expected_text)
})

# sd_loglik_fun() --------------------------------------------------------------

test_that("sd_loglik_fun() returns the expected object", {

  filepath      <- system.file("models/", "SEIR.stmx", package = "readsdr")

  unknown_pars  <- list(list(par_name = "par_beta", min = 0))

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  fun_obj <- sd_loglik_fun(filepath, unknown_pars, meas_data_mdl, neg_log = FALSE,
                           start_time = 0, stop_time = 10, timestep = 1/32)

  actual_val <- fun_obj$fun(c(1, 0.1))

  expected_val <- -32.47283

  expect_equal(actual_val, expected_val, tolerance = 1e-4)

  actual_list <- fun_obj$par_list

  expected_list <- list(list(par_name = "par_beta", par_trans = "exp"),
                        list(par_name = "phi", par_trans = c("exp", "inv")))

  expect_equal(actual_list, expected_list)

  # Test negative loglik
  fun_obj <- sd_loglik_fun(filepath, unknown_pars, meas_data_mdl,
                           neg_log = TRUE, start_time = 0, stop_time = 10,
                           timestep = 1/32)

  actual_val <- fun_obj$fun(c(1, 0.1))

  expected_val <- 32.47283

  expect_equal(actual_val, expected_val, tolerance = 1e-4)
})

# Multiple meas
test_that("sd_loglik_fun() handles multiple measurements", {

  filepath <- system.file("models/", "SEIR_age.stmx", package = "readsdr")

  unknown_pars  <- list(list(par_name = "k_AA", min = 0),
                        list(par_name = "par_rho", min = 0, max = 1))

  meas_data_mdl <- list(list(formula      = "y_A ~ neg_binomial_2(net_flow(C_A), phi)",
                             measurements = 1:10),
                        list(formula      = "y_B ~ neg_binomial_2(net_flow(C_B), phi)",
                             measurements = 11:20),
                        list(formula      = "y_C ~ neg_binomial_2(net_flow(C_C), phi)",
                             measurements = 21:30),
                        list(formula      = "y_D ~ neg_binomial_2(net_flow(C_D), phi)",
                             measurements = 31:40))

  fun_obj <- sd_loglik_fun(filepath, unknown_pars, meas_data_mdl, neg_log = FALSE,
                           start_time = 0, stop_time = 10, timestep = 1/32)

  actual_val <- fun_obj$fun(c(4, 0.5,0.1))

  expected_val <- -1168.551

  expect_equal(actual_val, expected_val, tolerance = 1e-4)
})

# Fixed pars

test_that("sd_loglik_fun() handles fixed pars", {

  filepath      <- system.file("models/", "SEIR.stmx", package = "readsdr")

  unknown_pars  <- list(list(par_name = "par_beta", min = 0))

  supplied_pars    <- c("par_rho", "I0", "phi")

  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  fun_obj <- sd_loglik_fun(filepath, unknown_pars, meas_data_mdl, neg_log = FALSE,
                           supplied_pars = supplied_pars, start_time = 0,
                           stop_time = 10, timestep = 1/32)

  actual_val <- fun_obj$fun(c(1), list(par_rho = 0.75,
                                       I0 = 1,
                                       inv_phi = exp(0.1)))

  expected_val <- -32.47283

  expect_equal(actual_val, expected_val, tolerance = 1e-4)

  actual_list <- fun_obj$par_list

  expected_list <- list(list(par_name = "par_beta", par_trans = "exp"),
                        list(par_name = "phi", par_trans = c("exp", "inv")))
})

test_that("sd_loglik_fun() overrides sim params", {

  filepath      <- system.file("models/", "SEIR.stmx", package = "readsdr")
  unknown_pars  <- list(list(par_name = "par_beta", min = 0))
  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  fun_obj <- sd_loglik_fun(filepath, unknown_pars, meas_data_mdl,
                           start_time = 0, stop_time = 10, timestep = 1/128)

  expect_obj <- list(start = 0,
                     stop  = 10,
                     dt    = 1/128)

  actual_obj <- fun_obj$ds_inputs$sim_params

  expect_equal(actual_obj, expect_obj)
})

test_that("sd_loglik_fun() overrides consts", {

  filepath      <- system.file("models/", "SEIR.stmx", package = "readsdr")

  unknown_pars  <- list(list(par_name = "par_beta", min = 0))
  meas_data_mdl <- list(list(formula      = "y ~ neg_binomial_2(net_flow(C), phi)",
                             measurements = 1:10))

  N_val <- 5234

  const_list <- list(N = N_val)

  fun_obj <- sd_loglik_fun(filepath, unknown_pars, meas_data_mdl,
                           start_time = 0, stop_time = 10, timestep = 1/128,
                           const_list = const_list)

  expect_equal(fun_obj$ds_inputs$consts[["N"]], N_val)
})
