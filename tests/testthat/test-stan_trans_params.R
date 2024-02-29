#------------------stan_trans_params()------------------------------------------

test_that("stan_trans_params() returns the expected string", {

  estimated_params <- list(sd_prior("par_beta", "lognormal", c(0, 1)),
                           sd_prior("par_rho", "beta", c(2, 2)),
                           sd_prior("I0", "lognormal", c(0, 1), "init"),
                           list(par_name  = "inv_phi",
                                type      = "meas_par",
                                par_trans = "inv"))


  lvl_obj <- list(list(name      = "S",
                       equation  = "-S_to_E",
                       initValue = "10000 - I0"),
                  list(name      = "E",
                       equation  = "S_to_E-E_to_I",
                       initValue = "0"),
                  list(name      = "I",
                       equation  = "E_to_I-I_to_R",
                       initValue = "I0"),
                  list(name      = "R",
                       equation  = "I_to_R",
                       initValue = "0"),
                  list(name      = "C",
                       equation  = "C_in",
                       initValue = "I0"))

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  actual <- stan_trans_params(estimated_params, meas_mdl, lvl_obj,
                              unk_inits   = TRUE,
                              data_params = NULL)

  expected <-  paste(
    "transformed parameters{",
    "  array[n_obs] vector[5] x; // Output from the ODE solver",
    "  array[2] real params;",
    "  vector[5] x0; // init values",
    "  array[n_obs] real delta_x_1;",
    "  real phi;",
    "  phi = 1 / inv_phi;",
    "  x0[1] = 10000 - I0; // S",
    "  x0[2] = 0; // E",
    "  x0[3] = I0; // I",
    "  x0[4] = 0; // R",
    "  x0[5] = I0; // C",
    "  params[1] = par_beta;",
    "  params[2] = par_rho;",
    "  x = ode_rk45(X_model, x0, 0, ts, params);",
    "  delta_x_1[1] =  x[1, 5] - x0[5] + 1e-5;",
    "  for (i in 1:n_obs-1) {",
    "    delta_x_1[i + 1] = x[i + 1, 5] - x[i, 5] + 1e-5;",
    "  }",
    "}", sep = "\n")

  expect_equal(actual, expected)

})

test_that("pars_assigned() ignores inits & meas pars", {

  estimated_params <- list(list(par_name = "par_beta", type = "constant"),
                           list(par_name = "par_rho", type = "constant"),
                           list(par_name = "I0", type = "init"),
                           list(par_name = "inv_phi", type = "meas_par"))

  actual   <- pars_assigned(estimated_params, NULL)
  expected <- c("par_beta", "par_rho")

  expect_equal(actual, expected)
})

test_that("extract_par_trans() returns the expected list", {

  prior <- list(list(par_name = "par_beta", type = "constant"),
                list(par_name = "par_rho", type = "constant"),
                list(par_name = "I0", type = "init"),
                list(par_name = "inv_phi", type = "meas_par",
                     par_trans = "inv"))

  actual <- extract_par_trans(prior)

  expected <- list(list(decl  = "  real phi;",
                        trans = "  phi = 1 / inv_phi;"))

  expect_equal(actual, expected)
})

test_that("extract_delta_decl() returns the expected list", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  actual <- extract_delta_decl(meas_mdl)

  expected <- list("  array[n_obs] real delta_x_1;")

  expect_equal(actual, expected)
})

test_that("construct_stock_init_lines() returns the expected list", {

  lvl_obj <- list(list(name      = "S",
                       equation  = "-S_to_E",
                       initValue = "10000 - I0"),
                  list(name      = "E",
                       equation  = "S_to_E-E_to_I",
                       initValue = "0"),
                  list(name      = "I",
                       equation  = "E_to_I-I_to_R",
                       initValue = "I0"),
                  list(name      = "R",
                       equation  = "I_to_R",
                       initValue = "0"),
                  list(name      = "C",
                       equation  = "C_in",
                       initValue = "I0"))

  prior <- list(list(par_name = "I0", type = "init"))

  actual <- construct_stock_init_lines(lvl_obj)

  expected <- paste(
    "  x0[1] = 10000 - I0; // S",
    "  x0[2] = 0; // E",
    "  x0[3] = I0; // I",
    "  x0[4] = 0; // R",
    "  x0[5] = I0; // C", sep = "\n")

  expect_equal(actual, expected)
})

test_that("get_for_body() returns the expected list", {

  filepath        <- system.file("models/", "SEIR.stmx", package = "readsdr")
  model_structure <- extract_structure_from_XMILE(filepath)

  mm1       <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl  <- list(mm1)
  lvl_names <- c("S", "E", "I", "R", "C")
  actual    <- get_for_body(meas_mdl, lvl_names)

  expected <- "    delta_x_1[i + 1] = x[i + 1, 5] - x[i, 5] + 1e-5;"

  expect_equal(actual, expected)

})

test_that("get_pred_asg() returns the expected string", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  lvl_names <- c("S", "E", "I", "R", "C")

  actual <- get_pred_asg(meas_mdl, lvl_names, "delta_meas")

  expected <- "  y_pred = x[n_obs + 1, 5] - x[n_obs, 5];"

  expect_equal(actual, expected)

  mm1       <-  "y ~ poisson(C)"
  meas_mdl  <- list(mm1)
  lvl_names <- c("S", "E", "I", "R", "C")
  actual    <- get_pred_asg(meas_mdl, lvl_names, "stock_meas")
  expected  <- "  y_pred = x[n_obs + 1, 5];"

  expect_equal(actual, expected)
})
