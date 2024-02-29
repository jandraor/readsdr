test_that("stan_gc() returns the expected string for a net flow measurement", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  actual <- stan_gc(meas_mdl, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y;",
    "  log_lik = neg_binomial_2_lpmf(y | delta_x_1, phi);",
    "  sim_y = neg_binomial_2_rng(delta_x_1, phi);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  meas_mdl <- list("y ~ poisson(net_flow(C))")
  actual   <- stan_gc(meas_mdl, lvl_names = c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y;",
    "  log_lik = poisson_lpmf(y | delta_x_1);",
    "  sim_y = poisson_rng(delta_x_1);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  meas_mdl <- list("y ~ normal(net_flow(C), tau)")
  actual   <- stan_gc(meas_mdl, lvl_names = c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] real sim_y;",
    "  log_lik = normal_lpdf(y | delta_x_1, tau);",
    "  sim_y = normal_rng(delta_x_1, tau);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_gc() returns the expected string for a stock measurement", {

  meas_mdl <- "y1 ~ lognormal(Lynx, sigma_1)"
  actual   <- stan_gc(meas_mdl, lvl_names = c("Hares", "Lynx"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] real sim_y1;",
    "  log_lik = lognormal_lpdf(y1 | x[:, 2], sigma_1);",
    "  sim_y1 = lognormal_rng(x[:, 2], sigma_1);",
    "}", sep = "\n")

  expect_equal(actual, expected)


  mm1      <- "y ~ poisson(C)"
  meas_mdl <- list(mm1)

  actual <- stan_gc(meas_mdl, lvl_names = c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y;",
    "  log_lik = poisson_lpmf(y | x[:, 5]);",
    "  sim_y = poisson_rng(x[:, 5]);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_gc() returns the expected string for a vectorised net flow measurement", {

  ag <- c("A", "B", "C", "D") # age_groups

  measurements <- stringr::str_glue("y_{ag} ~ poisson(net_flow(C_{ag}))")
  meas_mdl     <- as.list(measurements)

  filepath  <- system.file("models/", "SEIR_age.stmx", package = "readsdr")
  mdl       <- read_xmile(filepath)
  lvl_names <- sd_stocks(mdl)$name

  actual       <- stan_gc(meas_mdl, lvl_names = lvl_names)

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y_A;",
    "  array[n_obs] int sim_y_B;",
    "  array[n_obs] int sim_y_C;",
    "  array[n_obs] int sim_y_D;",
    "  log_lik = poisson_lpmf(y_A | delta_x_1) +",
    "    poisson_lpmf(y_B | delta_x_2) +",
    "    poisson_lpmf(y_C | delta_x_3) +",
    "    poisson_lpmf(y_D | delta_x_4);",
    "  sim_y_A = poisson_rng(delta_x_1);",
    "  sim_y_B = poisson_rng(delta_x_2);",
    "  sim_y_C = poisson_rng(delta_x_3);",
    "  sim_y_D = poisson_rng(delta_x_4);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_gc() handles forecasts", {

  meas_mdl <- list("y ~ lognormal(log(Hares), sigma_1)",
                   "z ~ lognormal(log(Lynx), sigma_2)",
                   "y0 ~ lognormal(log(Hares[0]), sigma_1)",
                   "z0 ~ lognormal(log(Lynx[0]), sigma_1)")

  lvl_names <- c("Hares", "Lynx")

  actual       <- stan_gc(meas_mdl, lvl_names, forecast = TRUE)

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] real sim_y;",
    "  array[n_obs] real sim_z;",
    "  real sim_y0;",
    "  real sim_z0;",
    "  array[n_fcst] real fcst_y;",
    "  array[n_fcst] real fcst_z;",
    "  array[n_fcst] vector[2] x_fcst; // Forecast",
    "  array[n_fcst] real t_fcst;",
    "  vector[2] x_fcst0; // Forecast init values",
    "  log_lik = lognormal_lpdf(y | log(x[:, 1]), sigma_1) +",
    "    lognormal_lpdf(z | log(x[:, 2]), sigma_2) +",
    "    lognormal_lpdf(y0 | log(x0[1]), sigma_1) +",
    "    lognormal_lpdf(z0 | log(x0[2]), sigma_1);",
    "  // Simulate forecast",
    "  x_fcst0 = x[n_obs, :];",
    "  t_fcst = linspaced_array(n_fcst, 1, n_fcst);",
    "  x_fcst = ode_rk45(X_model, x_fcst0, 0, t_fcst, params);",
    "  sim_y = lognormal_rng(log(x[:, 1]), sigma_1);",
    "  sim_z = lognormal_rng(log(x[:, 2]), sigma_2);",
    "  sim_y0 = lognormal_rng(log(x0[1]), sigma_1);",
    "  sim_z0 = lognormal_rng(log(x0[2]), sigma_1);",
    "  fcst_y = lognormal_rng(log(x_fcst[:, 1]), sigma_1);",
    "  fcst_z = lognormal_rng(log(x_fcst[:, 2]), sigma_2);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_gc() handles forecasts with a net flow in the meas component", {

  meas_mdl <- list("y ~ poisson(net_flow(C))")

  lvl_names <- c("S", "E", "I", "R", "C")

  actual <- stan_gc(meas_mdl, lvl_names = lvl_names, forecast = TRUE)

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y;",
    "  array[n_fcst] int fcst_y;",
    "  array[n_fcst] vector[5] x_fcst; // Forecast",
    "  array[n_fcst] real t_fcst;",
    "  vector[5] x_fcst0; // Forecast init values",
    "  array[n_fcst] real delta_x_fcst_1;",
    "  log_lik = poisson_lpmf(y | delta_x_1);",
    "  // Simulate forecast",
    "  x_fcst0 = x[n_obs, :];",
    "  t_fcst = linspaced_array(n_fcst, 1, n_fcst);",
    "  x_fcst = ode_rk45(X_model, x_fcst0, 0, t_fcst, params);",
    "  delta_x_fcst_1[1] =  x_fcst[1, 5] - x_fcst0[5] + 1e-5;",
    "  for (i in 1:n_fcst-1) {",
    "    delta_x_fcst_1[i + 1] = x_fcst[i + 1, 5] - x_fcst[i, 5] + 1e-5;",
    "  }",
    "  sim_y = poisson_rng(delta_x_1);",
    "  fcst_y = poisson_rng(delta_x_fcst_1);",
    "}", sep = "\n")


  expect_equal(actual, expected)
})

test_that("get_log_lik_statement() returns the expected string", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual   <- get_log_lik_statement(meas_obj)

  expected <- "neg_binomial_2_lpmf(y | delta_x_1, phi);"

  expect_equal(actual, expected)
})

test_that("generate_sim_data_lines() returns the expected list", {

  meas_mdl  <- list("y ~ neg_binomial_2(net_flow(C), phi)")
  lvl_names <- c("S", "E", "I", "R", "C")

  actual <- generate_sim_data_lines(meas_mdl, lvl_names, fcst = 0)

  expected <- list(decl   = "  array[n_obs] int sim_y;",
                   assign = "  sim_y = neg_binomial_2_rng(delta_x_1, phi);")

  expect_equal(actual, expected)

  meas_mdl  <- list("y1 ~ lognormal(log(Lynx), sigma_1)")
  lvl_names <- c("Hares", "Lynx" )
  actual    <- generate_sim_data_lines(meas_mdl, lvl_names, fcst = 0)

  expected <- list(decl   = "  array[n_obs] real sim_y1;",
                   assign = "  sim_y1 = lognormal_rng(log(x[:, 2]), sigma_1);")

  expect_equal(actual, expected)
})

test_that("generate_sim_data_lines() handles single measurements", {

  meas_mdl <- list("y ~ lognormal(log(Hares), sigma_1)",
                   "y0 ~ lognormal(log(Hares[0]), sigma_1)")

  lvl_names <- c("Hares", "Lynx" )

  actual <- generate_sim_data_lines(meas_mdl, lvl_names, fcst = 0)

  expected <- list(decl = "  array[n_obs] real sim_y;\n  real sim_y0;",
                   assign = paste(
                     "  sim_y = lognormal_rng(log(x[:, 1]), sigma_1);",
                     "  sim_y0 = lognormal_rng(log(x0[1]), sigma_1);",
                     sep = "\n"))

  expect_equal(actual, expected)
})
# ---------------get_dist_dens_mass_fun() --------------------------------------

test_that("get_dist_dens_mass_fun() returns the expected list", {

  lhs       <- "y1"
  lvl_names <- c("Hares", "Lynx" )
  dist_obj  <- list(dist_name = "lognormal",
                    mu        = "log(Lynx)",
                    sigma     = "sigma_1")

  actual <- get_dist_dens_mass_fun(lhs, dist_obj, lvl_names, 1)

  expected <- list(rhs           = "lognormal_lpdf(y1 | log(x[:, 2]), sigma_1)",
                   delta_counter = 1)

  expect_equal(actual, expected)
})
