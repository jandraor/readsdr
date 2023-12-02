test_that("stan_gc() returns the expected string for a net flow measurement", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  actual <- stan_gc(meas_mdl, FALSE, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y;",
    "  log_lik = neg_binomial_2_lpmf(y | delta_x_1, phi);",
    "  sim_y = neg_binomial_2_rng(delta_x_1, phi);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  actual <- stan_gc(meas_mdl, TRUE)

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  real log_lik_pred;",
    "  array[n_obs] int sim_y;",
    "  log_lik = neg_binomial_2_lpmf(y | delta_x_1, phi);",
    "  log_lik_pred = neg_binomial_2_lpmf(y_ahead | y_pred, phi);",
    "  sim_y = neg_binomial_2_rng(delta_x_1, phi);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  meas_mdl <- list("y ~ poisson(net_flow(C))")
  actual   <- stan_gc(meas_mdl, FALSE, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y;",
    "  log_lik = poisson_lpmf(y | delta_x_1);",
    "  sim_y = poisson_rng(delta_x_1);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  meas_mdl <- list("y ~ normal(net_flow(C), tau)")
  actual   <- stan_gc(meas_mdl, FALSE, c("S", "E", "I", "R", "C"))

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
  actual   <- stan_gc(meas_mdl, FALSE, c("Hares", "Lynx"))

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

  actual <- stan_gc(meas_mdl, FALSE, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y;",
    "  log_lik = poisson_lpmf(y | x[:, 5]);",
    "  sim_y = poisson_rng(x[:, 5]);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  meas_mdl <- list("y ~ poisson(C)")
  actual   <- stan_gc(meas_mdl, TRUE, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  real log_lik_pred;",
    "  array[n_obs] int sim_y;",
    "  log_lik = poisson_lpmf(y | x[:, 5]);",
    "  log_lik_pred = poisson_lpmf(y_ahead | y_pred);",
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

  actual       <- stan_gc(meas_mdl, FALSE, lvl_names)

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  array[n_obs] int sim_y_A;",
    "  array[n_obs] int sim_y_B;",
    "  array[n_obs] int sim_y_C;",
    "  array[n_obs] int sim_y_D;",
    "  log_lik = poisson_lpmf(y_A | delta_x_1)+poisson_lpmf(y_B | delta_x_2)+poisson_lpmf(y_C | delta_x_3)+poisson_lpmf(y_D | delta_x_4);",
    "  sim_y_A = poisson_rng(delta_x_1);",
    "  sim_y_B = poisson_rng(delta_x_2);",
    "  sim_y_C = poisson_rng(delta_x_3);",
    "  sim_y_D = poisson_rng(delta_x_4);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("get_log_lik_statement() returns the expected string", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual   <- get_log_lik_statement(meas_obj, FALSE)

  expected <- "neg_binomial_2_lpmf(y | delta_x_1, phi);"

  expect_equal(actual, expected)
})

test_that("generate_sim_data_lines() returns the expected list", {

  meas_mdl  <- list("y ~ neg_binomial_2(net_flow(C), phi)")
  lvl_names <- c("S", "E", "I", "R", "C")

  actual <- generate_sim_data_lines(meas_mdl, lvl_names)

  expected <- list(decl   = "  array[n_obs] int sim_y;",
                   assign = "  sim_y = neg_binomial_2_rng(delta_x_1, phi);")

  expect_equal(actual, expected)

  meas_mdl  <- list("y1 ~ lognormal(log(Lynx), sigma_1)")
  lvl_names <- c("Hares", "Lynx" )
  actual    <- generate_sim_data_lines(meas_mdl, lvl_names)

  expected <- list(decl   = "  array[n_obs] real sim_y1;",
                   assign = "  sim_y1 = lognormal_rng(log(x[:, 2]), sigma_1);")

  expect_equal(actual, expected)
})

test_that("get_dist_dens_mass_fun() returns the expected list", {

  lhs       <- "y1"
  lvl_names <- c("Hares", "Lynx" )
  dist_obj  <- list(dist_name = "lognormal",
                    mu        = "log(Lynx)",
                    sigma     = "sigma_1")

  actual <- get_dist_dens_mass_fun(lhs, dist_obj, FALSE, lvl_names, 1)

  expected <- list(rhs           = "lognormal_lpdf(y1 | log(x[:, 2]), sigma_1)",
                   delta_counter = 1)

  expect_equal(actual, expected)
})
