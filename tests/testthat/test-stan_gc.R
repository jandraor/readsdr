test_that("stan_gc() returns the expected string for a net flow measurement", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  actual <- stan_gc(meas_mdl, FALSE, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  log_lik = neg_binomial_2_lpmf(y | delta_x_1, phi);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  actual <- stan_gc(meas_mdl, TRUE)

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  real log_lik_pred;",
    "  log_lik = neg_binomial_2_lpmf(y | delta_x_1, phi);",
    "  log_lik_pred = neg_binomial_2_lpmf(y_ahead | y_pred, phi);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  meas_mdl <- list("y ~ poisson(net_flow(C))")
  actual   <- stan_gc(meas_mdl, FALSE, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  log_lik = poisson_lpmf(y | delta_x_1);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_gc() returns the expected string for a stock measurement", {

  mm1      <- "y ~ poisson(C)"
  meas_mdl <- list(mm1)

  actual <- stan_gc(meas_mdl, FALSE, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  log_lik = poisson_lpmf(y | x[:, 5]);",
    "}", sep = "\n")

  expect_equal(actual, expected)

  meas_mdl <- list("y ~ poisson(C)")
  actual   <- stan_gc(meas_mdl, TRUE, c("S", "E", "I", "R", "C"))

  expected <- paste(
    "generated quantities {",
    "  real log_lik;",
    "  real log_lik_pred;",
    "  log_lik = poisson_lpmf(y | x[:, 5]);",
    "  log_lik_pred = poisson_lpmf(y_ahead | y_pred);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("get_log_lik_statement() returns the expected string", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual   <- get_log_lik_statement(meas_obj, FALSE)

  expected <- "neg_binomial_2_lpmf(y | delta_x_1, phi);"

  expect_equal(actual, expected)
})
