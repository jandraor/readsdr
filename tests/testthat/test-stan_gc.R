test_that("stan_gc() returns the expected string", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  actual <- stan_gc(meas_mdl, FALSE)

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
})

test_that("get_log_lik_statement() returns the expected string", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual   <- get_log_lik_statement(meas_obj, FALSE)

  expected <- "neg_binomial_2_lpmf(y | delta_x_1, phi);"

  expect_equal(actual, expected)
})
