
test_that("extract_timeseries_var() returns the expected data frame", {
  test_df <- data.frame(`var[1]` = rep(0, 2),
                        `var[2]` = rep(1, 2),
                        check.names = FALSE)

  expected_df <- data.frame(iter     = rep(1:2, 2),
                            time     = rep(1:2, each = 2),
                            variable = "var",
                            value    = c(0, 0, 1, 1))

  expect_equal(extract_timeseries_var("var", test_df), expected_df)

})

test_that("extract_timeseries_stock() returns the expected data frame", {
  test_df <- data.frame(`yhat[1,2]` = rep(0, 2),
                        `yhat[2,2]` = rep(1, 2),
                        check.names = FALSE)

  expected_df <- data.frame(iter     = rep(1:2, 2),
                            time     = rep(1:2, each = 2),
                            stock    = "S2",
                            value    = c(0, 0, 1, 1))

  test_stocks <- c("S1", "S2")

  expect_equal(extract_timeseries_stock("S2", test_df, test_stocks, "yhat"),
               expected_df)
})

test_that("construct_data_decl() returns the expected string", {

  meas_obj   <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual     <- construct_data_decl(meas_obj, FALSE)
  expected   <- "  array[n_obs] int y;"

  expect_equal(actual, expected)
})

test_that("stan_data() returns the expected string", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  array[n_obs] int y;",
    "  real t0;",
    "  array[n_obs] real ts;",
    "}", sep = "\n")

  expect_equal(stan_data(meas_mdl, TRUE, FALSE, NULL, NULL), expected_string)

})

test_that("stan_data() declares the vector for init values", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  array[n_obs] int y;",
    "  real t0;",
    "  array[n_obs] real ts;",
    "  vector[5] x0;",
    "}", sep = "\n")

  expect_equal(stan_data(meas_mdl, FALSE, FALSE, NULL, NULL, 5),
               expected_string)

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  array[n_obs] int y;",
    "  int y_ahead;",
    "  real t0;",
    "  array[n_obs + 1] real ts;",
    "  vector[5] x0;",
    "}", sep = "\n")

  expect_equal(stan_data(meas_mdl, FALSE, TRUE, NULL, NULL, 5),
               expected_string)
})

test_that("construct_data_decl() returns the expected string", {
  meas_obj      <- "y ~ neg_binomial_2(net_flow(C), phi)"

  actual <- construct_data_decl(meas_obj, TRUE)

  expected <- paste("  array[n_obs] int y;",
                    "  int y_ahead;", sep = "\n")

  expect_equal(actual, expected)
})

test_that("get_dist_obj() declares the vector for init values", {

  rhs <- "neg_binomial_2(net_flow(C), phi)"

  actual <- get_dist_obj(rhs)

  expected <- list(dist_name = "neg_binomial_2",
                   mu        = "net_flow(C)",
                   phi       = "phi")

  expect_equal(actual, expected)
})

test_that("get_meas_params() deals with a given concentration parameter", {

  meas_mdl         <- list("y ~ neg_binomial_2(net_flow(C), 10)")
  estimated_params <- list(sd_prior("par_beta", "lognormal", c(0, 1)))

  actual   <- get_meas_params(meas_mdl, estimated_params)
  expected <- estimated_params

  expect_equal(actual, expected)
})
