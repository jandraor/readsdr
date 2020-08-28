test_that("stan_transformed_data() returns the expected string", {
  expected_string <- paste(
    "transformed data {",
    "  real x_r[0];",
    "  int  x_i[0];",
    "}", sep = "\n")
  expect_equal(stan_transformed_data(), expected_string)
})

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

test_that("stan_data() returns the expected string", {

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  int<lower = 1> n_params;",
    "  int<lower = 1> n_difeq;",
    "  int<lower = 1> y[n_obs];",
    "  real t0;",
    "  real ts[n_obs];",
    "  vector[n_difeq] y0;",
    "}", sep = "\n")

  expect_equal(stan_data("y"), expected_string)
})

