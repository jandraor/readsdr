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
