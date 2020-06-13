test_that("stan_transformed_data() returns the expected string", {
  expected_string <- paste(
    "transformed data {",
    "  real x_r[0];",
    "  int  x_i[0];",
    "}", sep = "\n")
  expect_equal(stan_transformed_data(), expected_string)
})
