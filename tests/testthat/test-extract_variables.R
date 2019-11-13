test_that("extract_variables() returns variables in equations that have numbers & exponents", {
  equation      <- "gamma * (1 + beta) ^ alpha"
  actual_vars   <- extract_variables(equation)
  actual_vars   <- sort(actual_vars)
  expected_vars <- c("alpha", "beta", "gamma")
  expect_equal(actual_vars, expected_vars)
})
