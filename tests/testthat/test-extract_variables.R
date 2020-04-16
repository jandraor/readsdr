test_that("extract_variables() returns variables in equations that have numbers & exponents", {
  test_lhs      <- "test_var"
  equation      <- "gamma * (1 + beta) ^ alpha"
  actual_vars   <- extract_variables(test_lhs, equation)
  actual_vars   <- sort(actual_vars)
  expected_vars <- c("alpha", "beta", "gamma")
  expect_equal(actual_vars, expected_vars)
})

test_that("extract_variables() ignores min function", {
  test_lhs      <- "test_var"
  equation      <- "min(minimiser, maximiser)"
  actual_vars   <- extract_variables(test_lhs, equation)
  actual_vars   <- sort(actual_vars)
  expected_vars <- c("maximiser", "minimiser")
  expect_equal(actual_vars, expected_vars)
})

test_that("extract_variables() ignores max function", {
  test_lhs      <- "test_var"
  equation      <- "max(minimiser, maximiser)"
  actual_vars   <- extract_variables(test_lhs, equation)
  actual_vars   <- sort(actual_vars)
  expected_vars <- c("maximiser", "minimiser")
  expect_equal(actual_vars, expected_vars)
})

test_that("extract_variables() ignores graph functions", {
  test_lhs      <- "test_var"
  equation      <- "f_test_var(a + b)"
  actual_vars   <- extract_variables(test_lhs, equation)
  actual_vars   <- sort(actual_vars)
  expected_vars <- c("a", "b")
  expect_equal(actual_vars, expected_vars)
})

test_that("extract_variables() returns unique elements", {
  test_lhs      <- "test_var"
  equation      <- "a^2 + 2*a*b + b^2"
  actual_vars   <- extract_variables(test_lhs, equation)
  actual_vars   <- sort(actual_vars)
  expected_vars <- c("a", "b")
  expect_equal(actual_vars, expected_vars)
})

test_that("extract_variables() ignores ifelse functions", {
  test_lhs      <- "test_var"
  equation      <- "ifelse(a + b, a, b)"
  actual_vars   <- extract_variables(test_lhs, equation)
  actual_vars   <- sort(actual_vars)
  expected_vars <- c("a", "b")
  expect_equal(actual_vars, expected_vars)
})

test_that("extract_variables() ignores logical operators ", {
  test_lhs      <- "test_var"
  equation      <- "ifelse(!(a>b)&a>c|c<b, a, b)"
  expect_equal(extract_variables(test_lhs, equation), c("a", "b", "c"))
})
