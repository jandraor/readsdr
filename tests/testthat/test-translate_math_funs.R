test_that("translate_SQRT returns the expected string", {
  expect_equal(translate_SQRT("SQRT(x)"), "sqrt(x)")
})

test_that("translate_EXP returns the expected string", {

  eq       <- "EXP(-par_alpha*P)"
  actual   <- translate_EXP(eq)
  expected <- "exp(-par_alpha*P)"

  expect_equal(actual, expected)
})
