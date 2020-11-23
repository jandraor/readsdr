test_that("translate_SQRT returns the expected string", {
  expect_equal(translate_SQRT("SQRT(x)"), "sqrt(x)")
})
