context("Translate SMOOTH functions")

test_that("translate_SMTH1() returns the expected object", {
  test_equation <- "SMTH1(0.5,  5,  1)"
  actual_obj    <- translate_SMOOTH1("S1", test_equation, "isee")

  expected_obj  <- list(
    variable = list(
      name     = "adjust_S1",
      equation = "(0.5-S1)/5"),
    stock    = list(
      name      = "S1" ,
      equation  = "adjust_S1",
      initValue = 1))

  expect_equal(actual_obj, expected_obj)

})
