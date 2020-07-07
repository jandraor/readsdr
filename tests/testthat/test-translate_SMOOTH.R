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

test_that("translate_SMTH3() returns the expected object", {
  test_equation <- "SMTH3(0.5,  6,  1)"
  actual_obj    <- translate_SMOOTH3("S3", test_equation, "isee")

  expected_obj  <- list(
    variable_list = list(
      list(
        name     = "adjust_S3",
        equation = "(S3_2-S3)/2"),
      list(
        name     = "adjust_S3_2",
        equation = "(S3_3-S3_2)/2"),
      list(
        name     = "adjust_S3_3",
        equation = "(0.5-S3_3)/2")
    ),
    stock_list = list(
      list(
        name      = "S3" ,
        equation  = "adjust_S3",
        initValue = 1),
      list(
        name      = "S3_2" ,
        equation  = "adjust_S3_2",
        initValue = 1),
      list(
        name      = "S3_3" ,
        equation  = "adjust_S3_3",
        initValue = 1)
    ))

  expect_equal(actual_obj, expected_obj)

})
