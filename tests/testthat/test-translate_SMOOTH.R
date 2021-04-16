context("Translate SMOOTH functions")

test_that("translate_SMOOTH1() returns the expected object for an equation from
Stella", {
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

test_that("translate_SMOOTH1() returns the expected object for an equation from
Stella when not initialised", {
  test_equation <- "SMTH1(demand,  5)"
  actual_obj    <- translate_SMOOTH1("expected_demand", test_equation, "isee")

  expected_obj  <- list(
    variable = list(
      name     = "adjust_expected_demand",
      equation = "(demand-expected_demand)/5"),
    stock    = list(
      name      = "expected_demand" ,
      equation  = "adjust_expected_demand",
      initValue = "demand"))

  expect_equal(actual_obj, expected_obj)
})



test_that("translate_SMOOTH1() returns the expected object for an equation from
Vensim", {
  test_equation <- "SMOOTH(demand, 5)"
  actual_obj    <- translate_SMOOTH1("expected_demand", test_equation, "Vensim",
                                     "SMOOTH")

  expected_obj  <- list(
    variable = list(
      name     = "adjust_expected_demand",
      equation = "(demand-expected_demand)/5"),
    stock    = list(
      name      = "expected_demand" ,
      equation  = "adjust_expected_demand",
      initValue = "demand"))

  expect_equal(actual_obj, expected_obj)

})

test_that("translate_SMOOTH1() returns the expected object for an equation from
Vensim with initialisation", {
  test_equation <- "SMOOTHI(0.5, 5, 1)"
  actual_obj    <- translate_SMOOTH1("S1", test_equation, "Vensim", "SMOOTHI")

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

test_that("translate_SMOOTH3() returns the expected object for an equation from
Stella", {
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
    ),
    delay_order = 3)

  expect_equal(actual_obj, expected_obj)

})

test_that("translate_SMOOTH3() returns the expected object for an equation from Stella with no initialisation", {
  test_equation <- "SMTH3(demand, 6)"

  actual_obj    <- translate_SMOOTH3("expected_demand", test_equation, "isee")

  expected_obj  <- list(
    variable_list = list(
      list(
        name     = "adjust_expected_demand",
        equation = "(expected_demand_2-expected_demand)/2"),
      list(
        name     = "adjust_expected_demand_2",
        equation = "(expected_demand_3-expected_demand_2)/2"),
      list(
        name     = "adjust_expected_demand_3",
        equation = "(demand-expected_demand_3)/2")
    ),
    stock_list = list(
      list(
        name      = "expected_demand" ,
        equation  = "adjust_expected_demand",
        initValue = "demand"),
      list(
        name      = "expected_demand_2" ,
        equation  = "adjust_expected_demand_2",
        initValue = "demand"),
      list(
        name      = "expected_demand_3" ,
        equation  = "adjust_expected_demand_3",
        initValue = "demand")
    ),
    delay_order = 3)

  expect_equal(actual_obj, expected_obj)

})

test_that("translate_SMOOTH3() returns the expected object for an equation from
Vensim", {
  test_equation <- "SMOOTH3(demand, 6)"

  actual_obj    <- translate_SMOOTH3("expected_demand", test_equation, "Vensim",
                                     "SMOOTH3")

  expected_obj  <- list(
    variable_list = list(
      list(
        name     = "adjust_expected_demand",
        equation = "(expected_demand_2-expected_demand)/2"),
      list(
        name     = "adjust_expected_demand_2",
        equation = "(expected_demand_3-expected_demand_2)/2"),
      list(
        name     = "adjust_expected_demand_3",
        equation = "(demand-expected_demand_3)/2")
    ),
    stock_list = list(
      list(
        name      = "expected_demand" ,
        equation  = "adjust_expected_demand",
        initValue = "demand"),
      list(
        name      = "expected_demand_2" ,
        equation  = "adjust_expected_demand_2",
        initValue = "demand"),
      list(
        name      = "expected_demand_3" ,
        equation  = "adjust_expected_demand_3",
        initValue = "demand")
    ),
    delay_order = 3)

  expect_equal(actual_obj, expected_obj)
})

test_that("translate_SMOOTH3() returns the expected object for an equation from Vensim with initialisation", {
  test_equation <- "SMOOTH3I(0.5, 6, 1)"

  actual_obj    <- translate_SMOOTH3("S3", test_equation, "Vensim",
                                     "SMOOTH3I")

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
    ),
    delay_order = 3)

  expect_equal(actual_obj, expected_obj)
})

test_that("translate_SMTHN() returns the expected object for delays of order higher than 1", {
  test_equation <- "SMTHN(0.5, 8, 4,  1)"
  actual_obj    <- translate_SMOOTHN("SN", test_equation, "isee")

  expected_obj  <- list(
    variable_list = list(
        list(name     = "adjust_SN",
             equation = "(SN_2-SN)/2"),
        list(name     = "adjust_SN_2",
             equation = "(SN_3-SN_2)/2"),
        list(name     = "adjust_SN_3",
             equation = "(SN_4-SN_3)/2"),
        list(name     = "adjust_SN_4",
             equation = "(0.5-SN_4)/2")
    ),
    stock_list = list(
      list(name      = "SN",
           equation  = "adjust_SN",
           initValue = 1),
      list(name      = "SN_2",
           equation  = "adjust_SN_2",
           initValue = 1),
      list(name      = "SN_3",
           equation  = "adjust_SN_3",
           initValue = 1),
      list(name      = "SN_4",
           equation  = "adjust_SN_4",
           initValue = 1)
    ),
    delay_order = 4)

  expect_equal(actual_obj, expected_obj)

})

test_that("translate_SMTHN() returns the expected object for delays of order higher than 1 without initialisation", {
  test_equation <- "SMTHN(demand, 8, 4)"
  actual_obj    <- translate_SMOOTHN("expected_demand", test_equation, "isee")

  expected_obj  <- list(
    variable_list = list(
      list(
        name     = "adjust_expected_demand",
        equation = "(expected_demand_2-expected_demand)/2"),
      list(
        name     = "adjust_expected_demand_2",
        equation = "(expected_demand_3-expected_demand_2)/2"),
      list(
        name     = "adjust_expected_demand_3",
        equation = "(expected_demand_4-expected_demand_3)/2"),
      list(
        name     = "adjust_expected_demand_4",
        equation = "(demand-expected_demand_4)/2")
    ),
    stock_list = list(
      list(
        name      = "expected_demand" ,
        equation  = "adjust_expected_demand",
        initValue = "demand"),
      list(
        name      = "expected_demand_2" ,
        equation  = "adjust_expected_demand_2",
        initValue = "demand"),
      list(
        name      = "expected_demand_3" ,
        equation  = "adjust_expected_demand_3",
        initValue = "demand"),
      list(
        name      = "expected_demand_4" ,
        equation  = "adjust_expected_demand_4",
        initValue = "demand")
    ),
    delay_order = 4)

  expect_equal(actual_obj, expected_obj)

})

test_that("translate_SMTHN() returns the expected object for delays of order
equal to 1", {
  test_equation <- "SMTHN(0.5, 2, 1, 1)"
  actual_obj    <- translate_SMOOTHN("SN", test_equation, "isee")

  expected_obj  <- list(
    variable_list = list(
      list(name     = "adjust_SN",
           equation = "(0.5-SN)/2")),
    stock_list = list(
      list(name      = "SN",
           equation  = "adjust_SN",
           initValue = 1)),
    delay_order = 1)

  expect_equal(actual_obj, expected_obj)

})

test_that("translate_SMTHN() throws an error when the delay order parameter is not an integer", {
  test_equation <- "SMTHN(0.5, 8, param_order,  1)"
  expect_error(translate_SMOOTHN("SN", test_equation, "isee"),
               "The delay order parameter, n, must be an integer in variable SN")
})

test_that("stc_vars_SN() handles total_delay as a numeric input", {
  actual_obj <- stc_vars_SN("expected_demand", "demand", "6", 2,
                            "demand")

  expected_obj <-   expected_obj  <- list(
    variable_list = list(
      list(
        name     = "adjust_expected_demand",
        equation = "(expected_demand_2-expected_demand)/3"),

      list(
        name     = "adjust_expected_demand_2",
        equation = "(demand-expected_demand_2)/3")
    ),
    stock_list = list(
      list(
        name      = "expected_demand" ,
        equation  = "adjust_expected_demand",
        initValue = "demand"),
      list(
        name      = "expected_demand_2" ,
        equation  = "adjust_expected_demand_2",
        initValue = "demand")
    ),
    delay_order = 2)

  expect_equal(actual_obj, expected_obj)
})

test_that("stc_vars_SN() handles total_delay as a character input", {
  actual_obj <- stc_vars_SN("expected_demand", "demand", "param_delay", 2,
                            "demand")

  expected_obj <-   expected_obj  <- list(
    variable_list = list(
      list(
        name     = "adjust_expected_demand",
        equation = "(expected_demand_2-expected_demand)/(param_delay/2)"),

      list(
        name     = "adjust_expected_demand_2",
        equation = "(demand-expected_demand_2)/(param_delay/2)")
    ),
    stock_list = list(
      list(
        name      = "expected_demand" ,
        equation  = "adjust_expected_demand",
        initValue = "demand"),
      list(
        name      = "expected_demand_2" ,
        equation  = "adjust_expected_demand_2",
        initValue = "demand")
    ),
    delay_order = 2)

  expect_equal(actual_obj, expected_obj)
})
