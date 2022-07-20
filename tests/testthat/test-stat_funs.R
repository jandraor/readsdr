test_that("translate_NORMAL returns the expected string", {
  expect_equal(translate_NORMAL("NORMAL(1,10)", "isee"),
               "rnorm(1,1,10)")

  expect_equal(translate_NORMAL("x + NORMAL(1,10)", "isee"),
               "x + rnorm(1,1,10)")
})

test_that("translate_NORMAL throws an error when NORMAL has more than two params", {
  expect_error(translate_NORMAL("NORMAL(0,1,1)", "isee"),
               "readsdr is restricted to translate NORMAL functions with only two parameters: mean, std_dev.")
})

test_that("translate_NORMAL returns the expected string for an equation from Vensim", {

  equation <- "RANDOM_NORMAL(0,200,Mean_of_Demand,Sd_of_Demand,0)"
  actual   <- translate_NORMAL(equation, "Vensim")
  expected <- "truncnorm::rtruncnorm(1,0,200,Mean_of_Demand,Sd_of_Demand)"

  expect_equal(actual, expected)
})
