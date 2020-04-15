context("Generate deSolve components")

# get_deSolve_elems()-----------------------------------------------------------
test_that("get_deSolve_elems() returns the basic elements", {
  test_structure <- list(
    parameters = NULL,
    levels = list(
      list(name      = "Population",
           equation  = "net_growth",
           initValue = 100)
    ),
    variables = list(
      list(name     = "net_growth",
           equation = "Population*growth_rate")
    ),
    constants = list(
      list(name  = "growth_rate",
           value = 0.01)
    ))
  expect_named(get_deSolve_elems(test_structure),
               c("stocks", "consts", "func"))
})

# construct_return_statement()--------------------------------------------------

test_that("construct_return_statement() works when there are no constants", {
  test_s <- list(list(name = "Price"))
  test_v <- list(list(name = "demand_price_schedule"))
  test_c <- list() # test constants
  actual   <- construct_return_statement(test_s, test_v, test_c)
  expected <- "return (list(c(d_Price_dt),\ndemand_price_schedule = demand_price_schedule))"
  expect_equal(actual, expected)
})

test_that("construct_return_statement() works when there are no variables", {
  test_s <- list(list(name = "Population"))
  test_v <- list() # test variables
  test_c <- list(list(name = "constant_growth")) # test constants
  actual   <- construct_return_statement(test_s, test_v, test_c)
  expected <- "return (list(c(d_Population_dt),\nconstant_growth = constant_growth))"
  expect_equal(actual, expected)
})
