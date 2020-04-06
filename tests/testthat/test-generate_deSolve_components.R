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
