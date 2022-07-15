test_that("arrange_variables() returns variables in computational order", {

  variables <- list(
    list(name = "births",
         equation = "population*birthRate"),
    list(name = "birthRate",
         equation = "birthRate2")
  )

  ordered_vars <- arrange_variables(variables)
  expected_list <- c(variables[2], variables[1])
  expect_equal(ordered_vars, expected_list)
})

test_that("arrange_variables() works when there are duplicated equations", {

  variables <- list(
    list(name = "births",
         equation = "population*(growth_rate_1+growth_rate_2)"),
    list(name = "growth_rate_1",
         equation = "growth_rate_base"),
    list(name = "growth_rate_2",
         equation = "growth_rate_base")
  )
  expected_list <- c(variables[2], variables[3], variables[1])
  ordered_vars <- arrange_variables(variables)
  expect_equal(ordered_vars, expected_list)
})

test_that("arrange_variables() returns an empty list if the input is an empty list", {
  variables    <- list()
  actual_val   <- arrange_variables(list())
  expected_val <- list()
  expect_equal(actual_val, expected_val)
})

test_that("arrange_variables() returns an error should the rhs does not contain variables", {

  test_list <- list(list(name = "c", equation = "a+b"),
                    list(name = "a", equation = "3"))

  expect_error(arrange_variables(test_list))
})
