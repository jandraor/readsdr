test_that("arrange_variables returns variables in computational order", {
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
