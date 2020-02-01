
test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant", {
  equation      <- "c"
  auxs          <- list(list(name = "c", equation = "100"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a one-level nested equation", {
  equation      <- "c"
  auxs          <- list(
    list(name = "c", equation = "c1 + c2"),
    list(name = "c1", equation = "80"),
    list(name = "c2", equation = "20"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a two-level nested equation", {
  equation      <- "c"
  auxs          <- list(
    list(name = "c", equation = "c1 + c2"),
    list(name = "c1", equation = "c3 * c4"),
    list(name = "c2", equation = "c5 / c6"),
    list(name = "c3", equation = "20"),
    list(name = "c4", equation = "4"),
    list(name = "c5", equation = "40"),
    list(name = "c6", equation = "2"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant and variable", {
  equation      <- "3 + c"
  auxs          <- list(
    list(name = "c", equation = "97"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant and a one-level nested variable", {
  equation      <- "5 * c"
  auxs          <- list(
    list(name = "c", equation = "c1 - c2"),
    list(name = "c1", equation = "27"),
    list(name = "c2", equation = "7"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_elem_name() returns the sanitised name when it has a breakline in between", {
  actual_val <- sanitise_elem_name("flow\\ntest")
  expected_val <- "flow_test"
  expect_equal(actual_val, expected_val)
})
