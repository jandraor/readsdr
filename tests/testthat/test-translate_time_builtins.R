test_that("translate_time_builtins() returns the expected translation", {
  test_equation <- "ifelse(TIME > 0, DT, 0)"
  actual_val    <- translate_time_builtins(test_equation)
  expected_val  <- "ifelse(time > 0, timestep(), 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_time() returns the expected translation", {
  test_equation <- "Time + startTime"
  actual_val    <- translate_time(test_equation)
  expected_val  <- "time + startTime"
  expect_equal(actual_val, expected_val)
})

test_that("translate_dt() returns the expected translation", {
  test_equation <- "volume / DT"
  actual_val    <- translate_dt(test_equation)
  expected_val  <- "volume / timestep()"
  expect_equal(actual_val, expected_val)
})
