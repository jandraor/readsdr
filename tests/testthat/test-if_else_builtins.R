context("sd_pulse_train")

test_that("sd_pulse_train() returns 1 at the start of the pulse", {
  actual_val   <- sd_pulse_train(5, 5, 3, 10, 20)
  expected_val <- 1
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_train() returns 1 at the middle of the first pulse", {
  actual_val   <- sd_pulse_train(6.5, 5, 3, 10, 20)
  expected_val <- 1
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_train() returns 0 after the end of the first pulse", {
  actual_val   <- sd_pulse_train(8, 5, 3, 10, 20)
  expected_val <- 0
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_train() returns 0 after the sequence end", {
  actual_val   <- sd_pulse_train(21, 5, 3, 10, 20)
  expected_val <- 0
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_train() returns 1 if the interval is cut", {
  actual_val   <- sd_pulse_train(17, 5, 3, 10, 17)
  expected_val <- 1
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_train() returns 0 for non-integer increments", {
  actual_val   <- sd_pulse_train(6.3, 5, 0, 0.8, 10)
  expected_val <- 0
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_train() returns 1 for non-integer increments", {
  actual_val   <- sd_pulse_train(5.8, 5, 0, 0.8, 10)
  expected_val <- 1
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_train() works for intervals of size 0", {
  actual_val   <- sd_pulse_train(6, 5, 0, 1, 10)
  expected_val <- 1
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_train() deals with values after the end ", {
  actual_val   <- sd_pulse_train(22, 2, 3, 10, 20)
  expected_val <- 0
  expect_equal(actual_val, expected_val)
})

context("create_pt_statement")

test_that("create_pt_statement() returns the correct condition", {
  actual_val    <- create_pt_statement(5, 3, 10, 20)
  expected_val  <- "ifelse((time >= 5 & time < 8) | (time >= 15 & time < 18), 1, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("create_pt_statement() returns the correct condition when the end of intervals is greater than the end time", {
  actual_val    <- create_pt_statement(5, 3, 10, 17)
  expected_val  <- "ifelse((time >= 5 & time < 8) | (time >= 15 & time <= 17), 1, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("create_pt_statement() deals with intervals equal to 0", {
  actual_val    <- create_pt_statement(5, 0, 1, 10)
  expected_val  <- "ifelse(time %in% seq(5, 10, 1), 1, 0)"
  expect_equal(actual_val, expected_val)
})

# sd_pulse_s ===================================================================
context("sd_pulse_s")

test_that("sd_pulse_s() returns 1 in an interval equal to zero", {
  assign("timestep", function() 0.25, envir = .GlobalEnv)
  actual_val    <- sd_pulse_s(2, 1, 2, 0)
  rm("timestep", envir = .GlobalEnv) # clean up
  expected_val  <- 4
  expect_equal(actual_val, expected_val)
})

test_that("sd_pulse_s() returns 0  in an interval equal to zero", {
  timestep      <- function() 0.25
  actual_val    <- sd_pulse_s(1, 1, 2, 0)
  expected_val  <- 0
  expect_equal(actual_val, expected_val)
})

# sd_pulse_v ===================================================================
context("sd_pulse_v")

test_that("sd_pulse_v() behaves like a pulse from Vensim", {
  expect_equal(sd_pulse_v(0, 1, 2), 0)
  expect_equal(sd_pulse_v(1, 1, 2), 1)
  expect_equal(sd_pulse_v(2, 1, 2), 1)
  expect_equal(sd_pulse_v(3, 1, 2), 0)
  expect_equal(sd_pulse_v(4, 1, 2), 0)
})
