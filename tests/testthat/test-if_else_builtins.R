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
