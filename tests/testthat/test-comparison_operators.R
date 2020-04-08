context("Translate equal sign")

test_that("translate_equal_sign() translates equal operator", {
  actual_val <- translate_equal_sign('a = b')
  expected_val <- 'a == b'
  expect_equal(actual_val, expected_val)
})

test_that("translate_equal_sign() does not misinterpret greater or equal to", {
  actual_val <- translate_equal_sign('a >= b')
  expected_val <- 'a >= b'
  expect_equal(actual_val, expected_val)
})

test_that("translate_equal_sign ignores the correct equal operator", {
  actual_val <- translate_equal_sign('a == b')
  expected_val <- 'a == b'
  expect_equal(actual_val, expected_val)
})

test_that("translate_equal_sign ignores the not equal operator", {
  actual_val <- translate_equal_sign('a != b')
  expected_val <- 'a != b'
  expect_equal(actual_val, expected_val)
})

context("Translate not equal sign")

test_that("translate_not_equal_sign() translates the not equal operator", {
  actual_val <- translate_not_equal_sign('a <> b')
  expected_val <- 'a != b'
  expect_equal(actual_val, expected_val)
})
