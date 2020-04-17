context("Translate extrema")


test_that("translate_MIN() returns the expected translation", {
  expect_equal(translate_MIN("Min(0,1)"), "min(0,1)")
})

test_that("translate_MAX() returns the expected translation", {
  expect_equal(translate_MAX("mAx(0,1)"), "max(0,1)")
})
