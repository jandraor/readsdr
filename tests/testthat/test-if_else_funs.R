# Translate IF ELSE statements==================================================
context("Translate IF ELSE")

test_that("translate_ifelse() returns an R-equivalent equation for Stella's XMILE", {
  test_equation <- "IF(Backlog > supply_capacity)\nTHEN supply_capacity\nELSE Backlog"
  actual_val    <- translate_ifelse(test_equation, "isee")
  expected_val  <- "ifelse(Backlog > supply_capacity,  supply_capacity\n,  Backlog)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse() does not alter equations when there are no ifelse statements from Vensim XMILE", {
  test_equation <- "a + b"
  actual_val    <- translate_ifelse(test_equation, "Vensim")
  expected_val  <- "a + b"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse() does not alter equations when there are no ifelse statements from Stella XMILE", {
  test_equation <- "a + b"
  actual_val    <- translate_ifelse(test_equation, "isee")
  expected_val  <- "a + b"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse() returns an R-equivalent equation for Vensim's XMILE", {
  test_equation <- "IF_THEN_ELSE(Backlog > Supply_capacity, Supply_capacity, Backlog)"
  actual_val    <- translate_ifelse(test_equation, "Vensim")
  expected_val  <- "ifelse(Backlog > Supply_capacity, Supply_capacity, Backlog)"
  expect_equal(actual_val, expected_val)
})

# Translate STEP function=======================================================

context("Translate STEP")

test_that("translate_step() returns the correct translation for a simple STEP", {
  test_equation <- "STEP(10, 5)"
  actual_val    <- translate_step(test_equation)
  expected_val  <- "ifelse(time >= 5, 10, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_step() returns the correct translation for a sum with STEP", {
  test_equation <- "10 + STEP(10, 5)"
  actual_val    <- translate_step(test_equation)
  expected_val  <- "10 + ifelse(time >= 5, 10, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_step() returns the correct translation for multiple STEP", {
  test_equation <- "STEP(10, 5) + STEP(10, 10) + STEP(10, 20)"
  actual_val    <- translate_step(test_equation)
  expected_val  <- "ifelse(time >= 5, 10, 0) + ifelse(time >= 10, 10, 0) + ifelse(time >= 20, 10, 0)"
  expect_equal(actual_val, expected_val)
})

# Translate PULSE_TRAIN function================================================
context("Translate PULSE TRAIN")
test_that("translate_pulse_train() returns the correct translation for a simple PULSE_TRAIN", {
  test_equation <- "PULSE_TRAIN(5, 3, 10, 20)"
  actual_val    <- translate_pulse_train(test_equation)
  expected_val  <- "ifelse((time >= 5 & time < 8) | (time >= 15 & time < 18), 1, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("create_pt_condition() returns the correct condition", {
  test_equation <- "PULSE_TRAIN(5, 3, 10, 20)"
  pattern_pt    <- stringr::regex("PULSE_TRAIN\\((.+?),(.+?),(.+?),(.+?)\\)")
  actual_val    <- create_pt_condition(test_equation, pattern_pt)
  expected_val  <- "(time >= 5 & time < 8) | (time >= 15 & time < 18)"
  expect_equal(actual_val, expected_val)
})

test_that("create_pt_condition() returns the correct condition when the end of intervals is greater than the end time", {
  test_equation <- "PULSE_TRAIN(5, 3, 10, 17)"
  pattern_pt    <- stringr::regex("PULSE_TRAIN\\((.+?),(.+?),(.+?),(.+?)\\)")
  actual_val    <- create_pt_condition(test_equation, pattern_pt)
  expected_val  <- "(time >= 5 & time < 8) | (time >= 15 & time <= 17)"
  expect_equal(actual_val, expected_val)
})
