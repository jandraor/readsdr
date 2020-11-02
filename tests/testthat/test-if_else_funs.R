# Translate IF ELSE statements==================================================
context("Translate IF-ELSE-like statements")

# Stella
test_that("translate_ifelse() returns an R-equivalent equation for Stella's XMILE", {
  test_equation <- "IF(Backlog > supply_capacity)\nTHEN supply_capacity\nELSE Backlog"
  actual_val    <- translate_ifelse(test_equation, "isee")
  expected_val  <- "ifelse(Backlog > supply_capacity,  supply_capacity\n,  Backlog)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse() handles statements without parentheses for Stella's XMILE", {
  test_equation <- "IF TIME > 240 THEN effect ELSE 1"
  actual_val    <- translate_ifelse(test_equation, "isee")
  expected_val  <- "ifelse( TIME > 240 ,  effect ,  1)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse() does not alter equations when there are no ifelse
statements from Stella XMILE", {
  test_equation <- "a + b"
  actual_val    <- translate_ifelse(test_equation, "isee")
  expected_val  <- "a + b"
  expect_equal(actual_val, expected_val)
})

test_that("if_else_condition() handles conditions with parentheses", {
  test_equation <- "IF(NOT (TIME = 3)) THEN 0 ELSE 1"
  actual_val    <- if_else_condition(test_equation)
  expected_val  <- "NOT (TIME = 3)"
  expect_equal(actual_val, expected_val)
})

test_that("if_else_condition() handles conditions without parentheses", {
  test_equation <- "IF NOT (TIME = 3) THEN 0 ELSE 1"
  actual_val    <- if_else_condition(test_equation)
  expected_val  <- " NOT (TIME = 3) "
  expect_equal(actual_val, expected_val)
})

# Vensim
test_that("translate_ifelse() does not alter equations when there are no ifelse
statements from Vensim", {
  test_equation <- "a + b"
  actual_val    <- translate_ifelse(test_equation, "Vensim")
  expected_val  <- "a + b"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse() returns an R-equivalent equation for Vensim's XMILE", {
  test_equation <- "IF_THEN_ELSE(Backlog > Supply_capacity, Supply_capacity, Backlog)"
  actual_val    <- translate_ifelse(test_equation, "Vensim")
  expected_val  <- "ifelse(Backlog > Supply_capacity, Supply_capacity, Backlog)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse deals with cases for equations from Vensim", {
  test_equation <- "if_Then_eLse(B > S, S, B)"
  expected_val  <- "ifelse(B > S, S, B)"
  expect_equal(translate_ifelse(test_equation, "Vensim"), expected_val)
})

# Both

test_that("translate_ifelse throws an error if there is more than one
statement", {
  test_equation <- "IF_THEN_ELSE(a > b, 1, 0) + IF_THEN_ELSE(c > d, 1, 0)"
  expect_error(translate_ifelse(test_equation, "Vensim"),
               "Only one IF-ELSE statement per variable is permitted")

  test_equation <- "(IF(TIME>1) THEN 1 ELSE 0) + (IF(TIME>2) THEN 1 ELSE 0)"
  expect_error(translate_ifelse(test_equation, "isee"),
               "Only one IF-ELSE statement per variable is permitted")
})

# Translate STEP function=======================================================

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

test_that("translate_step() ignores cases", {
  test_equation <- "Step(10, 5)"
  actual_val    <- translate_step(test_equation)
  expected_val  <- "ifelse(time >= 5, 10, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_step() deals with breaklines", {
  test_equation <- "STEP(10,\n5)"
  actual_val    <- translate_step(test_equation)
  expected_val  <- "ifelse(time >=\n5, 10, 0)"
  expect_equal(actual_val, expected_val)
})

# Translate PULSE_TRAIN function================================================

test_that("translate_pulse_train() returns the correct translation for a simple PULSE_TRAIN", {
  test_equation <- "PULSE_TRAIN(5, 3, 10, 20)"
  actual_val    <- translate_pulse_train(test_equation)
  expected_val  <- "sd_pulse_train(time, 5, 3, 10, 20)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse_train() returns the correct translation for a parameterised PULSE_TRAIN", {
  test_equation <- "PULSE_TRAIN(a, b, c, d)"
  actual_val    <- translate_pulse_train(test_equation)
  expected_val  <- "sd_pulse_train(time, a, b, c, d)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse_train() ignores cases", {
  test_equation <- "pULsE_TRAIn(5, 3, 10, 20)"
  actual_val    <- translate_pulse_train(test_equation)
  expected_val  <- "sd_pulse_train(time, 5, 3, 10, 20)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse_train() deals with breaklines", {
  test_equation <- "PULSE_TRAIN(5,\n3,\n10,\n20)"
  actual_val    <- translate_pulse_train(test_equation)
  expected_val  <- "sd_pulse_train(time, 5,\n3,\n10,\n20)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse_train() throws an error should there is more than
one statement", {
  test_equation <- "PULSE_TRAIN(5, 3, 10, 20) + PULSE_TRAIN(5, 3, 10, 20)"
  expect_error(translate_pulse_train(test_equation),
               "Only one PULSE_TRAIN statement per variable is permitted")
})

# Translate PULSE function from Vensim==========================================

test_that("translate_pulse() returns the correct translation for a PULSE from
          Vensim with width equal to 0", {
  test_equation <- "PULSE(1, 0)"
  actual_val    <- translate_pulse(test_equation, "Vensim")
  expected_val  <- "ifelse(time == 1, 1, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() returns the correct translation for a PULSE
          from Vensim with width greater than 0", {
  test_equation <- "PULSE(1, 1)"
  actual_val    <- translate_pulse(test_equation, "Vensim")
  expected_val  <- "ifelse(time >= 1 & time < 2, 1, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() returns the correct translation for a compounded
          expression that includes a PULSE from Vensim", {
  test_equation <- "0.1 * PULSE(1, 0)"
  actual_val    <- translate_pulse(test_equation, "Vensim")
  expected_val  <- "0.1 * ifelse(time == 1, 1, 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() returns the correct translation for a parameterised
PULSE from Vensim", {
  test_equation <- "PULSE(startTime,duration)"
  actual_val    <- translate_pulse(test_equation, "Vensim")
  expected_val  <- "sd_pulse_v(time,startTime,duration)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() deals with cases from a equation from Vensim", {
  test_equation <- "Pulse(1, 0)"
  expect_equal(translate_pulse(test_equation, "Vensim"),
               "ifelse(time == 1, 1, 0)")
})

test_that("translate_pulse throws an error should there is more than
one statement", {
  test_equation <- "PULSE(1, 0) + PULSE(1, 0)"
  expect_error(translate_pulse(test_equation, "Vensim"),
               "Only one PULSE statement per variable is permitted")
  expect_error(translate_pulse(test_equation, "isee"),
               "Only one PULSE statement per variable is permitted")
})

# Translate PULSE from Stella===================================================

test_that("translate_pulse() returns the correct translation for a PULSE with
          three numeric arguments from Stella and interval equal to 0", {
  test_equation <- "PULSE(0.1, 0, 0)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "ifelse(time == 0, 0.1 / timestep(), 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() returns the correct translation for a PULSE with
          three arguments from Stella, interval 0, and an argument as variable", {
  test_equation <- "PULSE(height_pulse, time_pulse, 0)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "ifelse(time == time_pulse, height_pulse / timestep(), 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() ignores cases", {
  test_equation <- "PUlSE(0.1, 0, 0)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "ifelse(time == 0, 0.1 / timestep(), 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() ignores breaklines", {
  test_equation <- "PULSE(0.1,\n0,\n0)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "ifelse(time ==\n0, 0.1 / timestep(), 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() returns the correct translation for a PULSE with
          three numeric arguments from Stella and interval higher than 0", {
  test_equation <- "PULSE(0.1, 2, 3)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "ifelse(time %in% seq( 2, max(time,  2), 3), 0.1 / timestep(), 0)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() returns the correct translation for a PULSE with
three numeric arguments from Stella and interval higher than 0", {

  test_equation <- "PULSE(0.1, 2, interval_var)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "sd_pulse_s(time, 0.1, 2, interval_var)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() returns the correct translation for a compounded
PULSE with three numeric arguments from Stella and interval higher than 0", {

  test_equation <- "test_var + PULSE(0.1, 2, interval_var)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "test_var + sd_pulse_s(time, 0.1, 2, interval_var)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_pulse() returns the correct translation for a PULSE with
two arguments from Stella", {

  test_equation <- "PULSE(0.1, 2)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "ifelse(time >=  2, 0.1 / timestep(), 0)"
  expect_equal(actual_val, expected_val)

})

test_that("translate_pulse() returns the correct translation for a compounded
PULSE with two arguments from Stella", {

  test_equation <- "a + PULSE(0.1, 2)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "a + ifelse(time >=  2, 0.1 / timestep(), 0)"
  expect_equal(actual_val, expected_val)

})

test_that("translate_pulse() returns the correct translation for a PULSE with
one argument from Stella", {

  test_equation <- "PULSE(0.1)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "0.1 / timestep()"
  expect_equal(actual_val, expected_val)

})

test_that("translate_pulse() returns the correct translation for a compounded
PULSE with one argument from Stella", {

  test_equation <- "a + PULSE(0.1)"
  actual_val    <- translate_pulse(test_equation, "isee")
  expected_val  <- "a + 0.1 / timestep()"
  expect_equal(actual_val, expected_val)

})

test_that("get_pulse_s_statement() returns the correct statement", {
  actual_val   <- get_pulse_s_statement(1, 2, 0)
  expected_val <- "ifelse(time ==2, 1 / timestep(), 0)"
  expect_equal(actual_val, expected_val)
})

test_that("get_pulse_v_statement() returns the correct statement", {
  actual_val   <- get_pulse_v_statement(1, 2)
  expected_val <- "ifelse(time >= 1 & time < 3, 1, 0)"
  expect_equal(actual_val, expected_val)
})



