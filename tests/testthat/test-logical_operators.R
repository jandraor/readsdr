context("Translate AND")

test_that("translate_AND() returns for the correct translation for expressions from VENSIM", {
  test_equation <- "a>b:AND:a>c"
  actual_val    <- translate_AND(test_equation, "Vensim")
  expected_val  <- 'a>b&a>c'
  expect_equal(actual_val, expected_val)
})

test_that("translate_AND() returns for the correct translation for expressions from Stella", {
  test_equation <- "ifelse(a>5AND(a<10),1,0)"
  actual_val    <- translate_AND(test_equation, "isee")
  expected_val  <- 'ifelse(a>5&(a<10),1,0)'
  expect_equal(actual_val, expected_val)
})

test_that("translate_AND() returns for the correct translation for compounded expressions from Stella", {
  test_equation <- "ifelse(a>5AND(a<10)AND(a<15),0.1,0)"
  actual_val    <- translate_AND(test_equation, "isee")
  expected_val  <- "ifelse(a>5&(a<10)&(a<15),0.1,0)"
  expect_equal(actual_val, expected_val)
})

context("Translate OR")

test_that("translate_OR() returns for the correct translation for expressions from VENSIM", {
  test_equation <- "a>b:OR:a>c"
  actual_val    <- translate_OR(test_equation, "Vensim")
  expected_val  <- 'a>b|a>c'
  expect_equal(actual_val, expected_val)
})

test_that("translate_OR() returns for the correct translation for expressions from Stella", {
  test_equation <- "ifelse(a>5OR(a<10),1,0)"
  actual_val    <- translate_OR(test_equation, "isee")
  expected_val  <- 'ifelse(a>5|(a<10),1,0)'
  expect_equal(actual_val, expected_val)
})

test_that("translate_OR() dealw with cases for expressions from Stella", {
  test_equation <- "ifelse(a>5or(a<10),1,0)"
  actual_val    <- translate_OR(test_equation, "isee")
  expected_val  <- 'ifelse(a>5|(a<10),1,0)'
  expect_equal(actual_val, expected_val)
})

test_that("translate_OR() returns for the correct translation for compounded expressions from Stella", {
  test_equation <- "ifelse(a>5OR(a<10)OR(a<15),0.1,0)"
  actual_val    <- translate_OR(test_equation, "isee")
  expected_val  <- "ifelse(a>5|(a<10)|(a<15),0.1,0)"
  expect_equal(actual_val, expected_val)
})

context("Translate NOT")

test_that("translate_NOT() returns for the correct translation for expressions from Stella", {
  test_equation <- "NOT(time=3)"
  actual_val    <- translate_NOT(test_equation, "isee")
  expected_val  <- '!(time=3)'
  expect_equal(actual_val, expected_val)
})

test_that("translate_NOT() returns for the correct translation for compounded expressions from Stella", {
  test_equation <- "ifelse(NOT(time=3),0,1)"
  actual_val    <- translate_NOT(test_equation, "isee")
  expected_val  <- 'ifelse(!(time=3),0,1)'
  expect_equal(actual_val, expected_val)
})

test_that("translate_NOT() returns for the correct translation for expressions from Vensim", {
  test_equation <- "ifelse(:NOT:Time=3,0,1)"
  actual_val    <- translate_NOT(test_equation, "Vensim")
  expected_val  <- 'ifelse(!Time=3,0,1)'
  expect_equal(actual_val, expected_val)
})

