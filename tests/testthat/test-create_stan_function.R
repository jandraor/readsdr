
test_that("create_stan_function() returns the expected string", {
  expected_stan_function <- paste(
    "functions {",
    "  real[] lotka_volterra(real t,",
    "              real[] y,",
    "              real[] params,",
    "              real[] x_r,",
    "              int[] x_i) {",
    "  real dydt[2];",
    "  real Bx;",
    "  real Dx;",
    "  real By;",
    "  real Dy;",
    "  Bx = 1*y[1];",
    "  Dx = 0.2*y[1]*y[2];",
    "  By = 0.04*y[1]*y[2];",
    "  Dy = 0.5*y[2];",
    "  dydt[1] = Bx-Dx;",
    "  dydt[2] = By-Dy;",
    "  return dydt;",
    "  }",
    "}",
    sep = "\n")

  file <- "./lotka_volterra.stmx"
  stan_function <- create_stan_function(file, "lotka_volterra")
  expect_equal(stan_function, expected_stan_function)
})

test_that("create_stan_function() returns a string", {
  file <- "./lotka_volterra.stmx"
  stan_function <- create_stan_function(file, "test_model")
  expect_is(stan_function, "character")
})

test_that("create_stan_function() assign the expected name to the function", {
  file <- "./lotka_volterra.stmx"
  stan_function <- create_stan_function(file, "lotka_volterra")
  second_line <- strsplit(stan_function, "\n")[[1]][[2]]
  expected_line <-   "  real[] lotka_volterra(real t,"
  expect_equal(second_line, expected_line)
})

test_that("create_stan_function() declares the exact number of stocks", {
  file <- "./lotka_volterra.stmx"
  stan_function <- create_stan_function(file, "lotka_volterra")
  seventh_line <- strsplit(stan_function, "\n")[[1]][[7]]
  expected_line <-   "  real dydt[2];"
  expect_equal(seventh_line, expected_line)
})

test_that("create_stan_function() declares the expected variables", {
  file <- "./lotka_volterra.stmx"
  stan_function <- create_stan_function(file, "lotka_volterra")
  eighth_line   <- strsplit(stan_function, "\n")[[1]][[8]]
  ninth_line    <- strsplit(stan_function, "\n")[[1]][[9]]
  tenth_line    <- strsplit(stan_function, "\n")[[1]][[10]]
  eleventh_line <- strsplit(stan_function, "\n")[[1]][[11]]
  expected_8th_line  <-   "  real Bx;"
  expected_9th_line  <-   "  real Dx;"
  expected_10th_line <-   "  real By;"
  expected_11th_line <-   "  real Dy;"
  expect_equal(eighth_line, expected_8th_line)
  expect_equal(ninth_line, expected_9th_line)
  expect_equal(tenth_line, expected_10th_line)
  expect_equal(eleventh_line, expected_11th_line)
})

test_that("create_stan_function() generates the expected auxiliar equations", {
  file <- "./lotka_volterra.stmx"
  stan_function <- create_stan_function(file, "lotka_volterra")
  actual_12th_line    <- strsplit(stan_function, "\n")[[1]][[12]]
  actual_13th_line    <- strsplit(stan_function, "\n")[[1]][[13]]
  actual_14th_line    <- strsplit(stan_function, "\n")[[1]][[14]]
  actual_15th_line    <- strsplit(stan_function, "\n")[[1]][[15]]
  expected_12th_line  <- "  Bx = 1*y[1];"
  expected_13th_line  <- "  Dx = 0.2*y[1]*y[2];"
  expected_14th_line  <- "  By = 0.04*y[1]*y[2];"
  expected_15th_line  <- "  Dy = 0.5*y[2];"
  expect_equal(actual_12th_line, expected_12th_line)
  expect_equal(actual_13th_line, expected_13th_line)
  expect_equal(actual_14th_line, expected_14th_line)
  expect_equal(actual_15th_line, expected_15th_line)

})

test_that("create_stan_function() generates the expected differential equations", {
  file <- "./lotka_volterra.stmx"
  stan_function <- create_stan_function(file, "lotka_volterra")
  actual_16th_line    <- strsplit(stan_function, "\n")[[1]][[16]]
  actual_17th_line    <- strsplit(stan_function, "\n")[[1]][[17]]
  expected_16th_line  <-   "  dydt[1] = Bx-Dx;"
  expected_17th_line  <-   "  dydt[2] = By-Dy;"
  expect_equal(actual_16th_line, expected_16th_line)
  expect_equal(actual_17th_line, expected_17th_line)
})


test_that("create_stan_function() parameterise the function", {
  file <- "./lotka_volterra.stmx"
  stan_function <- create_stan_function(file, "lotka_volterra",
                                        pars = c("d", "a"))
  actual_12th_line   <- strsplit(stan_function, "\n")[[1]][[12]]
  actual_15th_line   <- strsplit(stan_function, "\n")[[1]][[15]]
  expected_12th_line  <-   "  Bx = params[2]*y[1];"
  expected_15th_line  <-   "  Dy = params[1]*y[2];"
  expect_equal(actual_12th_line, expected_12th_line)
  expect_equal(actual_15th_line, expected_15th_line)
})

test_that("create_stan_function() returns equations in computational  order", {
  file           <- "./pop_extended_consts.stmx"
  stan_function <- create_stan_function(file, "test_model")

  expected_function <- paste(
    "functions {",
    "  real[] test_model(real t,",
    "              real[] y,",
    "              real[] params,",
    "              real[] x_r,",
    "              int[] x_i) {",
    "  real dydt[1];",
    "  real birthRate;",
    "  real births;",
    "  birthRate = 0.1;",
    "  births = y[1]*birthRate;",
    "  dydt[1] = births;",
    "  return dydt;",
    "  }",
    "}", sep = "\n")

    expect_equal(stan_function, expected_function)
})


