test_that("sd_velocities() return the expected data frame", {

  filepath  <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl       <- read_xmile(filepath)
  desc_list <- mdl$description
  actual    <- sd_velocities(desc_list)
  expect_s3_class(actual, "data.frame")

  actual_colnames   <- colnames(actual)
  expected_colnames <- c("stock", "equation")

  expect_equal(actual_colnames, expected_colnames)

  actual_stock_names   <- actual$stock
  expected_stock_names <- c("Susceptible", "Infected", "Recovered")

  expect_equal(actual_stock_names , expected_stock_names)

  actual_equations   <- actual$equation
  expected_equations <- c("-(Susceptible*Infected*((c*i)/population))",
                          "(Susceptible*Infected*((c*i)/population))-(Infected/recoveryDelay)",
                          "(Infected/recoveryDelay)")

  expect_equal(actual_equations, expected_equations)
})

test_that("construct_velocity_equation() returns the expected equation", {

  filepath  <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl       <- read_xmile(filepath)
  desc_list <- mdl$description
  lvl_obj   <- desc_list$levels[[1]]

  levels      <- desc_list$levels
  stock_names <- purrr::map_chr(levels, "name")
  consts      <- desc_list$constants
  const_names <- purrr::map_chr(consts, "name")
  elem_names  <- c(stock_names, const_names)
  var_obj     <- desc_list$variables

  actual_equation <- construct_velocity_equation(lvl_obj, elem_names, var_obj)

  expect_type(actual_equation, "character")

  expected_equation <- "-(Susceptible*Infected*((c*i)/population))"

  expect_equal(actual_equation, expected_equation)
})
