test_that("sd_impact_inputs() returns the expected list", {

  filepath  <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl       <- read_xmile(filepath)
  desc_list <- mdl$description

  actual    <- sd_impact_inputs(desc_list)

  expected_flows <- data.frame(
    stock    = c("Susceptible", rep("Infected", 2), "Recovered"),
    flow     = c("IR", "IR", "RR", "RR"),
    sign     = c("-", "+", "-", "+"),
    equation = c("(Susceptible*Infected*((c*i)/population))",
                 "(Susceptible*Infected*((c*i)/population))",
                 "(Infected/recoveryDelay)",
                 "(Infected/recoveryDelay)"))

  expected_pathways <- data.frame(
    from    = c("Susceptible", "Infected", "Susceptible", "Infected", "Infected", "Infected"),
    to      = c("Susceptible", "Susceptible", "Infected", "Infected", "Infected", "Recovered"),
    through = c("IR", "IR", "IR", "IR", "RR", "RR"))

  expected_velocities <- data.frame(
    stock    = c("Susceptible", "Infected", "Recovered"),
    equation = c("-(Susceptible*Infected*((c*i)/population))",
      "(Susceptible*Infected*((c*i)/population))-(Infected/recoveryDelay)",
      "(Infected/recoveryDelay)"))

  expected <- list(flows      = expected_flows,
                   pathways   = expected_pathways,
                   velocities = expected_velocities)

  expect_equal(actual, expected)
})

test_that("velocity_equations() return the expected data frame", {

  filepath  <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl       <- read_xmile(filepath)
  desc_list <- mdl$description
  actual    <- velocity_equations(desc_list)
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

test_that("flow_equations() returns the expected data frame", {

  filepath  <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl       <- read_xmile(filepath)
  desc_list <- mdl$description

  actual    <- flow_equations(desc_list)

  expected <- data.frame(
    stock    = c("Susceptible", rep("Infected", 2), "Recovered"),
    flow     = c("IR", "IR", "RR", "RR"),
    sign     = c("-", "+", "-", "+"),
    equation = c("(Susceptible*Infected*((c*i)/population))",
                 "(Susceptible*Infected*((c*i)/population))",
                 "(Infected/recoveryDelay)",
                 "(Infected/recoveryDelay)"))

  expect_equal(actual, expected)
})

test_that("decompose_net_flow() returns the expected data frame", {

  eq       <- "f1-f2"
  actual   <- decompose_net_flow(eq)
  expected <- data.frame(flow = c("f1", "f2"), sign = c("+", "-"))

  expect_equal(actual, expected)
})

test_that("pathways() return the expected data frame", {

  filepath  <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl       <- read_xmile(filepath)
  desc_list <- mdl$description

  flows_df  <- flow_equations(desc_list)

  actual    <- pathways(flows_df)

  expected  <- data.frame(
    from    = c("Susceptible", "Infected", "Susceptible", "Infected", "Infected", "Infected"),
    to      = c("Susceptible", "Susceptible", "Infected", "Infected", "Infected", "Recovered"),
    through = c("IR", "IR", "IR", "IR", "RR", "RR"))

  expect_equal(actual, expected)
})


