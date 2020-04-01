
test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant", {
  stock_name    <- "test_stock"
  equation      <- "c"
  auxs          <- list(list(name = "c", equation = "100"))
  actual_val    <- compute_init_value(stock_name, equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a one-level nested equation", {
  stock_name    <- "test_stock"
  equation      <- "c"
  auxs          <- list(
    list(name = "c", equation = "c1 + c2"),
    list(name = "c1", equation = "80"),
    list(name = "c2", equation = "20"))
  actual_val    <- compute_init_value(stock_name, equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a two-level nested equation", {
  stock_name    <- "test_stock"
  equation      <- "c"
  auxs          <- list(
    list(name = "c", equation = "c1 + c2"),
    list(name = "c1", equation = "c3 * c4"),
    list(name = "c2", equation = "c5 / c6"),
    list(name = "c3", equation = "20"),
    list(name = "c4", equation = "4"),
    list(name = "c5", equation = "40"),
    list(name = "c6", equation = "2"))
  actual_val    <- compute_init_value(stock_name, equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant and variable", {
  stock_name    <- "test_stock"
  equation      <- "3 + c"
  auxs          <- list(
    list(name = "c", equation = "97"))
  actual_val    <- compute_init_value(stock_name, equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant and a one-level nested variable", {
  stock_name    <- "test_stock"
  equation      <- "5 * c"
  auxs          <- list(
    list(name = "c", equation = "c1 - c2"),
    list(name = "c1", equation = "27"),
    list(name = "c2", equation = "7"))
  actual_val    <- compute_init_value(stock_name, equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() calculates the initial value in an equation when all variables are defined by constants", {
  stock_name         <- "test_stock"
  test_equation      <- "ey*ep/eyvm"
  test_auxs          <- list(
    list(name = "ey", equation = "20000"),
    list(name = "ep", equation = "1"),
    list(name = "eyvm", equation = "5"))
  actual_val    <- compute_init_value(stock_name, test_equation, test_auxs)
  expected_val  <- 4000
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() calculates the initial value when there are graph function along the process", {
  stock_name         <- "Population"
  test_equation      <- "desired_init"
  test_auxs          <- list(
    list(name = "desired_init", equation = "value_scaled_down * scale_up"),
    list(name = "scale_up", equation = "2"),
    list(name      = "value_scaled_down",
         equation  = "f_value_scaled_down(init_value)",
         graph_fun = list(
           name = "f_value_scaled_down",
           fun  = approxfun(
             x = seq(0, 200, 50),
             y = seq(0, 200, 50) / 2,
             method = "linear",
             yleft  = 0,
             yright = 100)
         )),
    list(name = "init_value", equation = "100"))
  actual_val    <- compute_init_value(stock_name, test_equation, test_auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})
#===============================================================================

test_that("sanitise_elem_name() returns the sanitised name when it has a breakline in between", {
  actual_val <- sanitise_elem_name("flow\\ntest")
  expected_val <- "flow_test"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_elem_name() removes commentaries", {
  actual_val <- sanitise_elem_name("\n\t\t\t\t\t5800\n{(nic*ey)}\n\t\t\t\t\t")
  expected_val <- "5800"
  expect_equal(actual_val, expected_val)
})

#===============================================================================
test_that("sanitise_aux_equation() removes comentaries at the beginning", {
  actual_val <- sanitise_aux_equation("{0}1")
  expected_val <- "1"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() removes comentaries at the end", {
  actual_val <- sanitise_aux_equation("1{}")
  expected_val <- "1"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() removes comentaries at the beginning and at the end", {
  actual_val <- sanitise_aux_equation("{comment1}1{comment2}")
  expected_val <- "1"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() set min function in lower case", {
  actual_val <- sanitise_aux_equation("MIN(a,b)")
  expected_val <- "min(a,b)"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() set maxfunction in lower case", {
  actual_val <- sanitise_aux_equation("MAX(a,b)")
  expected_val <- "max(a,b)"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates AND operator", {
  actual_val <- sanitise_aux_equation('a > b :AND: a > c')
  expected_val <- 'a>b&a>c'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates OR operator", {
  actual_val <- sanitise_aux_equation('a > b :OR: a > c')
  expected_val <- 'a>b|a>c'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates = operator", {
  actual_val <- sanitise_aux_equation('a = b')
  expected_val <- 'a==b'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() does not misinterpret greater or equal to", {
  actual_val <- sanitise_aux_equation('a >= b')
  expected_val <- 'a>=b'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates time variable", {
  actual_val <- sanitise_aux_equation('Time + startTime')
  expected_val <- 'time+startTime'
  expect_equal(actual_val, expected_val)
})

#===============================================================================
test_that("translate_ifelse() returns an R-equivalent equation for Stella's XMILE", {
  test_equation <- "IF(Backlog > supply_capacity)\nTHEN supply_capacity\nELSE Backlog"
  actual_val    <- translate_ifelse(test_equation)
  expected_val  <- "ifelse(Backlog > supply_capacity,  supply_capacity\n,  Backlog)"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse() does not alter equations when there are no ifelse statements", {
  test_equation <- "a + b"
  actual_val    <- translate_ifelse(test_equation)
  expected_val  <- "a + b"
  expect_equal(actual_val, expected_val)
})

test_that("translate_ifelse() returns an R-equivalent equation for Vensim's XMILE", {
  test_equation <- "IF_THEN_ELSE(Backlog > Supply_capacity, Supply_capacity, Backlog)"
  actual_val    <- translate_ifelse(test_equation)
  expected_val  <- "ifelse(Backlog > Supply_capacity, Supply_capacity, Backlog)"
  expect_equal(actual_val, expected_val)
})

#===============================================================================
test_that("eval_constant_expr() returns the value of a constant expression", {
  test_equation <- "2 + 2"
  actual_val    <- eval_constant_expr(test_equation)
  expected_val  <- "4"
  expect_equal(actual_val, expected_val)
})

test_that("eval_constant_expr() returns an equation if there is no constant expression", {
  test_equation <- "a + b"
  actual_val    <- eval_constant_expr(test_equation)
  expected_val  <- "a + b"
  expect_equal(actual_val, expected_val)
})

test_that("eval_constant_expr() is not affected by elements in the global environment", {
  test_equation <- "a + 3"
  a <- 0
  actual_val    <- eval_constant_expr(test_equation)
  expected_val  <- "a + 3"
  expect_equal(actual_val, expected_val)
})
