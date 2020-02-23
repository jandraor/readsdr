
test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant", {
  equation      <- "c"
  auxs          <- list(list(name = "c", equation = "100"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a one-level nested equation", {
  equation      <- "c"
  auxs          <- list(
    list(name = "c", equation = "c1 + c2"),
    list(name = "c1", equation = "80"),
    list(name = "c2", equation = "20"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a two-level nested equation", {
  equation      <- "c"
  auxs          <- list(
    list(name = "c", equation = "c1 + c2"),
    list(name = "c1", equation = "c3 * c4"),
    list(name = "c2", equation = "c5 / c6"),
    list(name = "c3", equation = "20"),
    list(name = "c4", equation = "4"),
    list(name = "c5", equation = "40"),
    list(name = "c6", equation = "2"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant and variable", {
  equation      <- "3 + c"
  auxs          <- list(
    list(name = "c", equation = "97"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is the defined by a constant and a one-level nested variable", {
  equation      <- "5 * c"
  auxs          <- list(
    list(name = "c", equation = "c1 - c2"),
    list(name = "c1", equation = "27"),
    list(name = "c2", equation = "7"))
  actual_val    <- compute_init_value(equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() calculates the initial value in an equation when all variables are defined by constants", {
  test_equation      <- "ey*ep/eyvm"
  test_auxs          <- list(
    list(name = "ey", equation = "20000"),
    list(name = "ep", equation = "1"),
    list(name = "eyvm", equation = "5"))
  actual_val    <- compute_init_value(test_equation, test_auxs)
  expected_val  <- 4000
  expect_equal(actual_val, expected_val)
})

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


test_that("create_level_obj_xmile() returns the expected object", {
  test_stocks_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
        <stock name="Population">
          <eqn>initPopulation</eqn>
          <inflow>netGrowth</inflow>
        </stock>
      </variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:stock")

  test_vars <- list(
    list(name = "netGrowth",
         equation = "Population*growthFraction"))

  test_consts <- list(
    list(name = "growthFraction",
         value = "0.01"),
    list(name = "initPopulation",
         value = "100"))

  level_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts)
  actual_val   <- level_obj[[1]]
  expected_val <- list(name = "Population",
                       equation = "netGrowth",
                       initValue = 100)
  expect_equal(actual_val, expected_val)
})


