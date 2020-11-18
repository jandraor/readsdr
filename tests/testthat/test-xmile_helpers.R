context("XMILE helpers")

# compute_init_value()----------------------------------------------------------
test_that("compute_init_value() extracts the expected initial value when it is
the defined by a constant", {
  stock_name    <- "test_stock"
  equation      <- "c"
  auxs          <- list(list(name = "c", equation = "100"))
  actual_val    <- compute_init_value(stock_name, equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is
the defined by a one-level nested equation", {
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

test_that("compute_init_value() extracts the expected initial value when it is
the defined by a two-level nested equation", {
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

test_that("compute_init_value() extracts the expected initial value when it is
the defined by a constant and variable", {
  stock_name    <- "test_stock"
  equation      <- "3 + c"
  auxs          <- list(
    list(name = "c", equation = "97"))
  actual_val    <- compute_init_value(stock_name, equation, auxs)
  expected_val  <- 100
  expect_equal(actual_val, expected_val)
})

test_that("compute_init_value() extracts the expected initial value when it is
the defined by a constant and a one-level nested variable", {
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

test_that("compute_init_value() calculates the initial value in an equation when
all variables are defined by constants", {
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

test_that("compute_init_value() calculates the initial value when there are
graph function along the process", {
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

test_that("compute_init_value() indicates the stock's name for which the
process failed", {
  auxs <- list()
  expect_error(compute_init_value("test_stock", "1000-seed_value", auxs),
               "Can't compute the init value of 'test_stock'")
})

# sanitise_elem_name()----------------------------------------------------------

test_that("sanitise_elem_name() returns the sanitised name when it has a
breakline in between", {
  expect_equal(sanitise_elem_name("flow\\ntest"), "flow_test")
})

test_that("sanitise_elem_name() deals with spaces", {
  expect_equal(sanitise_elem_name("main stock"), "main_stock")
})

# sanitise_init_value()---------------------------------------------------------

test_that("sanitise_init_value() removes commentaries", {
  actual_val <- sanitise_init_value("\n\t\t\t\t\t5800\n{(nic*ey)}\n\t\t\t\t\t")
  expect_equal(actual_val, "5800")
})

test_that("sanitise_init_value() removes commentaries", {
  actual_val <- sanitise_init_value("total_population - 1")
  expect_equal(actual_val, "total_population - 1")
})

#===============================================================================
test_that("sanitise_aux_equation() removes comentaries at the beginning", {
  actual_val <- sanitise_aux_equation("{0}1", "Vensim")
  expected_val <- "1"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() removes comentaries at the end", {
  actual_val <- sanitise_aux_equation("1{}", "isee")
  expected_val <- "1"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() removes comentaries at the beginning and at the end", {
  actual_val <- sanitise_aux_equation("{comment1}1{comment2}", "Vensim")
  expected_val <- "1"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() set min function in lower case", {
  actual_val <- sanitise_aux_equation("MIN(a,b)", "isee")
  expected_val <- "min(a,b)"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() set maxfunction in lower case", {
  actual_val <- sanitise_aux_equation("MAX(a,b)", "Vensim")
  expected_val <- "max(a,b)"
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates AND operator from a Vensim model", {
  actual_val <- sanitise_aux_equation('a > b :AND: a > c', "Vensim")
  expected_val <- 'a>b&a>c'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates OR operator form a Vensim model", {
  actual_val <- sanitise_aux_equation('a > b :OR: a > c', "Vensim")
  expected_val <- 'a>b|a>c'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates = operator", {
  actual_val <- sanitise_aux_equation('a = b', "isee")
  expected_val <- 'a==b'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() does not misinterpret greater or equal to", {
  actual_val <- sanitise_aux_equation('a >= b', "Vensim")
  expected_val <- 'a>=b'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates time variable from Vensim", {
  actual_val <- sanitise_aux_equation('Time + startTime', "Vensim")
  expected_val <- 'time+startTime'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates time variable from Stella", {
  actual_val <- sanitise_aux_equation('TIME + startTime', "isee")
  expected_val <- 'time+startTime'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() ignores the correct equal operator", {
  actual_val <- sanitise_aux_equation('a == b', "Vensim")
  expected_val <- 'a==b'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates joint IF & NOT statements from Stella ", {
  actual_val <- sanitise_aux_equation('IF(NOT (TIME = 3)) THEN 0 ELSE 1', "isee")
  expected_val <- 'ifelse(!(time==3),0,1)'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates ABS from Vensim", {
  actual_val   <- sanitise_aux_equation('ABS(a,b)', "Vensim")
  expected_val <- 'abs(a,b)'
  expect_equal(actual_val, expected_val)
})

test_that("sanitise_aux_equation() translates ABS from Stella", {
  actual_val   <- sanitise_aux_equation('ABS(a,b)', "isee")
  expected_val <- 'abs(a,b)'
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

#===============================================================================
test_that("check_elem_name() throws an error in the presence of invalid names", {
  test_name <- "a+"
  expect_error(check_elem_name(test_name))
})

test_that("check_elem_name() returns the input if there is no error", {
  test_name    <- "a"
  actual_val   <- check_elem_name(test_name)
  expected_val <- "a"
  expect_equal(actual_val, expected_val)
})

# which vendor()================================================================
context("which vendor")

test_that("which_vendor() detects Vensim", {
  raw_xml         <- xml2::read_xml(
  '<?xml version="1.0" encoding="utf-8" ?>
  <xmile version="1.0" xmlns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
	  <header>
		  <product version="1.0" lang="en">Vensim</product>
		  <vendor>Ventana Systems, Inc.</vendor>
	  </header>
  </xmile>')
  actual_vendor   <- which_vendor(raw_xml)
  expected_vendor <- "Vensim"
  expect_equal(actual_vendor, expected_vendor)
})

test_that("which_vendor() detects isee", {
  raw_xml         <- xml2::read_xml(
  '<?xml version="1.0" encoding="utf-8"?>
   <xmile version="1.0" xmlns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0" xmlns:isee="http://iseesystems.com/XMILE">
	   <header>
		   <smile version="1.0" namespace="std, isee"/>
		   <name>pop1</name>
		   <uuid>d683eaba-f1f7-4e19-bace-5420261df8c0</uuid>
		   <vendor>isee systems, inc.</vendor>
		   <product version="1.6.2" isee:build_number="1445" isee:saved_by_v1="true" lang="en">Stella Architect</product>
	   </header>
   </xmile>')
  actual_vendor   <- which_vendor(raw_xml)
  expected_vendor <- "isee"
  expect_equal(actual_vendor, expected_vendor)
})

context("Safe read")

test_that("safe_read() extracts the xml from a file", {
  test_xml <-
    '<model>
       <variables>
         <aux name = "var1">
           <eqn>a + b</eqn>
         </aux>
       </variables>
    </model>'

  actual_output <- safe_read(test_xml)
  expect_is(actual_output, "xml_document")
})

test_that("safe_read() deals with less than operator", {
  filepath      <- "./test_xml_less_than.xml"
  actual_output <- safe_read(filepath)
  expect_is(actual_output, "xml_document")
})

test_that("safe_read() throws an error for invalid xml", {
  filepath      <- "./invalid_xml.xml"
  expect_error(safe_read(filepath), "Invalid XML file")
})

# sanitise_arrays()=============================================================

test_that("sanitise_arrays() returns the expected value", {
  expect_equal(sanitise_arrays("Population[B]*growth_rate[A]", "isee"),
               "Population_B*growth_rate_A")
})
