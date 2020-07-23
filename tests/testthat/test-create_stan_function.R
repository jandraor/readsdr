context("Create Stan function")

lv <- # Lotka-Volterra
  '<root>
      <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
        <header>
		      <vendor>isee systems, inc.</vendor>
		    </header>
	      <sim_specs>
	        <start>0</start>
		      <stop>4</stop>
		      <dt reciprocal="true">4</dt>
	      </sim_specs>
	   	  <variables>
	   	    <stock name="x">
				    <eqn>10</eqn>
				    <inflow>Bx</inflow>
				    <outflow>Dx</outflow>
			    </stock>
			    <flow name="Bx">
				    <eqn>a * x</eqn>
			    </flow>
			    <flow name="Dx">
				    <eqn>b * x * y</eqn>
			    </flow>
			    <stock name="y">
				    <eqn>2</eqn>
				    <inflow>By</inflow>
				    <outflow>Dy</outflow>
			    </stock>
			    <flow name="By">
				    <eqn>c * x * y</eqn>
			    </flow>
			    <flow name="Dy">
				    <eqn>d * y</eqn>
			    </flow>
			    <aux name="c">
				    <eqn>0.04</eqn>
			    </aux>
			    <aux name="d">
				    <eqn>0.5</eqn>
			    </aux>
			    <aux name="b">
				    <eqn>0.2</eqn>
			    </aux>
			    <aux name="a">
				    <eqn>1</eqn>
			    </aux>
        </variables>
      </doc1>
    </root>'

test_model <-
  '<root>
      <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
        <header>
		      <vendor>isee systems, inc.</vendor>
		    </header>
	      <sim_specs>
	        <start>0</start>
		      <stop>4</stop>
		      <dt reciprocal="true">4</dt>
	      </sim_specs>
	   	  <variables>
	   	    <stock name="population">
				    <eqn>100</eqn>
				    <inflow>births</inflow>
			    </stock>
			    <flow name="births">
				    <eqn>population * birthRate</eqn>
			    </flow>
			    <aux name="birthRate">
				    <eqn>birthRate2</eqn>
			    </aux>
			    <aux name="birthRate2">
				    <eqn>0.1</eqn>
			    </aux>
        </variables>
      </doc1>
    </root>'


test_that("create_stan_function() returns the expected string", {
  expected_stan_function <- paste(
    "functions {",
    "  real[] lotka_volterra(real time,",
    "              real[] y,",
    "              real[] params,",
    "              real[] x_r,",
    "              int[] x_i) {",
    "  real dydt[2];",
    "    real Bx;",
    "    real Dx;",
    "    real By;",
    "    real Dy;",
    "    Bx = 1*y[1];",
    "    Dx = 0.2*y[1]*y[2];",
    "    By = 0.04*y[1]*y[2];",
    "    Dy = 0.5*y[2];",
    "    dydt[1] = Bx-Dx;",
    "    dydt[2] = By-Dy;",
    "  return dydt;",
    "  }",
    "}",
    sep = "\n")

  stan_function <- create_stan_function(lv, "lotka_volterra")
  expect_equal(stan_function, expected_stan_function)
})

test_that("create_stan_function() returns a string", {
  stan_function <- create_stan_function(lv, "test_model")
  expect_is(stan_function, "character")
})

test_that("create_stan_function() assign the expected name to the function", {
  stan_function <- create_stan_function(lv, "lotka_volterra")
  second_line   <- strsplit(stan_function, "\n")[[1]][[2]]
  expected_line <-   "  real[] lotka_volterra(real time,"
  expect_equal(second_line, expected_line)
})

test_that("create_stan_function() declares the exact number of stocks", {
  stan_function <- create_stan_function(lv, "lotka_volterra")
  seventh_line <- strsplit(stan_function, "\n")[[1]][[7]]
  expected_line <-   "  real dydt[2];"
  expect_equal(seventh_line, expected_line)
})

test_that("create_stan_function() parameterise the function", {
  stan_function <- create_stan_function(lv, "lotka_volterra",
                                        pars = c("d", "a"))
  actual_12th_line   <- strsplit(stan_function, "\n")[[1]][[12]]
  actual_15th_line   <- strsplit(stan_function, "\n")[[1]][[15]]
  expected_12th_line  <-   "    Bx = params[2]*y[1];"
  expected_15th_line  <-   "    Dy = params[1]*y[2];"
  expect_equal(actual_12th_line, expected_12th_line)
  expect_equal(actual_15th_line, expected_15th_line)
})

test_that("create_stan_function() throws an error when the parameter does not
exist", {
  expect_error(create_stan_function(lv, "lotka_volterra", pars = "wrong_par"),
               "'wrong_par' not found")
})

test_that("create_stan_function() returns equations in computational  order", {
  stan_function <- create_stan_function(test_model, "test_model")

  expected_function <- paste(
    "functions {",
    "  real[] test_model(real time,",
    "              real[] y,",
    "              real[] params,",
    "              real[] x_r,",
    "              int[] x_i) {",
    "  real dydt[1];",
    "    real birthRate;",
    "    real births;",
    "    birthRate = 0.1;",
    "    births = y[1]*birthRate;",
    "    dydt[1] = births;",
    "  return dydt;",
    "  }",
    "}", sep = "\n")

    expect_equal(stan_function, expected_function)
})

test_that("create_stan_function() allows user to override constant values", {
  consts <- list(list(name = "a", value = 0.5), list(name = "c", value = 0.3))
  stan_function <- create_stan_function(
    filepath = lv, func_name = "lotka_volterra", override.consts = consts)

  expected_stan_function <- paste(
    "functions {",
    "  real[] lotka_volterra(real time,",
    "              real[] y,",
    "              real[] params,",
    "              real[] x_r,",
    "              int[] x_i) {",
    "  real dydt[2];",
    "    real Bx;",
    "    real Dx;",
    "    real By;",
    "    real Dy;",
    "    Bx = 0.5*y[1];",
    "    Dx = 0.2*y[1]*y[2];",
    "    By = 0.3*y[1]*y[2];",
    "    Dy = 0.5*y[2];",
    "    dydt[1] = Bx-Dx;",
    "    dydt[2] = By-Dy;",
    "  return dydt;",
    "  }",
    "}",
    sep = "\n")
  expect_equal(stan_function, expected_stan_function)
})

test_that("create_stan_function() allows user to add other functions", {
  expected_stan_function <- paste(
    "functions {",
    "  real[] lotka_volterra(real time,",
    "              real[] y,",
    "              real[] params,",
    "              real[] x_r,",
    "              int[] x_i) {",
    "  real dydt[2];",
    "    real Bx;",
    "    real Dx;",
    "    real By;",
    "    real Dy;",
    "    Bx = 1*y[1];",
    "    Dx = 0.2*y[1]*y[2];",
    "    By = 0.04*y[1]*y[2];",
    "    Dy = 0.5*y[2];",
    "    dydt[1] = Bx-Dx;",
    "    dydt[2] = By-Dy;",
    "  return dydt;",
    "  }",
    "  real test_sum(int y, int x) {",
    "    return x + y;",
    "  }",
    "}",
    sep = "\n")

  user_function <- paste(
    "  real test_sum(int y, int x) {",
    "    return x + y;",
    "  }",
    sep = "\n")

  expect_equal(create_stan_function(lv, "lotka_volterra",
                                    additional_funs = list(user_function)),
               expected_stan_function)
})





