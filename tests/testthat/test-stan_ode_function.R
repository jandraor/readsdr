
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

test_that("stan_ode_function() returns the expected string", {

  expected_stan_function <- paste(
    "functions {",
    "  vector lotka_volterra(real time, vector y, real[] params) {",
    "    vector[2] dydt;",
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
    "    return dydt;",
    "  }",
    "}",
    sep = "\n")

  stan_function <- stan_ode_function(lv, "lotka_volterra")
  expect_equal(stan_function, expected_stan_function)
})

test_that("create_stan_function() allows user to add other functions", {

  expected_stan_function <- paste(
    "functions {",
    "  vector lotka_volterra(real time, vector y, real[] params) {",
    "    vector[2] dydt;",
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
    "    return dydt;",
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

  stan_function <- stan_ode_function(lv, "lotka_volterra",
                                     extra_funs = list(user_function))

  expect_equal(stan_function, expected_stan_function)
})

test_that("get_fun_declaration() returns the expected string", {
  expect_equal(get_fun_declaration("lotka_volterra"),
               "  vector lotka_volterra(real time, vector y, real[] params) {")
})

test_that("get_diffeq_declaration() returns the expected string", {
  expect_equal(get_diffeq_declaration(3), "    vector[3] dydt;")
})

test_that("get_auxs_declaration() returns the expected string", {
  var_obj <- list(list(name = "Bx"),
                  list(name = "Dx"),
                  list(name = "By"),
                  list(name = "Dy"))

  expect_equal(get_auxs_declaration(var_obj),
               paste("    real Bx;",
                     "    real Dx;",
                     "    real By;",
                     "    real Dy;", sep = "\n"))
})

test_that("set_unknowns() updates the constant list", {
  const_list <- list(list(name = "a",
                          value = 0.1),
                     list(name = "b",
                          value = 0.2),
                     list(name = "c",
                          value = 0.2))

  updated_list <- list(list(name = "a",
                            value = "params[2]"),
                       list(name = "b",
                            value = "params[1]"),
                       list(name = "c",
                            value = 0.2))

  pars <- c("b", "a")

  expect_equal(set_unknowns(pars, const_list), updated_list)
})

test_that("get_equations() returns the expected string", {

  expected_equations <- paste("    Bx = 0.04*y[1];",
                              "    Dx = 0.2*y[1]*y[2];",
                              "    By = 1*y[1]*y[2];",
                              "    Dy = 0.5*y[2];",
                              sep = "\n")

  var_list  <- list(list(name = "Bx",
                         equation = "a*x"),
                    list(name = "Dx",
                         equation = "b*x*y"),
                    list(name = "By",
                         equation = "c*x*y"),
                    list(name = "Dy",
                         equation = "d*y"))

  const_list  <- list(list(name = "c",
                           value = 1),
                      list(name = "d",
                           value = 0.5),
                      list(name = "b",
                           value = 0.2),
                      list(name = "a",
                           value = 0.04))

  level_names <- c("x", "y")

  equations <- get_equations(var_list, const_list, level_names)
  expect_equal(equations, expected_equations)
})

test_that("get_diffeq() returns the expected string", {
  stock_list <- list(list(name     = "x",
                          equation = "Bx-Dx"),
                     list(name     = "y",
                          equation = "By-Dy"))

  diff_eq <- get_diffeq(stock_list)

  expect_equal(diff_eq,
               paste("    dydt[1] = Bx-Dx;",
                     "    dydt[2] = By-Dy;",
                     sep = "\n"))
})


