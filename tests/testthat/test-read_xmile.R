context("Read xmile file")

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
				    <inflow>net_growth</inflow>
			    </stock>
			    <flow name="net growth">
				    <eqn>population * growth_rate</eqn>
			    </flow>
			    <aux name="growth rate">
				    <eqn>0.01</eqn>
			    </aux>
        </variables>
      </doc1>
    </root>'

test_that("the output from read_xmile() is a list", {
  expect_is(read_xmile(test_model), "list")
})

test_that("the output from read_xmile() produces the required elements", {
  expect_named(read_xmile(test_model, graph = TRUE),
               c("description", "deSolve_components", "graph_dfs"))
})

sd_simulate <- function(mdl, method = "euler") {
  # Create the start time, finish time, and time step
  START  <- mdl$description$parameters$start
  FINISH <- mdl$description$parameters$stop
  STEP   <- mdl$description$parameters$dt

  # Create time vector
  simtime <- seq(START, FINISH, by = STEP)

  data.frame(deSolve::ode(y      = mdl$deSolve_components$stocks,
                          times  = simtime,
                          func   = mdl$deSolve_components$func,
                          parms  = mdl$deSolve_components$consts,
                          method = method))
}

test_that("read_xmile() returns a runnable model", {
  mdl <- read_xmile(test_model)
  expect_is(sd_simulate(mdl), 'data.frame')
})

test_that("read_xmile() produces a model function that returns all levels, variables & constants", {
  mdl <- read_xmile(test_model)
  o   <- sd_simulate(mdl)
  expect_equal(ncol(o), 4) # including time
})

test_that("read_xmile() works for a model that has a NOT statement
from Stella", {
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
				    <inflow>net_growth</inflow>
			    </stock>
			    <flow name="net growth">
				    <eqn>population * growth_rate</eqn>
			    </flow>
			    <aux name="growth rate">
				    <eqn>IF(NOT (TIME = 3)) THEN 0 ELSE 1</eqn>
			    </aux>
        </variables>
      </doc1>
    </root>'

  # It is anticipated that this operation will throw a warning because
  # it has a function that cannot be converted to a graph
  mdl          <- suppressWarnings(read_xmile(test_model))
  output       <- sd_simulate(mdl)
  actual_val   <- output[output$time == 3.25, "population"]
  expected_val <- 125
  expect_equal(actual_val, expected_val)
})

test_that("read_xmile() allows the user to override init values of stocks", {
  stock_list <- list(population = 200)
  mdl        <- read_xmile(test_model, stock_list = stock_list)
  expect_equal(mdl$description$levels[[1]]$initValue, 200)
  expect_equal(mdl$deSolve_components$stocks[[1]], 200)
})

test_that("read_xmile() allows the user to override values of constants", {

  const_list <- list(growth_rate = 0.02)
  mdl        <- read_xmile(test_model, const_list = const_list)
  expect_equal(mdl$description$constants[[1]]$value, 0.02)
  expect_equal(mdl$deSolve_components$consts[[1]], 0.02)
})

test_that("read_xmile() supports n-dimensional arrays from Vensim", {

  expect_named(read_xmile("./2d_pop.xmile"),
               c("description", "deSolve_components"))

})

test_that("read_xmile() overrides metaparameter for delay N", {

  expected <- 12

  filepath      <- system.file("models/", "SEjIkR.stmx", package = "readsdr")
  mdl           <- read_xmile(filepath, const_list = list(j = 5))

  actual   <- length(mdl$description$levels)

  expect_equal(actual, expected)

  filepath <- "./SEjIkR.xmile"
  mdl      <- read_xmile(filepath, const_list = list(j = 5))
  actual   <- length(mdl$description$levels)

  expect_equal(actual, expected)
})

test_that("read_xmile() handles Simlin files", {

  filepath <- "./test_models/SEIR_simlin.stmx"

  mdl <- read_xmile(filepath)

  expect_is(mdl, "list")
})

#xmile_to_deSolve()-------------------------------------------------------------

test_that("xmile_to_deSolve() returns a list", {
  expect_is(xmile_to_deSolve(test_model), "list")
})
