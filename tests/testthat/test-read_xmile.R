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
  expect_named(read_xmile(test_model), c("description", "deSolve_components",
                                         "graph_dfs"))
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

  # It is anticipated that this operation will throw a warning
  mdl          <- suppressWarnings(read_xmile(test_model))
  output       <- sd_simulate(mdl)
  actual_val   <- output[output$time == 3.25, "population"]
  expected_val <- 125
  expect_equal(actual_val, expected_val)
})

#xmile_to_deSolve()-------------------------------------------------------------

test_that("xmile_to_deSolve() returns a list", {
  expect_is(xmile_to_deSolve(test_model), "list")
})
