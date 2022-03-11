test_that("translate_delay handles Vensim equations", {
  eq       <- "outflow==DELAY_FIXED(inflow,2,0)"
  actual   <- translate_delay(eq, "Vensim")
  expected <- "sd_fixed_delay('inflow',time,2,0,.memory)"

  expect_equal(actual, expected)
})

test_that("sd_fixed_delay() works", {
   mdl_text <-
     '<?xml version="1.0" encoding="utf-8" ?>
<xmile version="1.0" xmlns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
	<header>
		<product version="1.0" lang="en">Vensim</product>
		<vendor>Ventana Systems, Inc.</vendor>
  </header>
	<sim_specs method="RK4" time_units="Month">
		<start>0</start>
		<stop>100</stop>
		<dt>1</dt>
	</sim_specs>
	<model>
		<variables>
			<stock name="outflow">
				  <eqn>
					outflow= DELAY_FIXED (
	inflow, 2, 0)
					</eqn>
			</stock>
			<stock name="test">
			    <eqn>
					0
					</eqn>
					<inflow>
					inflow
					</inflow>
					<outflow>
					outflow
					</outflow>
			</stock>
			<aux name="inflow">
				  <eqn>3 + STEP(3, 2)					</eqn>
			</aux>
		</variables>
	</model>
</xmile>'

   ds_comp <- xmile_to_deSolve(mdl_text)

   output  <- sd_simulate(ds_comp, integ_method = "euler", start_time = 0,
                          stop_time = 6, timestep = 0.25)
   actual  <- output$outflow

   expected <- c(rep(0, 8), rep(3, 8), rep(6, 9))

   expect_equal(actual, expected)
})

# identify_delayed_vars------------------------------------------------------------

test_that("identify_delayed_vars() works", {

  variables <- list(
    list(name = "inflow",
         equation ="a*stock"),
    list(name     = "outflow",
         equation = "sd_fixed_delay('inflow', 6, 2, 0, .memory)")
  )

  actual_obj   <- identify_delayed_vars(variables)
  expected_obj <- c("inflow")

  expect_equal(actual_obj, expected_obj)

})
