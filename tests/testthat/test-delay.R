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

test_that("translate_DELAYN() returns the expected list", {

  actual <- translate_DELAYN(var_name = "I_to_R",
                             eq = "DELAYN(E_to_I,1/par_gamma,k,par_gamma*I0)",
                             vendor = "isee",
                             consts = list(list(name  = "I0",
                                                value = 1),
                                           list(name  = "par_gamma",
                                                value = 0.5),
                                           list(name  = "k",
                                                value = 2)),
                             inits_vector = NULL)

  expected <- list(
    variable_list = list(
      list(name     = "I_to_R",
           equation = "dly_E_to_I_2_out"),
      list(name = "dly_E_to_I_1_out",
           equation = "dly_E_to_I_1/((2)/2.0)"),
      list(name     = "dly_E_to_I_2_out",
           equation = "dly_E_to_I_2/((2)/2.0)")),
    stock_list = list(
      list(name      = "dly_E_to_I_1",
           equation  = "E_to_I - dly_E_to_I_1_out",
           initValue = 0.5),
      list(name      = "dly_E_to_I_2",
           equation  = "dly_E_to_I_1_out - dly_E_to_I_2_out",
           initValue = 0.5)),
    delay_order = 2)

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

# stc_vars_DELAY ---------------------------------------------------------------

test_that("stc_vars_DELAYN() returns the expected list", {

  name        <- "I_to_R"
  input       <- "E_to_I"
  duration    <- 2
  delay_order <- 5
  init        <- 0.2
  eq          <- "DELAYN(E_to_I,2,k,0.5)"

  actual <- stc_vars_DELAYN(name, input, duration, delay_order, init, eq)

  expected <-   list(
    variable_list = list(
      list(name = "I_to_R", equation = "dly_E_to_I_5_out"),
      list(name = "dly_E_to_I_1_out", equation = "dly_E_to_I_1/((2)/5.0)"),
      list(name = "dly_E_to_I_2_out", equation = "dly_E_to_I_2/((2)/5.0)"),
      list(name = "dly_E_to_I_3_out", equation = "dly_E_to_I_3/((2)/5.0)"),
      list(name = "dly_E_to_I_4_out", equation = "dly_E_to_I_4/((2)/5.0)"),
      list(name = "dly_E_to_I_5_out", equation = "dly_E_to_I_5/((2)/5.0)")),
    stock_list    = list(
      list(name = "dly_E_to_I_1",
           equation = "E_to_I - dly_E_to_I_1_out",
           initValue = 0.2),
      list(name = "dly_E_to_I_2",
           equation = "dly_E_to_I_1_out - dly_E_to_I_2_out",
           initValue = 0.2),
      list(name = "dly_E_to_I_3",
           equation = "dly_E_to_I_2_out - dly_E_to_I_3_out",
           initValue = 0.2),
      list(name = "dly_E_to_I_4",
           equation = "dly_E_to_I_3_out - dly_E_to_I_4_out",
           initValue = 0.2),
      list(name = "dly_E_to_I_5",
           equation = "dly_E_to_I_4_out - dly_E_to_I_5_out",
           initValue = 0.2)),
    delay_order   = 5)

  expect_equal(actual, expected)
})
