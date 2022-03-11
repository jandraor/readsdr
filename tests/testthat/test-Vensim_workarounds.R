test_that("extract_vars_in_stocks handles DELAY FIXED from Vensim", {
  test_xml <- xml2::read_xml('
  <xmile version="1.0" xmlns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
    <model>
		  <variables>
			  <stock name="outflow">
				  <eqn>
					out=
	DELAY_FIXED(inflow, 2, 0)
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
  </xmile>')

  stocks_xml <- xml2::xml_find_all(test_xml, ".//d1:stock")

  vars_and_consts <- list(
    variables      = list(
      list(name     = "inflow",
           equation = "3+ifelse(time>=3,3,0)")),
    constants      = list(),
    builtin_stocks = list())

  actual_obj <- extract_vars_in_stocks(stocks_xml, vars_and_consts)

  expected_obj <- list(
    variables      = list(
      list(name     = "inflow",
           equation = "3+ifelse(time>=3,3,0)"),
      list(name     = "outflow",
           equation = "sd_fixed_delay('inflow',time,2,0,.memory)")),
    constants      = list(),
    builtin_stocks = list())

  expect_equal(actual_obj, expected_obj)
})
