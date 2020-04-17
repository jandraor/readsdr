context("XML model components")

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


# create_param_obj_xmile()------------------------------------------------------


test_that("create_param_obj_xmile() returns the expected DT", {
  sim_specs <- xml2::read_xml(test_model) %>%
    xml2::xml_find_all(".//d1:sim_specs")
  output <- create_param_obj_xmile(sim_specs)
  expect_equal(output$dt, 0.25)
})


# create_vars_consts_obj_xmile()------------------------------------------------

test_that("create_vars_consts_obj_xmile() returns the expected variables &
constants", {
  vars_consts <- xml2::read_xml(test_model) %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")

  output <- create_vars_consts_obj_xmile(vars_consts, "isee")

  expect_equal(output$variables,
               list(list(name     = "net_growth",
                         equation = "population*growth_rate")))

  expect_equal(output$constants,
               list(list(name = "growth_rate", value = 0.01)))
})

test_that("create_vars_consts_obj_xmile() returns an empty list there are no
  vars and consts", {

    auxs_xml <- xml2::read_xml('
    <root>
      <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
        <variables>
          <stock name="Population">
            <eqn>100</eqn>
          </stock>
        </variables>
      </doc1>
    </root>') %>%
      xml2::xml_find_all(".//d1:flow|.//d1:aux")

    actual_obj   <- create_vars_consts_obj_xmile(auxs_xml, "isee")
    expected_obj <- list(variables = list(), constants = list())
    expect_equal(actual_obj, expected_obj)
  })

test_that("create_vars_consts_obj_xmile() ignores aux Time from Vensim", {

  auxs_xml <- xml2::read_xml('
    <root>
      <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
        <variables>
			    <stock name="Population">
				    <eqn>100</eqn>
			    </stock>
			    <aux name="effect">
				    <eqn>Population</eqn>
			    </aux>
			    <aux name="Time">
				    <eqn>INTEG(1, INITIAL_TIME )</eqn>
			   </aux>
		    </variables>
      </doc1>
    </root>') %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")

  actual_obj   <- create_vars_consts_obj_xmile(auxs_xml, "Vensim")
  expected_obj <- list(variables = list(
    list(name = "effect", equation = "Population")
  ), constants = list())
  expect_equal(actual_obj, expected_obj)
})


test_that("create_vars_consts_obj_xmile() creates the var object for a variable
          with a graphical function, and the XMILE was producted by VENSIM", {

            test_var_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
        <stock name="Price">
          <eqn>15</eqn>
        </stock>
        <aux name="demand_price_schedule">
          <eqn>WITH LOOKUP (Price, ([(0,10)-(50,100)],(5,100),(10,73),(15,57),(20,45),
            (25,35),(30,28),(35,22),(40,18),(45,14),(50,10) ))
          </eqn>
        </aux>
      </variables>
    </doc1>
  </root>') %>%
              xml2::xml_find_all(".//d1:flow|.//d1:aux")

            actual_obj   <- create_vars_consts_obj_xmile(test_var_xml, "Vensim")

            expected_obj <- list(
              variables = list(
                list(name = "demand_price_schedule",
                     equation = "f_demand_price_schedule(Price)",
                     graph_fun = list(
                       name = "f_demand_price_schedule",
                       fun  = approxfun(
                         x = seq(5, 50, 5),
                         y = c(100, 73, 57, 45, 35, 28, 22, 18, 14, 10),
                         method = "linear",
                         yleft  = 100,
                         yright = 10)))
              ),
              constants = list())

            expect_equal(actual_obj, expected_obj)
          })

test_that("create_vars_consts_obj_xmile() creates the var object for a variable
with a graphical function, and the XMILE was producted by STELLA", {

  test_var_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
			  <stock name="Price">
				  <eqn>15</eqn>
				</stock>
			  <aux name="demand price schedule">
				  <eqn>Price</eqn>
				  <gf>
					  <xscale min="5" max="50"/>
					  <yscale min="0" max="2"/>
					  <ypts>100,73,57,45,35,28,22,18,14,10</ypts>
				  </gf>
			  </aux>
      </variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")


  actual_obj   <- create_vars_consts_obj_xmile(test_var_xml, "isee")

  expected_obj <- list(
    variables = list(
      list(name = "demand_price_schedule",
           equation = "f_demand_price_schedule(Price)",
           graph_fun = list(
             name = "f_demand_price_schedule",
             fun  = approxfun(
               x = seq(5, 50, 5),
               y = c(100, 73, 57, 45, 35, 28, 22, 18, 14, 10),
               method = "linear",
               yleft  = 100,
               yright = 10)))
    ),
    constants = list())

  expect_equal(actual_obj, expected_obj)
})

test_that("create_vars_consts_obj_xmile() sanitises constant expressions", {
  test_var_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
        <stock name="population">
				  <eqn>100</eqn>
				  <inflow>net_growth</inflow>
			  </stock>
			  <flow name="net growth">
				  <eqn>population * growth_rate</eqn>
			  </flow>
			  <aux name="growth rate">
				  <eqn>1 / 10</eqn>
			  </aux>
      </variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")

  actual_obj   <- create_vars_consts_obj_xmile(test_var_xml, "Vensim")

  expected_obj <- list(
    variables = list(
      list(name     = "net_growth",
           equation = "population*growth_rate")),
    constants = list(
      list(name  = "growth_rate",
           value = 0.1)
    )
  )
  expect_equal(actual_obj, expected_obj)
})

test_that("create_vars_consts_obj_xmile() works with PULSE from Vensim", {
  test_var_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
    <variables>
      <aux name="growth_rate">
				<eqn>0.01 * PULSE(1, 0)					</eqn>
			</aux>
		</variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")

  actual_obj   <- create_vars_consts_obj_xmile(test_var_xml, "Vensim")

  expected_obj <- list(
    variables = list(
      list(name     = "growth_rate",
           equation = "0.01*ifelse(time==1,1,0)")),
    constants = list()
  )
  expect_equal(actual_obj, expected_obj)
})

# create_level_obj_xmile()======================================================

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

test_that("create_level_obj_xmile() deals with levels with no flows", {
  test_stocks_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
        <stock name="Population">
          <eqn>100</eqn>
        </stock>
      </variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:stock")

  test_vars   <- list()
  test_consts <- list()

  level_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts)
  actual_obj   <- level_obj[[1]]

  expected_obj <- list(name = "Population",
                       equation = "0",
                       initValue = 100)

  expect_equal(actual_obj, expected_obj)
})

test_that("create_level_obj_xmile() works should levels depend on other levels in init values", {
  test_stocks_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
        <stock name="pop1">
          <eqn>pop3*0.5</eqn>
          <inflow>net_change_1</inflow>
        </stock>
        <stock name="pop2">
          <eqn>100</eqn>
          <inflow>net_change_2</inflow>
       </stock>
       <stock name="pop3">
         <eqn>pop2*0.5</eqn>
         <inflow>net_change_3</inflow>
         </stock>
      </variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:stock")

  test_vars   <- list()
  test_consts <- list()

  actual_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts)
  expected_obj <- list(
    list(name      = "pop1",
         equation  = "net_change_1",
         initValue = 25),
    list(name      = "pop2",
         equation  = "net_change_2",
         initValue = 100),
    list(name      = "pop3",
         equation  = "net_change_3",
         initValue = 50))

  expect_equal(actual_obj, expected_obj)
})

test_that("create_level_obj_xmile() returns the expected object in the presence of multiple outflows", {
  test_stocks_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
        <stock name="Population">
				  <eqn>100</eqn>
				  <inflow>births</inflow>
				  <outflow>emigration</outflow>
				  <outflow>deaths</outflow>
			  </stock>
			</variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:stock")

  test_vars <- list(
    list(name = "births",
         equation = "Population*0.02"),
    list(name = "deaths",
         equation = "Population*0.01"),
    list(name = "emigration",
         equation = "Population*0.01")
    )

  test_consts <- list()

  level_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts)
  actual_val   <- level_obj[[1]]
  expected_val <- list(name = "Population",
                       equation = "births-emigration-deaths",
                       initValue = 100)
  expect_equal(actual_val, expected_val)
})

test_that("create_level_obj_xmile() returns the expected object in the presence of multiple inflows", {
  test_stocks_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
        <stock name="Population">
				  <eqn>100</eqn>
				  <inflow>births</inflow>
				  <inflow>immigration</inflow>
				  <outflow>deaths</outflow>
			  </stock>
      </variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:stock")

  test_vars <- list(
    list(name = "births",
         equation = "Population*0.01"),
    list(name = "deaths",
         equation = "Population*0.02"),
    list(name = "immigration",
         equation = "Population*0.01")
  )

  test_consts <- list()

  level_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts)
  actual_val   <- level_obj[[1]]
  expected_val <- list(name = "Population",
                       equation = "births+immigration-deaths",
                       initValue = 100)
  expect_equal(actual_val, expected_val)
})

test_that("create_level_obj_xmile() throws an error when there are no stocks", {
  test_stocks_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
		  <variables>
		    <aux name="var1">
				  <eqn>20</eqn>
			  </aux>
			  <aux name="var2">
				  <eqn>30</eqn>
			  </aux>
			  <aux name="var3">
				  <eqn>var1 + var2</eqn>
			  </aux>
      </variables>
    </doc1>
  </root>') %>%
    xml2::xml_find_all(".//d1:stock")

  test_vars <- list(
    list(name = "var3",
         equation = "var1+var2"))

  test_consts <- list(
    list(name = "var1",
         value = "20"),
    list(name = "var2",
         value = "30"))

  expect_error(create_level_obj_xmile(test_stocks_xml, test_vars, test_consts))
})
