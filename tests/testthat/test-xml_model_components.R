
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

smooth1_xml <-  xml2::read_xml(
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
         <aux name="S1">
           <eqn>SMTH1(0.5,  5,  1)</eqn>
         </aux>
       </variables>
     </doc1>
   </root>')

# create_param_obj_xmile()------------------------------------------------------


test_that("create_param_obj_xmile() returns the expected DT", {
  sim_specs <- xml2::read_xml(test_model) %>%
    xml2::xml_find_all(".//d1:sim_specs")
  output <- create_param_obj_xmile(sim_specs)
  expect_equal(output$dt, 0.25)
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

  time_aux   <- list(name     = "time",
                     equation = 0)

  level_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts,
                                         time_aux = time_aux)
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

  time_aux   <- list(name     = "time",
                     equation = 0)

  level_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts,
                                         time_aux = time_aux)
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

  time_aux   <- list(name     = "time",
                     equation = 0)

  actual_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts,
                                         time_aux = time_aux)
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

  time_aux   <- list(name     = "time",
                     equation = 0)

  level_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts,
                                         time_aux = time_aux)
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

  time_aux   <- list(name     = "time",
                     equation = 0)

  level_obj    <- create_level_obj_xmile(test_stocks_xml,
                                         test_vars, test_consts,
                                         time_aux = time_aux)
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

test_that("create_level_obj_xmile() takes into account builtin stocks", {

  test_stocks_xml <- xml2::xml_find_all(smooth1_xml, ".//d1:stock")


  variables <- list(
      list(name     = "adjust_S1",
           equation = "(0.5-S1)/5"))

  constants <- list()

  builtin_stocks <- list(
    list(name      = "S1",
         equation  = "adjust_S1",
         initValue = 1)
    )

  time_aux   <- list(name     = "time",
                     equation = 0)


  actual_val      <- create_level_obj_xmile(test_stocks_xml, variables,
                                            constants, builtin_stocks,
                                            time_aux = time_aux)

  expected_val <- list(name      = "S1",
                       equation  = "adjust_S1",
                       initValue = 1)

  expect_equal(actual_val[[1]], expected_val)
})

test_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
		  <variables>
			<stock name="Population">
				<dimensions>
					<dim name="region"/>
				</dimensions>
				<element subscript="A">
					<eqn>100</eqn>
				</element>
				<element subscript="B">
					<eqn>200</eqn>
				</element>
				<inflow>growth</inflow>
			</stock>
      </variables>
    </doc1>
  </root>')

test_stocks_xml <- xml2::xml_find_all(test_xml, ".//d1:stock")

test_that("create_level_obj_xmile() handles stock vectors", {

  test_vars   <- list()
  test_consts <- list()

  expected_obj <- list(
    list(
      name      = "Population_A",
      equation  = "growth_A",
      initValue = 100),
    list(
      name      = "Population_B",
      equation  = "growth_B",
      initValue = 200))

  time_aux   <- list(name     = "time",
                     equation = 0)

  dims_obj <- list(global_dims = list(region = c("A", "B")))

  actual_obj <- create_level_obj_xmile(test_stocks_xml, test_vars, test_consts,
                                       time_aux = time_aux,
                                       dims_obj = dims_obj,
                                       vendor = "isee")

  expect_equal(actual_obj, expected_obj)
})

test_that("create_level_obj_xmile() handles DELAY FIXED from Vensim", {

  test_xml <- xml2::read_xml('
  <xmile version="1.0" xmlns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
    <model>
		  <variables>
			  <stock name="out">
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
      </variables>
	  </model>
  </xmile>')

  test_stocks_xml <- xml2::xml_find_all(test_xml, ".//d1:stock")
  test_vars   <- list()
  test_consts <- list()

  expected_obj <- list(
    list(
      name      = "test",
      equation  = "inflow-outflow",
      initValue = 0))

  time_aux   <- list(name     = "time",
                     equation = 0)

  actual_obj <- create_level_obj_xmile(test_stocks_xml, test_vars, test_consts,
                                       time_aux = time_aux, vendor = "Vensim")

  expect_equal(actual_obj, expected_obj)
})

test_that("create_level_obj_xmile() handles step function in init value", {
  stocks_xml <- list()

  variables <- list(
    list(name     = "orders",
         equation = "3+ifelse(time>=2,3,0)"),
    list(name     = "adjust_smoothed_step",
         equation = "(orders-smoothed_step)/2")
  )

  constants <- list()

  builtin_stocks <- list(list(name      = "smoothed_step",
                              equation  = "adjust_smoothed_step",
                              initValue = "orders"))

  time_aux   <- list(name     = "time",
                     equation = 0)

  actual_obj <- create_level_obj_xmile(stocks_xml, variables, constants,
                                       builtin_stocks, time_aux = time_aux)

  expected_obj <- list(list(name      = "smoothed_step",
                            equation  = "adjust_smoothed_step",
                            initValue = 3))

  expect_equal(actual_obj, expected_obj)
})

# extract_stock_info()----------------------------------------------------------

test_that("extract_stock_info() handles a stock vector", {
  expected_obj <- list(
    list(name = "Population_A", equation = "growth_A", initValue = "100"),
    list(name = "Population_B", equation = "growth_B", initValue = "200"))

  dims_obj <- list(global_dims = list(region = c("A", "B")))

  actual_obj <- extract_stock_info(test_stocks_xml[[1]],
                                   dims_obj = dims_obj, vendor = "isee")

  expect_equal(actual_obj, expected_obj)
})

test_that("extract_stock_info() handles vector stock with a unique initialisation", {
  test_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
		  <variables>
			  <stock name="E">
				  <dimensions>
					  <dim name="Age"/>
				  </dimensions>
				  <eqn>0</eqn>
				  <inflow>IR</inflow>
				  <outflow>DR</outflow>
			  </stock>
      </variables>
    </doc1>
  </root>')

  test_stocks_xml <- xml2::xml_find_all(test_xml, ".//d1:stock")

  expected_obj <- list(
    list(name = "E_1", equation = "IR_1-DR_1", initValue = "0"),
    list(name = "E_2", equation = "IR_2-DR_2", initValue = "0"),
    list(name = "E_3", equation = "IR_3-DR_3", initValue = "0"),
    list(name = "E_4", equation = "IR_4-DR_4", initValue = "0"))

  dims_obj <- list(global_dims = list(Age = c(1:4)))

  actual_obj <- extract_stock_info(test_stocks_xml[[1]], dims_obj, "isee")

  expect_equal(actual_obj, expected_obj)
})

test_that("extract_stock_info() handles a 2D stock from Vensim", {

  filepath        <- "./2d_pop.xmile"

  raw_xml       <- xml2::read_xml(filepath)
  dims_obj      <- create_dims_obj(raw_xml)
  variables_xml <- xml2::xml_find_first(raw_xml, ".//d1:variables")
  stocks_xml    <-  xml2::xml_find_all(variables_xml, ".//d1:stock")
  stock_xml     <- stocks_xml[[1]]
  actual_obj    <- extract_stock_info(stocks_xml[[1]], dims_obj, "Vensim")

  expected_obj <- list(
    list(name       = "Population_Westeros_Young",
         equation   = "Net_growth_Westeros_Young",
         initValue = "init_pop_Westeros_Young"),
    list(name       = "Population_Westeros_Old",
         equation   = "Net_growth_Westeros_Old",
         initValue = "init_pop_Westeros_Old"),
    list(name       = "Population_Essos_Young",
         equation   = "Net_growth_Essos_Young",
         initValue = "init_pop_Essos_Young"),
    list(name       = "Population_Essos_Old",
         equation   = "Net_growth_Essos_Old",
         initValue = "init_pop_Essos_Old"))

  expect_equal(actual_obj, expected_obj)
})

test_that("extract_stock_info() handles vectorised init values", {

  filepath      <- "./test_models/vec_pop.stmx"
  raw_xml       <- xml2::read_xml(filepath)
  dims_obj      <- create_dims_obj(raw_xml)
  variables_xml <- xml2::xml_find_first(raw_xml, ".//d1:variables")
  stocks_xml    <-  xml2::xml_find_all(variables_xml, ".//d1:stock")
  stock_xml     <- stocks_xml[[1]]

  actual_obj    <- extract_stock_info(stock_xml , dims_obj, "isee")

  expected_obj <- list(
    list(name      = "Population_1",
         equation  = "net_growth_1",
         initValue = "init_pop_1"),
    list(name      = "Population_2",
         equation  = "net_growth_2",
         initValue = "init_pop_2 - 1")
  )

  expect_equal(actual_obj, expected_obj)


  filepath      <- system.file("models/", "SEIR_age.stmx", package = "readsdr")
  raw_xml       <- xml2::read_xml(filepath)
  dims_obj      <- create_dims_obj(raw_xml)
  variables_xml <- xml2::xml_find_first(raw_xml, ".//d1:variables")
  stocks_xml    <-  xml2::xml_find_all(variables_xml, ".//d1:stock")
  stock_xml     <- stocks_xml[[1]]

  actual_obj    <- extract_stock_info(stock_xml, dims_obj, "isee")

  expected_obj <- list(
    list(name      = "S_A",
         equation  = "-S_to_E_A",
         initValue = "N_A-I_A"),
    list(name      = "S_B",
         equation  = "-S_to_E_B",
         initValue = "N_B-I_B"),
    list(name      = "S_C",
         equation  = "-S_to_E_C",
         initValue = "N_C-I_C"),
    list(name      = "S_D",
         equation  = "-S_to_E_D",
         initValue = "N_D-I_D"))

  expect_equal(actual_obj, expected_obj)
})

# create_dims_obj()-------------------------------------------------------------

test_that("create_dims_obj() returns the expected object", {

  test_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
	<dimensions>
		<dim name="Age" size="4"/>
		<dim name="Interactions">
			<elem name="b11"/>
			<elem name="b12"/>
    </dim>
	</dimensions>
    </doc1>
  </root>')

  actual_obj <- create_dims_obj(test_xml)
  expected_obj <- list(global_dims = list(Age = c(1:4),
                                          Interactions = c("b11", "b12")),
                       dictionary = NULL)

  expect_equal(actual_obj, expected_obj)


  filepath <- "./2d_pop.xmile"
  test_xml <- xml2::read_xml(filepath)
  actual_obj <- create_dims_obj(test_xml)

  expected_obj <- list(global_dims = list(Age = c("Young", "Old"),
                                          Region = c("Westeros", "Essos")),
                       dictionary = list(Population  = c("Region", "Age"),
                                         Net_growth  = c("Region", "Age"),
                                         Growth_rate = c("Region", "Age"),
                                         init_pop    = c("Region", "Age")))

  expect_equal(actual_obj, expected_obj)

  filepath <- "./test_models/vec_pop.stmx"

  test_xml <- xml2::read_xml(filepath)
  actual_obj <- create_dims_obj(test_xml)

  expected_obj <- list(global_dims = list(Region = 1:2),
                       dictionary = list(Population  = "Region",
                                         net_growth  = "Region",
                                         growth_rate = "Region",
                                         init_pop    = "Region"))

  expect_equal(actual_obj, expected_obj)
})

# get_init_value()--------------------------------------------------------------



test_that("get_init_value() returns the expected list", {

  stock_obj <- list(name      = "S",
                    equation  = "-S_to_E",
                    initValue = "n - I0")

  auxs <- list(list(name     = "n",
                    equation = 10000),
               list(name     = "I0",
                    equation = 1))

  actual <- get_init_value(stock_obj, auxs, NULL)

  expected <- list(name      = "S",
                   equation  = "-S_to_E",
                   initValue = 9999)

  expect_equal(actual, expected)

})

test_that("get_init_value() returns the expected list with fixed init", {

  stock_obj <- list(name      = "S",
                    equation  = "-S_to_E",
                    initValue = "n - I0")

  auxs <- list(list(name     = "n",
                    equation = 10000),
               list(name     = "I0",
                    equation = 1))

  fixed_inits <- "I0"

  actual <- get_init_value(stock_obj, auxs, fixed_inits)

  expected <- list(name      = "S",
                   equation  = "-S_to_E",
                   initValue = "(10000) - I0")

  expect_equal(actual, expected)

})

