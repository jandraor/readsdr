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


  actual_val      <- create_level_obj_xmile(test_stocks_xml, variables,
                                            constants, builtin_stocks)
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

  actual_obj <- create_level_obj_xmile(test_stocks_xml, test_vars, test_consts)

  expect_equal(actual_obj, expected_obj)
})

test_that("extract_stock_info() handles a stock vector", {
  expected_obj <- list(
    list(name = "Population_A", equation = "growth_A", initValue = "100"),
    list(name = "Population_B", equation = "growth_B", initValue = "200"))

  actual_obj <- extract_stock_info(test_stocks_xml[[1]], dims_obj = NULL)

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

  dims_obj <- list(Age = c(1:4))

  actual_obj <- extract_stock_info(test_stocks_xml[[1]], dims_obj)

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
  expected_obj <- list(Age = c(1:4),
                       Interactions = c("b11", "b12"))

  expect_equal(actual_obj, expected_obj)
})
