
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

    actual_obj   <- create_vars_consts_obj_xmile(auxs_xml)
    expected_obj <- list(variables = list(), constants = list())
    expect_equal(actual_obj, expected_obj)
  })

test_that("create_vars_consts_obj_xmile() ignores aux Time", {

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

  actual_obj   <- create_vars_consts_obj_xmile(auxs_xml)
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

            actual_obj   <- create_vars_consts_obj_xmile(test_var_xml)

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


  actual_obj   <- create_vars_consts_obj_xmile(test_var_xml)

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

#===============================================================================
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

# test_that("create_level_obj_xmile() estimates the initial value for stock which
# depends on a graphical function", {
#
#   test_stocks_xml <- xml2::read_xml('
#   <root>
#     <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
#       <variables>
#         <stock name="Price">
# 				  <eqn>20</eqn>
# 				  <inflow>Change_in_price</inflow>
# 				</stock>
#         <stock name="Inventory">
#           <eqn>Desired_Inventory</eqn>
#           <inflow>Supply</inflow>
#           <outflow>Shipments</outflow>
#         </stock>
#       </variables>
#     </doc1>
#   </root>') %>%
#     xml2::xml_find_all(".//d1:stock")
#
#   test_vars   <- list()
#   test_consts <- list()
#
#   level_obj    <- create_level_obj_xmile(test_stocks_xml,
#                                          test_vars, test_consts)
#   actual_val   <- level_obj[[2]]
#   expected_val <- list(name = "Inventory",
#                        equation = "Supply - Shipments",
#                        initValue = 150)
#   expect_equal(actual_val, expected_val)
# })



