context("Vars & Consts xmile")

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

smooth3_xml <-  xml2::read_xml(
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
         <aux name="S3">
           <eqn>SMTH3(0.5,  6,  1)</eqn>
         </aux>
       </variables>
     </doc1>
   </root>')

smooth4_xml <-  xml2::read_xml(
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
         <aux name="SN">
           <eqn>SMTHN(0.5, 8, 4,1)</eqn>
         </aux>
       </variables>
     </doc1>
   </root>')

test_that("create_vars_consts_obj_xmile() returns the expected variables &
constants", {
  vars_consts <- xml2::read_xml(test_model) %>%
    xml2::xml_find_all(".//d1:flow|.//d1:aux")

  output <- create_vars_consts_obj_xmile(vars_consts, "isee",
                                         const_list = NULL)

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

            expect_equal(actual_obj, expected_obj, check.environment = FALSE)
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

  actual_obj   <- create_vars_consts_obj_xmile(test_var_xml, "Vensim",
                                               const_list = NULL)

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

test_that("create_vars_consts_obj_xmile() translates SMTH1 builtin", {
  auxs_xml     <- xml2::xml_find_all(smooth1_xml, ".//d1:flow|.//d1:aux")

  actual_obj   <- create_vars_consts_obj_xmile(auxs_xml, "isee")

  expected_obj <- list(
    variables = list(
      list(name     = "adjust_S1",
           equation = "(0.5-S1)/5")),
    constants = list(),
    builtin_stocks = list(
      list(name      = "S1",
           equation  = "adjust_S1",
           initValue = 1)
    )
  )

  expect_equal(actual_obj, expected_obj)
})

test_that("create_vars_consts_obj_xmile() translates SMTH3 builtin", {
  auxs_xml     <- xml2::xml_find_all(smooth3_xml, ".//d1:flow|.//d1:aux")

  actual_obj   <- create_vars_consts_obj_xmile(auxs_xml, "isee")

  expected_obj <- list(
    variables = list(
      list(name     = "adjust_S3",
           equation = "(S3_2-S3)/2"),
      list(name     = "adjust_S3_2",
           equation = "(S3_3-S3_2)/2"),
      list(name     = "adjust_S3_3",
           equation = "(0.5-S3_3)/2")),
    constants = list(),
    builtin_stocks = list(
      list(name      = "S3",
           equation  = "adjust_S3",
           initValue = 1),
      list(name      = "S3_2",
           equation  = "adjust_S3_2",
           initValue = 1),
      list(name      = "S3_3",
           equation  = "adjust_S3_3",
           initValue = 1)
    )
  )

  expect_equal(actual_obj, expected_obj)
})

test_that("create_vars_consts_obj_xmile() translates SMTHN builtin for N > 1", {
  auxs_xml     <- xml2::xml_find_all(smooth4_xml, ".//d1:flow|.//d1:aux")

  actual_obj   <- create_vars_consts_obj_xmile(auxs_xml, "isee")

  expected_obj <- list(
    variables = list(
      list(name     = "adjust_SN",
           equation = "(SN_2-SN)/2"),
      list(name     = "adjust_SN_2",
           equation = "(SN_3-SN_2)/2"),
      list(name     = "adjust_SN_3",
           equation = "(SN_4-SN_3)/2"),
      list(name     = "adjust_SN_4",
           equation = "(0.5-SN_4)/2")
    ),
    constants = list(),
    builtin_stocks = list(
      list(name      = "SN",
           equation  = "adjust_SN",
           initValue = 1),
      list(name      = "SN_2",
           equation  = "adjust_SN_2",
           initValue = 1),
      list(name      = "SN_3",
           equation  = "adjust_SN_3",
           initValue = 1),
      list(name      = "SN_4",
           equation  = "adjust_SN_4",
           initValue = 1)
    )
  )

  expect_equal(actual_obj, expected_obj)
})

test_that("create_vars_consts_obj_xmile() handles apply all for constant vector", {
  test_var_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
			  <aux name="growth rate">
				  <dimensions>
					  <dim name="Region"/>
				  </dimensions>
				  <eqn>0.1</eqn>
			  </aux>
      </variables>
    </doc1>
  </root>')

  auxs_xml <- xml2::xml_find_all(test_var_xml, ".//d1:flow|.//d1:aux")

  dims_obj <- list(global_dims = list(Region = c("A", "B")))

  actual_obj   <- create_vars_consts_obj_xmile(auxs_xml, "isee", dims_obj,
                                               const_list = NULL)

  expected_obj <- list(
    variables = NULL,
    constants = list(
      list(name  = "growth_rate_A",
           value = 0.1),
      list(name  = "growth_rate_B",
           value = 0.1)
    ))

  expect_equal(actual_obj, expected_obj)
})

test_that("create_vars_consts_obj_xmile() handles Stella's apply all for equations", {

  test_var_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
      <variables>
				<flow name="I to R">
				<dimensions>
					<dim name="Age"/>
				</dimensions>
				<eqn>par_gamma * I</eqn>
			</flow>
      </variables>
    </doc1>
  </root>')

  auxs_xml <- xml2::xml_find_all(test_var_xml, ".//d1:flow|.//d1:aux")

  dims_obj <- list(global_dims = list(Age = c("1", "2")),
                   dictionary  = list(I = "Age"))

  vendor <- "isee"

  actual_obj   <- create_vars_consts_obj_xmile(auxs_xml, vendor, dims_obj)

  expected_obj <- list(
    variables = list(list(name     = "I_to_R_1",
                          equation = "par_gamma*I_1"),
                     list(name     = "I_to_R_2",
                          equation = "par_gamma*I_2")),
    constants = list())

  expect_equal(actual_obj, expected_obj)
})



test_that("classify_elems() handles a 2 dimensional arrayed constant from Vensim", {


  filepath      <- "./2d_pop.xmile"
  raw_xml       <- safe_read(filepath)
  vendor        <- which_vendor(raw_xml)
  variables_xml <- xml2::xml_find_first(raw_xml, ".//d1:variables")
  dims_obj      <- create_dims_obj(raw_xml)
  auxs_xml      <- xml2::xml_find_all(variables_xml, ".//d1:flow|.//d1:aux")

  elem_list     <- lapply(auxs_xml, classify_elems, vendor = vendor,
                          dims_obj = dims_obj)

  actual <- elem_list[[2]]$consts

  expected <- list(
    list(name     = "Growth_rate_Westeros_Young",
         value    = 0.01),
    list(name     = "Growth_rate_Westeros_Old",
         value    = 0.1),
    list(name     = "Growth_rate_Essos_Young",
         value    = 0.05),
    list(name     = "Growth_rate_Essos_Old",
         value    = 0.05))

  expect_equal(actual, expected)

})

#-------interpret_non_consts----------------------------------------------------

test_that("interpret_non_consts() handles a arrayed variable", {

  test_var_xml <- xml2::read_xml('
  <root>
    <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
    <variables>
			<flow name="growth">
				<dimensions>
					<dim name="region"/>
				</dimensions>
				<element subscript="A">
					<eqn>Population[A] * growth_rate[A]</eqn>
				</element>
				<element subscript="B">
					<eqn>Population[B] * growth_rate[B]</eqn>
				</element>
			</flow>
		</variables>
    </doc1>
  </root>')

  auxs_xml <- xml2::xml_find_all(test_var_xml, ".//d1:flow|.//d1:aux")

  non_const_obj <- list(
    elems = list(
      list("growth_A",
           "Population_A*growth_rate_A"),
      list("growth_B",
           "Population_B*growth_rate_B")),
    aux_name = "growth")

  actual_obj <- interpret_non_consts(non_const_obj, "isee", list(), auxs_xml)

  expected_obj <- list(
    vars = list(
      list(name     = "growth_A",
           equation = "Population_A*growth_rate_A"),
      list(name     = "growth_B",
           equation = "Population_B*growth_rate_B")))

  expect_equal(actual_obj, expected_obj)
})

test_that("interpret_non_consts() handles a 2 dimensional arrayed variable from Vensim", {

  filepath        <- "./2d_pop.xmile"
  raw_xml         <- safe_read(filepath)
  vendor          <- which_vendor(raw_xml)
  variables_xml   <- xml2::xml_find_first(raw_xml, ".//d1:variables")
  dims_obj        <- create_dims_obj(raw_xml)
  auxs_xml        <- xml2::xml_find_all(variables_xml, ".//d1:flow|.//d1:aux")

  non_const_obj <- list(
    elems = list(
      list("Net_growth_Westeros_Young",
           "Population_Westeros_Young*Growth_rate_Westeros_Young"),
      list("Net_growth_Westeros_Old",
           "Population_Westeros_Old*Growth_rate_Westeros_Old"),
      list("Net_growth_Essos_Young",
           "Population_Essos_Young*Growth_rate_Essos_Young"),
      list("Net_growth_Essos_Old",
           "Population_Essos_Old*Growth_rate_Essos_Old")),
    aux_name = "Net_growth")

  actual <- interpret_non_consts(non_const_obj, vendor, list(), auxs_xml)

  expected <- list(
    vars = list(
      list(name     = "Net_growth_Westeros_Young",
           equation = "Population_Westeros_Young*Growth_rate_Westeros_Young"),
      list(name     = "Net_growth_Westeros_Old",
           equation = "Population_Westeros_Old*Growth_rate_Westeros_Old"),
      list(name     = "Net_growth_Essos_Young",
           equation = "Population_Essos_Young*Growth_rate_Essos_Young"),
      list(name     = "Net_growth_Essos_Old",
           equation = "Population_Essos_Old*Growth_rate_Essos_Old")))

  expect_equal(actual, expected)

})

test_that("interpret_non_consts() handles DELAYN from Stella", {

  filepath      <- system.file("models/", "SEjIkR.stmx", package = "readsdr")
  raw_xml       <- safe_read(filepath)
  vendor        <- which_vendor(raw_xml)
  variables_xml <- xml2::xml_find_first(raw_xml, ".//d1:variables")
  auxs_xml      <- xml2::xml_find_all(variables_xml, ".//d1:flow|.//d1:aux")

  consts <- list(list(name  = "j",
                      value = 2))

  non_const_obj <- list(elems = list(list("E_to_I",
                                          "DELAYN(S_to_E,2,j,0)")),
                        aux_name = "E_to_I")

  actual <- interpret_non_consts(non_const_obj, vendor, consts, auxs_xml, NULL)

  expected <- list(
    vars = list(
      list(name     = "E_to_I",
           equation = "dly_S_to_E_2_out"),
      list(name     = "dly_S_to_E_1_out",
           equation = "dly_S_to_E_1/((2)/2.0)"),
      list(name     = "dly_S_to_E_2_out",
           equation = "dly_S_to_E_2/((2)/2.0)")),
    builtin_stocks = list(
      list(name      = "dly_S_to_E_1",
           equation  = "S_to_E - dly_S_to_E_1_out",
           initValue = 0),
      list(name      = "dly_S_to_E_2",
           equation  = "dly_S_to_E_1_out - dly_S_to_E_2_out",
           initValue = 0)))

  expect_equal(actual, expected)
})


#override_consts()--------------------------------------------------------------

test_that("override_consts() works for a single change in multiple options", {

  actual_consts <- list(list(name  = "growth_rate2",
                             value = 0.1),
                        list(name  = "growth_rate1",
                             value = 0.1),
                        list(name  = "growth_rate3",
                             value = 0.1))

  const_list <- list(growth_rate1 = 0.2)


  actual_obj <- override_consts(actual_consts, const_list)

  expected_obj <- actual_consts
  expected_obj[[2]]$value <- 0.2

  expect_equal(actual_obj, expected_obj)
})

test_that("override_consts throws an error when the constant doesn't exist", {
  mdl_structure <- list(constants =
                          list(list(name  = "growth_rate1",
                                    value = 0.1)))

  const_list <- list(growth_rate2 = 0.2)

  expect_error(override_consts(mdl_structure, const_list),
               "Can't find constant: growth_rate2")
})

