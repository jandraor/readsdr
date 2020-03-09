test_that("translate_Vensim_graph_func() returns the expected object", {
  test_equation <- "WITHLOOKUP(Price,([(0,10)-(50,100)],(5,100),(10,73),(15,57),(20,45),(25,35),(30,28),(35,22),(40,18),(45,14),(50,10)))"

  actual_obj    <- translate_Vensim_graph_func(test_equation)
  expected_obj  <- list(
    input  = "Price",
    graph_fun = approxfun(
      x = seq(5, 50, 5),
      y = c(100, 73, 57, 45, 35, 28, 22, 18, 14, 10),
      method = "linear",
      yleft  = 100,
      yright = 10))

  comparison_result <- all.equal(actual_obj, expected_obj)

  expect_equal(comparison_result, TRUE)
})

test_that("translate_graph_func() returns the expected object", {
  test_gf_xml <- xml2::read_xml('
    <root>
      <doc1 xmlns = "http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
        <aux name="demand price schedule">
				  <eqn>Price</eqn>
				  <gf>
				    <xscale min="5" max="50"/>
					  <yscale min="0" max="2"/>
					  <ypts>100,73,57,45,35,28,22,18,14,10</ypts>
				  </gf>
			  </aux>
      </doc1>
    </root>')%>%
    xml2::xml_find_first(".//d1:gf")

  actual_obj    <- translate_graph_func(test_gf_xml)

  expected_obj  <- approxfun(
      x = seq(5, 50, 5),
      y = c(100, 73, 57, 45, 35, 28, 22, 18, 14, 10),
      method = "linear",
      yleft  = 100,
      yright = 10)

  comparison_result <- all.equal(actual_obj, expected_obj)

  expect_equal(comparison_result, TRUE)
})
