stocks    <- list(list(name = "population",
                       equation = "births",
                       initValue = 100))

variables <- list(list(name = "births", equation = "population*birthRate"),
                  list(name = "birthRate", equation = "birthRate2"))

constants <- list(list(name = "birthRate2", value = 0.1))

stocks2    <- stocks
variables2 <- list(list(name = "births", equation = "population*birthRate"))
constants2 <- list(list(name = "birthRate", value = 0.1))

test_that("generate_edges_df() ignores info-links whose tail is a constant", {
  edges_df <- generate_edges_df(stocks, variables, constants)
  expect_equal(nrow(edges_df), 3)
})

test_that("generate_nodes_df replaces auxiliar consts with their value in equations", {
  nodes_df <- generate_nodes_df(stocks2, variables2, constants2)
  expect_equal(nodes_df[[2, "equation"]], "population*0.1")
})
