context("Generate igraph inputs")

structure_m1 <- list(
  parameters = NULL,
  levels = list(
    list(name      = "Population",
         equation  = "net_growth",
         initValue = 100)
  ),
  variables = list(
    list(name     = "net_growth",
         equation = "Population*growth_rate")
  ),
  constants = list(
    list(name  = "growth_rate",
         value = 0.01)
  ))

#get_igraph_inputs()------------------------------------------------------------

test_that("get_igraph_inputs() returns the expected elements", {
  expect_named(get_igraph_inputs(structure_m1), c("nodes", "edges"))
})

test_that("get_igraph_inputs() valid inputs for igraph", {
  graph_dfs <- get_igraph_inputs(structure_m1)

  gr        <- igraph::graph_from_data_frame(graph_dfs$edges, directed = T,
                                             vertices = graph_dfs$nodes)
  expect_is(gr, "igraph")
})



stocks    <- list(list(name = "population",
                       equation = "births",
                       initValue = 100))


variables <- list(list(name = "births", equation = "population*birthRate"),
                  list(name = "birthRate", equation = "birthRate2"))

constants <- list(list(name = "birthRate2", value = 0.1))

stocks2    <- stocks
variables2 <- list(list(name = "births", equation = "population*birthRate"))
constants2 <- list(list(name = "birthRate", value = 0.1))

# generate_edges_df()-----------------------------------------------------------

test_that("generate_edges_df() returns the correct number of edges", {
  e_df <- with(structure_m1, generate_edges_df(levels, variables, constants))
  expect_equal(nrow(e_df), 2)
})

test_that("generate_edges_df() returns the correct number of flows", {
  e_df <- with(structure_m1, generate_edges_df(levels, variables, constants))
  e_df <- e_df[e_df$type == "flow", ]
  expect_equal(nrow(e_df), 1)
})

test_that("generate_edges_df() ignores info-links whose tail is a constant", {
  edges_df <- generate_edges_df(stocks, variables, constants)
  expect_equal(nrow(edges_df), 3)
})

# generate_nodes_df()-----------------------------------------------------------

test_that("generate_nodes_df() returns a df with the correct columns", {
  actual_val <- with(structure_m1,
                    generate_nodes_df(levels, variables, constants))

  expect_named(actual_val, c("name", "type", "equation"))
})

test_that("generate_nodes_df() returns the correct number of nodes", {
  df <- with(structure_m1, generate_nodes_df(levels, variables, constants))
  expect_equal(nrow(df), 2)
})

test_that("generate_nodes_df() replaces auxiliar consts with their value in equations", {
  nodes_df <- generate_nodes_df(stocks2, variables2, constants2)
  expect_equal(nodes_df[[2, "equation"]], "population*0.1")
})

test_that("generate_nodes_df() throws an error should a variable directly
depends on time", {

  variables <- list(
    list(name     = "net_growth",
         equation = "ifelse(time>1Population*growth_rate,0)")
  )
  expect_error(
    generate_nodes_df(structure_m1$levels, variables, structure_m1$constants),
    "A variable depends on time")
})

# construct_var_edge()----------------------------------------------------------

test_that("construct_var_edge() ignores scalars in equations", {
  var_obj <- list(name = "w",
                  equation = "x*y/(k+1/z)")

  const_names_test <- c("k")
  actual    <- construct_var_edge(var_obj, const_names_test)
  expected  <- data.frame(from = c("x", "y", "z"), to = "w", type = "info_link",
                          stringsAsFactors = FALSE)
  expect_equal(actual, expected)
})

test_that("construct_var_edge() accounts for repeated variables", {
  var_obj <- list(name = "z",
                  equation = "(x+y)/x")

  const_names_test <- c("k")
  actual    <- construct_var_edge(var_obj, const_names_test)
  expected  <- data.frame(from = c("x", "y"), to = "z", type = "info_link",
                          stringsAsFactors = FALSE)
  expect_equal(actual, expected)
})

# construct_stock_edge()--------------------------------------------------------

test_that('construct_stock_edge returns NULL constant-alike stocks', {
  stock_obj <- list(name     = "test_stock",
                    equation = "0")
  expect_equal(construct_stock_edge(stock_obj), NULL)
} )
