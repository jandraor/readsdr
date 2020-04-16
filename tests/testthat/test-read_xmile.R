context("Read xmile file")

#SIR.xmile is a xmile file generated from Vensim
files <- c("SIR.xmile", "SIR.stmx")

test_that("the output from read_xmile() is a list", {
  for(file in files) {
    mdl  <- read_xmile(file)
    expect_is(mdl, "list")
  }
})

test_that("the output from read_xmile() produces the required elements", {
  for(file in files) {
    mdl   <- read_xmile(file)
    expected_properties <- names(mdl)
    expect_equal("description" %in% expected_properties, TRUE)
    expect_equal("deSolve_components" %in% expected_properties, TRUE)
    expect_equal("graph_dfs" %in% expected_properties, TRUE)
  }
})

test_that("read_xmile() returns the correct DT", {
  for(file in files) {
    mdl       <- read_xmile(file)
    actual_dt <- mdl$description$parameters$dt
    expect_equal(actual_dt, 0.125)
  }
})

test_that("read_xmile() returns the correct number of levels", {
  expected_levels <- c(3, 3)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)
    n_levels  <- length(mdl$description$levels)
    expect_equal(n_levels, expected_levels[index])
  }
})

test_that("read_xmile() returns the correct number of variables", {
  expected_variables <- c(5, 5)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)
    n_vars    <- length(mdl$description$variables)
    expect_equal(n_vars, expected_variables[index])
  }
})

test_that("read_xmile() returns the correct number of constants", {
  expected_constants <- c(4, 4)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)
    n_consts  <- length(mdl$description$constants)
    expect_equal(n_consts, expected_constants[index])
  }
})

test_that("read_xmile() returns a runnable model", {

  for(file in files) {
    mdl       <- read_xmile(file)
    START     <- as.numeric(mdl$description$parameters$start)
    FINISH    <- as.numeric(mdl$description$parameters$stop)
    STEP      <- as.numeric(mdl$description$parameters$dt)

    # Create time vector
    simtime <- seq(START, FINISH, by = STEP)

    o <- data.frame(deSolve::ode(y      = mdl$deSolve_components$stocks,
                        times  = simtime,
                        func   = mdl$deSolve_components$func,
                        parms  = mdl$deSolve_components$consts,
                        method = "euler"))

    expect_is(o, 'data.frame')
  }
})

test_that("read_xmile() produces a model function that returns all levels, variables & constants", {
  expected_cols <- c(13, 13) # +1, Time is expected

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)
    START     <- as.numeric(mdl$description$parameters$start)
    FINISH    <- as.numeric(mdl$description$parameters$stop)
    STEP      <- as.numeric(mdl$description$parameters$dt)

    # Create time vector
    simtime <- seq(START, FINISH, by = STEP)

    o <- data.frame(deSolve::ode(y = mdl$deSolve_components$stocks,
                        times = simtime,
                        func = mdl$deSolve_components$func,
                        parms = mdl$deSolve_components$consts,
                        method = "euler"))
    n_of_cols <- ncol(o)
    expect_equal(n_of_cols, expected_cols[index])
  }
})

test_that("read_xmile() returns a nodes-df with the correct columns", {
  for(file in files) {
    mdl   <- read_xmile(file)
    nodes_df <- mdl$graph_dfs$nodes
    col_names <- colnames(nodes_df)
    expect_equal("name" %in% col_names, TRUE)
    expect_equal("type" %in% col_names, TRUE)
    expect_equal("equation" %in% col_names, TRUE)
  }
})

test_that("read_xmile() returns the correct inputs for constructing a graph", {

  for(file in files) {
    mdl       <- read_xmile(file)

    gr <- igraph::graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)
    expect_is(gr, "igraph")
  }

})


test_that("read_xmile() returns the correct number of edges", {
  expected_edges <- c(10, 10)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)
    gr <- igraph::graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)
    n_edges <- igraph::gsize(gr)

    expect_equal(n_edges, expected_edges[index])
  }
})

test_that("read_xmile() returns the correct number of nodes", {
  expected_nodes <- c(8, 8)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)

    gr <- igraph::graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)
    n_nodes <- igraph::gorder(gr)

    expect_equal(n_nodes, expected_nodes[index])
  }
})

test_that("read_xmile() returns the correct number of flows", {
  expected_flows <- c(4, 4)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)

    gr <- igraph::graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)

    n_flows <- length(igraph::E(gr)[[type == "flow"]])

    expect_equal(n_flows, expected_flows[index])
  }
})


test_that("read_xmile() returns the correct structure for identifying strong components", {
  expected_sc <- c(2, 2)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)
    gr <- igraph::graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)

    strong_components   <- igraph::components(gr, mode = "strong")
    n_strong_components <- strong_components$no

    expect_equal(n_strong_components, expected_sc[index])
  }
})

test_that("read_xmile() returns the correct structure for identifying nodes in strong components", {
  expected_nodes_sc <- c(9, 9)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)

    gr <- igraph::graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)

    members <- igraph::membership(igraph::clusters(gr, mode = "strong"))

    strong_subgraphs <- purrr::map(unique(members), gr = gr, function (x, gr){
      igraph::induced.subgraph(gr, which(members == x)) })

    subgraphs_sizes <- sapply(strong_subgraphs,
                              function(subgraph) igraph::gsize(subgraph))

    n_edges_in_sc <- sum(subgraphs_sizes)

    expect_equal(n_edges_in_sc, expected_nodes_sc[index])
  }
})

test_that("read_xmile() reads variables with compound names separated by blanks", {
  file           <- "one_stock_one_inflow.stmx"
  mdl            <- read_xmile(file)
  actual_names   <- c(mdl$description$levels[[1]]$name,
                      mdl$description$constants[[1]]$name)
  expected_names <- c("main_stock", "growth_rate")
  expect_equal(actual_names, expected_names)
})

test_that("read_xmile() returns the expected stock's initial value", {
  file         <- "initByEquation_simplest.stmx"
  mdl          <- read_xmile(file)
  actual_val   <- mdl$description$levels[[1]]$initValue
  expected_val <- 100
  expect_equal(actual_val, expected_val)
})


sd_simulate <- function(mdl, method = "euler") {
  # Create the start time, finish time, and time step
  START  <- mdl$description$parameters$start
  FINISH <- mdl$description$parameters$stop
  STEP   <- mdl$description$parameters$dt

  # Create time vector
  simtime <- seq(START, FINISH, by = STEP)

  data.frame(deSolve::ode(y      = mdl$deSolve_components$stocks,
                 times  = simtime,
                 func   = mdl$deSolve_components$func,
                 parms  = mdl$deSolve_components$consts,
                 method = method))
}

test_that("read_xmile() works for a model that has a NOT statement
from Stella", {
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
				    <eqn>IF(NOT (TIME = 3)) THEN 0 ELSE 1</eqn>
			    </aux>
        </variables>
      </doc1>
    </root>'

  # It is anticipated that this operation will throw a warning
  mdl          <- suppressWarnings(read_xmile(test_model))
  output       <- sd_simulate(mdl)
  actual_val   <- output[output$time == 3.25, "population"]
  expected_val <- 125
  expect_equal(actual_val, expected_val)
})




