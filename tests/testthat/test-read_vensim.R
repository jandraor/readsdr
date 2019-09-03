context("Read Vensim file")

library(deSolve)
library(igraph)
library(purrr)

files <- c("SIR.mdl", "SEIR.mdl")

test_that("the output from read_vensim() is a list", {
  for(file in files) {
    mdl  <- read_vensim(file)
    expect_is(mdl, "list")
  }
})

test_that("the output from read_vensim() produces the required elements", {
  for(file in files) {
    mdl   <- read_vensim(file)
    expected_properties <- names(mdl)
    expect_equal("description" %in% expected_properties, TRUE)
    expect_equal("deSolve_components" %in% expected_properties, TRUE)
    expect_equal("graph_dfs" %in% expected_properties, TRUE)
  }
})

test_that("it returns the correct number of levels", {
  expected_levels <- c(3, 4)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_vensim(file)
    n_levels  <- length(mdl$description$levels)
    expect_equal(n_levels, expected_levels[index])
  }
})

test_that("it returns the correct number of variables", {
  expected_variables <- c(5, 6)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_vensim(file)
    n_vars    <- length(mdl$description$variables)
    expect_equal(n_vars, expected_variables[index])
  }
})

test_that("it returns the correct number of constants", {
  expected_constants <- c(4, 4)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_vensim(file)
    n_consts  <- length(mdl$description$constants)
    expect_equal(n_consts, expected_constants[index])
  }
})

test_that("it returns a runnable model", {

  for(file in files) {
    mdl       <- read_vensim(file)
    START     <- as.numeric(mdl$description$parameters[[2]]$value)
    FINISH    <- as.numeric(mdl$description$parameters[[1]]$value)
    STEP      <- as.numeric(mdl$description$parameters[[4]]$value)

    # Create time vector
    simtime <- seq(START, FINISH, by = STEP)

    o <- data.frame(ode(y = mdl$deSolve_components$stocks,
                        times = simtime,
                        func = mdl$deSolve_components$func,
                        parms = mdl$deSolve_components$consts,
                        method = "euler"))

    expect_is(o, 'data.frame')
  }
})

test_that("it returns the correct inputs for constructing a graph", {

  for(file in files) {
    mdl       <- read_vensim(file)

    gr <- graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)
    expect_is(gr, "igraph")
  }

})

test_that("read_vensim() returns the correct number of edges", {
  expected_edges <- c(10, 17)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_vensim(file)
    gr <- graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)
    n_edges <- gsize(gr)

    expect_equal(n_edges, expected_edges[index])
    }
})

test_that("read_vensim() returns the correct number of nodes", {
  expected_nodes <- c(8, 10)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_vensim(file)

    gr <- graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)
    n_nodes <- gorder(gr)

    expect_equal(n_nodes, expected_nodes[index])
  }
})

test_that("read_vensim() returns the correct structure for identifying strong components", {
  expected_sc <- c(2, 1)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_vensim(file)
    gr <- graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)

    strong_components   <- components(gr, mode = "strong")
    n_strong_components <- strong_components$no

    expect_equal(n_strong_components, expected_sc[index])
  }
})

test_that("read_vensim() returns the correct structure for identifying nodes in strong components", {
  expected_nodes_sc <- c(9, 17)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_vensim(file)

    gr <- graph_from_data_frame(mdl$graph_dfs$edges, directed = T,
                                vertices = mdl$graph_dfs$nodes)

    members <- membership(clusters(gr, mode = "strong"))

    strong_subgraphs <- map(unique(members), gr = gr, function (x, gr){
      induced.subgraph(gr, which(members == x)) })

    subgraphs_sizes <- sapply(strong_subgraphs,
                              function(subgraph) gsize(subgraph))

    n_edges_in_sc <- sum(subgraphs_sizes)

    expect_equal(n_edges_in_sc, expected_nodes_sc[index])
  }
})

