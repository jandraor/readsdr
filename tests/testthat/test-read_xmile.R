context("Read xmile file")

files <- c("SIR.xmile")

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

test_that("read_xmile() returns the correct number of levels", {
  expected_levels <- c(3)

  for(file in files) {
    index     <- which(file == files)
    mdl       <- read_xmile(file)
    n_levels  <- length(mdl$description$levels)
    expect_equal(n_levels, expected_levels[index])
  }
})

