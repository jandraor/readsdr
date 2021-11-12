test_that("sd_what_if_from_time() returns the expected output", {

  filepath       <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl            <- read_xmile(filepath)
  ds_components  <- mdl$deSolve_components
  output         <- sd_what_if_from_time(3, par_list = list(c = 4),
                                         ds_inputs = ds_components,
                                         start_time = 0, stop_time = 5,
                                         integ_method = "rk4",
                                         timestep = 1 / 16)
  expect_is(output, "data.frame")

  actual_val <- tail(output[, "Infected"], 1)
  expect_equal(actual_val , 253.706683332, tolerance = 1e-9)
})

test_that("sd_what_from_time() works with the up_to_time parameter", {

  filepath       <- system.file("models/", "SIR.stmx", package = "readsdr")
  mdl            <- read_xmile(filepath)
  ds_components  <- mdl$deSolve_components
  output         <- sd_what_if_from_time(time = 3, up_to_time = 5,
                                         par_list = list(c = 4),
                                         ds_inputs = ds_components,
                                         start_time = 0, stop_time = 10,
                                         integ_method = "rk4",
                                         timestep = 1 / 16)

  expect_is(output, "data.frame")

  actual_val <- tail(output[, "Infected"], 1)
  expect_equal(actual_val, 63.4511067818, tolerance = 1e-9)
})
