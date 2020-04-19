context("Simulators")

deSolve_m1 <- list(
  stocks = c(Population = 100),
  consts = c(growth_rate = 0.01),
  func   =
    function(time, stocks, auxs) {
      with(as.list(c(stocks, auxs)), {
        net_growth      <- Population * growth_rate
        d_Population_dt <- net_growth
        return(list(c(d_Population_dt), net_growth = net_growth,
                    growth_rate = growth_rate))
      })
    },
  sim_params = list(start = 0, stop = 1, dt = 0.25))

deSolve_m2 <- list(
  stocks = c(Population = 100),
  consts = c(growth_rate = 0.01),
  func   =     function(time, stocks, auxs, graph_funs) {
    with(as.list(c(stocks, auxs, graph_funs)), {
      non_linear_effect <- f_non_linear_effect(Population)
      net_growth        <- Population*growth_rate*non_linear_effect
      d_Population_dt   <- net_growth
      return(list(c(d_Population_dt), net_growth = net_growth,
                  growth_rate = growth_rate))
    })
  },
  sim_params = list(start = 0, stop = 1, dt = 0.25),
  graph_funs = list(
    f_non_linear_effect =
      approxfun(
        x      = 100:105,
        y      = c(2, 2, 2, 1, 1, 1),
        method = "linear",
        yleft  = 2,
        yright = 1
  )))


test_that("sd_simulate() returns the expected output", {
  output <- sd_simulate(deSolve_m1)
  expect_is(output, "data.frame")
  expect_equal(output[nrow(output), "Population"], 101.003756)
})

test_that("sd_simulate() allows changing default sim params", {
  output <- sd_simulate(deSolve_m1, start_time = 1, stop_time = 2,
                        timestep = 0.5)

  expect_equal(output[1, "time"], 1)
  expect_equal(output[nrow(output), "time"], 2)
  expect_equal(nrow(output), 3)
})

test_that("sd_simulate() handles graphical functions", {
  output <- sd_simulate(deSolve_m2)
  expect_equal(output[nrow(output), "Population"], 102.015050)
})
