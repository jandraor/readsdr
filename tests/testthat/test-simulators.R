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

deSolve_m3 <- list(
  stocks = c(Population = 100,
             Cumulative_Deaths = 0),
  consts = c(birth_rate = 0.01, death_rate = 0.01),
  func   =
    function(time, stocks, auxs) {
      with(as.list(c(stocks, auxs)), {
        births                 <- Population * birth_rate
        deaths                 <- Population * death_rate
        d_Population_dt        <- births - deaths
        d_Cumulative_Deaths_dt <- deaths
        return(list(c(d_Population_dt, d_Cumulative_Deaths_dt), births = births,
                    deaths = deaths,
                    birth_rate = birth_rate,
                    death_rate = death_rate))
      })
    },
  sim_params = list(start = 0, stop = 1, dt = 0.25))


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

test_that("sd_sensitivity_run() returns the expected output for constant sensitivity", {
  consts_df <- data.frame(growth_rate = c(0.01, 0.02))
  output    <- sd_sensitivity_run(deSolve_m1, consts_df = consts_df,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(101.003756, 102.015050))
})

test_that("sd_sensitivity_run() returns the expected output for sensitivity of initial values ", {
  stocks_df <- data.frame(Population = c(10,100))
  output    <- sd_sensitivity_run(deSolve_m1, stocks_df = stocks_df,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(10.100376, 101.003756))
})

test_that("sd_sensitivity_run() works for simultaneous sensitivity to stocks and constants", {
  consts_df <- data.frame(growth_rate = c(0.01, 0.02))
  stocks_df <- data.frame(Population = c(100, 10))
  output    <- sd_sensitivity_run(deSolve_m1, consts_df = consts_df,
                                  stocks_df = stocks_df,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(101.003756, 	10.2015050))
})

test_that("sd_sensitivity_run() returns an error when the input data frames are
of unequal size", {
  consts_df <- data.frame(growth_rate = c(0.01))
  stocks_df <- data.frame(Population = c(100, 10))

  expect_error(
    sd_sensitivity_run(deSolve_m1, consts_df = consts_df,
                       stocks_df = stocks_df),
    "the number of rows in both data frames (consts & stocks) must be of equal size",
    fixed = TRUE)
})

test_that("sd_sensitivity_run() works for models with graph funs", {
  consts_df <- data.frame(growth_rate = c(0.01, 0.02))
  output    <- sd_sensitivity_run(deSolve_m2, consts_df = consts_df,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(102.015050, 103.540124))
})

test_that("sd_sensitivity_run() works for a subset of constants", {
  consts_df <- data.frame(death_rate = c(0, 0.01))
  output    <- sd_sensitivity_run(deSolve_m3, consts_df = consts_df,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(101.003756, 100))
})

test_that("sd_sensitivity_run() works for a subset of stocks", {
  stocks_df <- data.frame(Population = c(10,100))
  output    <- sd_sensitivity_run(deSolve_m3, stocks_df = stocks_df,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(10, 100))
})

test_that("sd_sensitivity_run() works for a subset of constants & stocks", {
  stocks_df <- data.frame(Population = c(10, 100))
  consts_df <- data.frame(death_rate = c(0, 0.01))
  output    <- sd_sensitivity_run(deSolve_m3, consts_df = consts_df,
                                  stocks_df = stocks_df,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(10.1003756, 100))
})

test_that("sd_sensitivity_run() deals with the order of the stocks", {
  stocks_df <- data.frame(Cumulative_Deaths = c(0, 10))
  output    <- sd_sensitivity_run(deSolve_m3, stocks_df = stocks_df,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Cumulative_Deaths"], c(1, 11))
})

test_that("sd_sensitivity_run() works with several cores", {
  consts_df <- data.frame(growth_rate = c(0.01, 0.02))
  output    <- sd_sensitivity_run(deSolve_m1, consts_df = consts_df,
                                  multicore = TRUE, n_cores = 2,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(101.003756, 102.015050))


  stocks_df <- data.frame(Population = c(10,100))
  output    <- sd_sensitivity_run(deSolve_m1, stocks_df = stocks_df,
                                  multicore = TRUE, n_cores = 2,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(10.100376, 101.003756))


  consts_df <- data.frame(growth_rate = c(0.01, 0.02))
  stocks_df <- data.frame(Population = c(100, 10))
  output    <- sd_sensitivity_run(deSolve_m1, consts_df = consts_df,
                                  stocks_df = stocks_df,
                                  multicore = TRUE, n_cores = 2,
                                  reporting_interval = 0.25)
  expect_equal(output[c(5, 10), "Population"], c(101.003756, 	10.2015050))
})

test_that("sd_sensitivity_run() throws an error if supplied an inexistent const", {

  filepath  <- system.file("models/", "SEIR.stmx", package = "readsdr")
  ds_inputs <- xmile_to_deSolve(filepath)

  expect_error(sd_sensitivity_run(ds_inputs,
                                  consts_df = data.frame(par_zeta = 1:3)))
})

test_that("sd_sensitivity_run() filters rows at intermediate steps", {

  filepath  <- system.file("models/", "SEIR.stmx", package = "readsdr")
  ds_inputs <- xmile_to_deSolve(filepath)

  actual <- sd_sensitivity_run(ds_inputs,
                               consts_df = data.frame(par_beta = c(0.5, 1, 1.25)),
                               start_time = 1, stop_time = 3)

  expect_equal(actual$time, rep(1:3, 3))

  actual <- sd_sensitivity_run(ds_inputs,
                               consts_df = data.frame(par_beta = c(0.5, 1, 1.25)),
                               stocks_df = data.frame(S = c(1000, 1000, 1000)),
                               start_time = 1, stop_time = 3)

  expect_equal(actual$time, rep(1:3, 3))

  actual <- sd_sensitivity_run(ds_inputs,
                               stocks_df = data.frame(S = c(1000, 1000, 1000)),
                               start_time = 1, stop_time = 3)

  expect_equal(actual$time, rep(1:3, 3))
})
