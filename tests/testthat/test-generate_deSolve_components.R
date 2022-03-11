context("Generate deSolve components")

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

structure_m2 <- list(
  parameters = NULL,
  levels = list(
    list(name      = "Population",
         equation  = "net_growth",
         initValue = 100)
  ),
  variables = list(
    list(name     = "net_growth",
         equation = "Population*growth_rate*non_linear_effect"),

    list(name      = "non_linear_effect",
         equation  = "f_non_linear_effect(Population)",
         graph_fun = list(
           name = "f_non_linear_effect",
           fun  = approxfun(
             x      = 100:105,
             y      = c(2, 2, 2, 1, 1, 1),
             method = "linear",
             yleft  = 2,
             yright = 1
           )
         ))
  ),
  constants = list(
    list(name  = "growth_rate",
         value = 0.01)
  )
)

# get_deSolve_elems()-----------------------------------------------------------
test_that("get_deSolve_elems() returns the basic elements", {
  expect_named(get_deSolve_elems(structure_m1),
               c("stocks", "consts", "func", "sim_params"))
})

test_that("get_deSolve_elems() returns the graph_fun element", {
  expect_named(get_deSolve_elems(structure_m2),
               c("stocks", "consts", "func", "sim_params", "graph_funs"))
})

test_that("get_deSolve_elems handles models with FIXED DELAYS", {
  mdl_structure <- list(parameters = list(),
                        levels     = list(
                          list(name      = "test",
                               equation  = "inflow-outflow",
                               initValue = 0)
                        ),
                        variables = list(
                          list(name     = "inflow",
                               equation = "3+ifelse(time>=2,3,0)"),
                          list(name     = "outflow",
                               equation = "sd_fixed_delay('inflow',time,2,0,.memory)")
                        ),
                        constants = list())

  actual_obj <- get_deSolve_elems(mdl_structure)

  func_body <- paste(
    'with(as.list(c(stocks, auxs)), {',
    'inflow <- 3 + ifelse(time >= 2, 3, 0)',
    'outflow <- sd_fixed_delay("inflow", time, 2, 0, .memory)',
    'd_test_dt <- inflow - outflow',
    '.memory[as.character(time), c("inflow")] <<- inflow',
    'return(list(c(d_test_dt), inflow = inflow, outflow = outflow))',
    '})', sep = "\n")

  model_func <- rlang::new_function(
    args = rlang::exprs(time = , stocks =, auxs = ),
    body = rlang::parse_expr(func_body)
  )

  expected_obj <- list(
    stocks       = c(test = 0),
    consts       = list(),
    func         = model_func,
    sim_params   = list(),
    delayed_vars = c("inflow"))

  actual_val <- all.equal(actual_obj, expected_obj, check.environment = FALSE)

  expect_equal(actual_val, TRUE)
})

# construct_return_statement()--------------------------------------------------

test_that("construct_return_statement() works when there are no constants", {
  test_s <- list(list(name = "Price"))
  test_v <- list(list(name = "demand_price_schedule"))
  test_c <- list() # test constants
  actual   <- construct_return_statement(test_s, test_v, test_c)
  expected <- "return (list(c(d_Price_dt),\ndemand_price_schedule = demand_price_schedule))"
  expect_equal(actual, expected)
})

test_that("construct_return_statement() works when there are no variables", {
  test_s <- list(list(name = "Population"))
  test_v <- list() # test variables
  test_c <- list(list(name = "constant_growth")) # test constants
  actual   <- construct_return_statement(test_s, test_v, test_c)
  expected <- "return (list(c(d_Population_dt),\nconstant_growth = constant_growth))"
  expect_equal(actual, expected)
})

# generate_gf_list--------------------------------------------------------------

test_that("generate_gf_list() returns the expected list", {

  test_variables <-  list(
    list(name     = "net_growth",
         equation = "Population*growth_rate*non_linear_effect"),

    list(name      = "non_linear_effect",
         equation  = "f_non_linear_effect(Population)",
         graph_fun = list(
           name = "f_non_linear_effect",
           fun  = stats::approxfun(
             x      = 100:105,
             y      = c(2, 2, 2, 1, 1, 1),
             method = "linear",
             yleft  = 2,
             yright = 1
           )
         ))
  )

  expected_obj <- list(
    f_non_linear_effect = stats::approxfun(
      x      = 100:105,
      y      = c(2, 2, 2, 1, 1, 1),
      method = "linear",
      yleft  = 2,
      yright = 1
  ))

  actual_val <- all.equal(generate_gf_list(test_variables), expected_obj)
  expect_equal(actual_val, TRUE)
})

# generate_model_func-----------------------------------------------------------

test_that("generate_model_func() returns the expected fun", {
  func_body <- paste(
    'with(as.list(c(stocks, auxs)), {',
    'net_growth <- Population * growth_rate',
    'd_Population_dt <- net_growth',
    'return(list(c(d_Population_dt), net_growth = net_growth,',
    'growth_rate = growth_rate))',
    '})', sep = "\n")

  model_func <- rlang::new_function(
    args = rlang::exprs(time = , stocks =, auxs = ),
    body = rlang::parse_expr(func_body)
  )

  actual_obj <- generate_model_func(structure_m1$variables, structure_m1$levels,
                                    structure_m1$constants, FALSE, NULL)

  actual_val <- all.equal(actual_obj, model_func, check.environment = FALSE)

  expect_equal(actual_val, TRUE)
})

test_that("generate_model_func() works for models with graphical functions", {
  func_body <- paste(
    'with(as.list(c(stocks, auxs, graph_funs)), {',
    'non_linear_effect <- f_non_linear_effect(Population)',
    'net_growth <- Population * growth_rate * non_linear_effect',
    'd_Population_dt <- net_growth',
    'return(list(c(d_Population_dt), non_linear_effect = non_linear_effect,',
    'net_growth = net_growth, growth_rate = growth_rate))',
    '})', sep = "\n")

  model_func <- rlang::new_function(
    args = rlang::exprs(time = , stocks =, auxs = , graph_funs =),
    body = rlang::parse_expr(func_body)
  )

  actual_obj <- generate_model_func(structure_m2$variables, structure_m2$levels,
                                    structure_m2$constants, TRUE, NULL)

  actual_val <- all.equal(actual_obj, model_func, check.environment = FALSE)

  expect_equal(actual_val, TRUE)
})
