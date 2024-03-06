
test_that("get_dist_obj() declares the vector for init values", {

  rhs <- "neg_binomial_2(net_flow(C), phi)"

  actual <- get_dist_obj(rhs)

  expected <- list(dist_name = "neg_binomial_2",
                   mu        = "net_flow(C)",
                   phi       = "phi")

  expect_equal(actual, expected)
})

test_that("get_meas_params() deals with a given concentration parameter", {

  meas_mdl         <- list("y ~ neg_binomial_2(net_flow(C), 10)")
  estimated_params <- list(sd_prior("par_beta", "lognormal", c(0, 1)))

  actual   <- get_meas_params(meas_mdl, estimated_params)
  expected <- estimated_params

  expect_equal(actual, expected)
})

test_that("get_meas_params() handles priors for measurement parameters", {

  meas_mdl <- list("y1 ~ lognormal(log(Lynx), sigma_1)")

  estimated_params <- list(sd_prior("par_alpha", "normal", c(1, 0.5),
                                    min_0 = TRUE),
                           sd_prior("sigma_1", "lognormal", c(-1, 1)))

  actual   <- get_meas_params(meas_mdl, estimated_params)

  expected <- list(sd_prior("par_alpha", "normal", c(1, 0.5),
                            min_0 = TRUE),
                   list(par_name = "sigma_1",
                        dist     = "lognormal",
                        mu       = -1,
                        sigma    = 1,
                        min      = 0,
                        type     = "meas_par"))

  expect_equal(actual, expected)
})

test_that("translate_stock_text() returns the expected object", {

  lvl_names     <- c("Hares", "Lynx")
  delta_counter <- 1
  stock_txt     <- "log(Hares[0])"

  actual <- translate_stock_text(stock_txt, delta_counter, lvl_names)

  expected <- list(stock_txt     = "log(x0[1])",
                   delta_counter = 1)

  expect_equal(actual, expected)
})

#---------tidy_meas_params()------------------------------------------------

test_that("tidy_meas_params() returns the expected list", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual   <- tidy_meas_params(meas_obj, list())

  expected <- list(
    list(par_name  = "inv_phi",
         dist      = "exponential",
         beta      = 5,
         min       = 0,
         type      = "meas_par",
         par_trans = "inv"))

  expect_equal(actual, expected)
})
