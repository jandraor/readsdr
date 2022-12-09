test_that("configure_meas_models() returns the expected list", {

  meas_mdl   <- list("y ~ neg_binomial_2(net_flow(C), phi)")

  meas_params <- list(list(par_name  = "inv_phi",
                           dist      = "exponential",
                           beta      = 5,
                           min       = 0,
                           type      = "meas_par",
                           par_trans = "inv"))

  prior_vals <- list(inv_phi = c(0.62781087, 0.04486981))

  actual <- configure_meas_models(meas_mdl, meas_params, prior_vals)

  expected <- list(list("y ~ neg_binomial_2(net_flow(C), 1.59283639036068)"),
                   list("y ~ neg_binomial_2(net_flow(C), 22.2867001219751)"))

  expect_equal(actual, expected)
})
