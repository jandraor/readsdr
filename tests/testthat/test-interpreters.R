test_that("sd_interpret_estimates() returns the expected data frame", {

  estimates <- c(par_beta = 0,
                 par_rho  = 0.8472979,
                 I0       = 0,
                 inv_phi  = -2.3025855)

  par_list <- list(list(par_name  = "par_beta",
                        par_trans = "exp"),
                   list(par_name  = "par_rho",
                        par_trans = "expit"),
                   list(par_name  = "I0",
                          par_trans = "exp"),
                   list(par_name  = "phi",
                        par_trans = c("exp", "inv")))

 actual   <- sd_interpret_estimates(estimates, par_list)

 expected <- data.frame(par_beta = 1, par_rho = 0.7, I0 = 1, phi = 10)

 expect_equal(actual, expected, tol = 1e-4)


 estimates <- data.frame(c(0, 0.6931472),
                         c(0.8472979, 0.8472979),
                         c(0, 0.4054651),
                         c(-2.3025855, -2.3025855))

 actual   <- sd_interpret_estimates(estimates, par_list)

 expected <- data.frame(par_beta = c(1, 2),
                        par_rho = 0.7,
                        I0 = c(1, 1.5),
                        phi = 10)

 expect_equal(actual, expected, tol = 1e-4)
})

test_that("sd_conf_intervals returns the expected data frame", {

  estimates <- c(-0.2630303135, 1.5788579359)

  par_list  <- list(list(par_name  = "par_inv_R0",
                         par_trans = "expit"),
                    list(par_name  = "I0",
                         par_trans = "exp"))

  hsn    <- matrix(c(3513.105214, -493.5469626, -493.5469626, 88.48712899),
                   ncol = 2)

  actual <- sd_conf_intervals(estimates, par_list, hsn)

  expected <- data.frame(X1 = c("par_inv_R0", "I0"),
                         X2 = c(0.4172404, 3.0986791),
                         X3 = c(0.4521597, 7.5893045))

  names(expected) <- c("parameter", "2.5%", "97.5%")

  expect_equal(actual, expected)
})
