test_that("stan_model() returns the expected string", {

  m1      <- "y ~ normal(net_flow(C), tau)"
  meas_mdl <- list(m1)

  estimated_params <- list(
    sd_prior("par_beta", "lognormal", c(0, 1)),
    sd_prior("par_rho", "beta", c(2, 2)),
    sd_prior("I0", "lognormal", c(0, 1), "init"),
    list(par_name = "tau", dist = "exponential", beta = 0.1, min = 0,
         type = "meas_par"))

  lvl_names <- c("S", "E", "I", "R", "C")

  actual <- stan_model(estimated_params, meas_mdl, lvl_names)

  expected <- paste(
    "model {",
    "  par_beta ~ lognormal(0, 1);",
    "  par_rho ~ beta(2, 2);",
    "  I0 ~ lognormal(0, 1);",
    "  tau ~ exponential(0.1);",
    "  y ~ normal(delta_x_1, tau);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_model() returns the expected string for a vectorised model", {

  ag <- c("A", "B", "C", "D") # age_groups

  measurements <- stringr::str_glue("y_{ag} ~ poisson(net_flow(C_{ag}))")
  meas_mdl     <- as.list(measurements)

  estimated_params <- list(sd_prior("par_rho", "beta", c(2, 2)))

  filepath  <- system.file("models/", "SEIR_age.stmx", package = "readsdr")
  mdl       <- read_xmile(filepath)
  lvl_names <- sd_stocks(mdl)$name

  actual <- stan_model(estimated_params, meas_mdl, lvl_names)

  expected <- paste(
    "model {",
    "  par_rho ~ beta(2, 2);",
    "  y_A ~ poisson(delta_x_1);",
    "  y_B ~ poisson(delta_x_2);",
    "  y_C ~ poisson(delta_x_3);",
    "  y_D ~ poisson(delta_x_4);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_model() handles the lognormal distribution", {

  estimated_params  <- list(sd_prior("par_alpha", "normal",
                                     c(1, 0.5), min_0 = TRUE))

  lvl_names <- c("Hares", "Lynx")

  meas_mdl <- list("y1 ~ lognormal(Lynx, sigma_1)")
  actual   <- stan_model(estimated_params, meas_mdl, lvl_names)

  expected <- paste(
    "model {",
    "  par_alpha ~ normal(1, 0.5);",
    "  y1 ~ lognormal(x[:, 2], sigma_1);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

test_that("stan_model() handles measurements of init values", {

  estimated_params  <- list(
    list(
      par_name  = "sigma_1",
      dist      = "lognormal",
      mu        = -1,
      sigma     = 1,
      min       = 0,
      type      = "meas_par"),
    list(
      par_name = "H0",
      dist     = "lognormal",
      mu       = log(10),
      sigma    = 1,
      min      = 0,
      type     = "init"),
    list(
      par_name = "par_gamma",
      dist     = "normal",
      mu       = 1,
      sigma    = 0.5,
      type     = "constant",
      min      = 0))

  meas_mdl <- list("y ~ lognormal(Hares, sigma_1)",
                   "y0 ~ lognormal(log(Hares[0]), sigma_1)")

  lvl_names <- c("Hares", "Lynx")
  actual    <- stan_model(estimated_params, meas_mdl, lvl_names)

  expected <- paste(
    "model {",
    "  sigma_1 ~ lognormal(-1, 1);",
    "  H0 ~ lognormal(2.30258509299405, 1);",
    "  par_gamma ~ normal(1, 0.5);",
    "  y ~ lognormal(x[:, 1], sigma_1);",
    "  y0 ~ lognormal(log(x0[1]), sigma_1);",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

# Construct prior line----------------------------------------------------------
test_that("construct_prior_line() returns the expected string", {

  prior_obj <- list(par_name = "par_beta",
                    dist     = "lognormal",
                    mu       =  0,
                    sigma    =  1,
                    min      =  0,
                    type     =  "constant")

  actual   <- construct_prior_line(prior_obj)

  expected <- "  par_beta ~ lognormal(0, 1);"

  expect_equal(actual, expected)
})

test_that("construct_likelihood_line() returns the expected string", {

  meas_obj <- "y ~ neg_binomial_2(net_flow(C), phi)"

  actual   <- construct_likelihood_line(meas_obj, 1)

  expected <- list(line          = "  y ~ neg_binomial_2(delta_x_1, phi);",
                   delta_counter = 2)

  expect_equal(actual, expected)

  meas_obj <- "y ~ poisson(C)"

  lvl_names <- c("S", "E", "I", "R", "C")

  actual   <- construct_likelihood_line(meas_obj, 1, lvl_names)

  expected <- list(line          = "  y ~ poisson(x[:, 5]);",
                   delta_counter = 1)

  expect_equal(actual, expected)
})

test_that("construct_likelihood_line() handles log transformations", {

  meas_obj <- "y1 ~ lognormal(log(Lynx), sigma_1)"
  actual   <- construct_likelihood_line(meas_obj, 1, c("Hares", "Lynx"))

  expected <- list(line          = "  y1 ~ lognormal(log(x[:, 2]), sigma_1);",
                   delta_counter = 1)

  expect_equal(actual, expected)
})
