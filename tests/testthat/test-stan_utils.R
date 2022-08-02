test_that("stan_transformed_data() returns the expected string", {
  expected_string <- paste(
    "transformed data {",
    "  real x_r[0];",
    "  int  x_i[0];",
    "}", sep = "\n")
  expect_equal(stan_transformed_data(), expected_string)
})

test_that("extract_timeseries_var() returns the expected data frame", {
  test_df <- data.frame(`var[1]` = rep(0, 2),
                        `var[2]` = rep(1, 2),
                        check.names = FALSE)

  expected_df <- data.frame(iter     = rep(1:2, 2),
                            time     = rep(1:2, each = 2),
                            variable = "var",
                            value    = c(0, 0, 1, 1))

  expect_equal(extract_timeseries_var("var", test_df), expected_df)

})

test_that("extract_timeseries_stock() returns the expected data frame", {
  test_df <- data.frame(`yhat[1,2]` = rep(0, 2),
                        `yhat[2,2]` = rep(1, 2),
                        check.names = FALSE)

  expected_df <- data.frame(iter     = rep(1:2, 2),
                            time     = rep(1:2, each = 2),
                            stock    = "S2",
                            value    = c(0, 0, 1, 1))

  test_stocks <- c("S1", "S2")

  expect_equal(extract_timeseries_stock("S2", test_df, test_stocks, "yhat"),
               expected_df)
})

test_that("stan_data() returns the expected string", {

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  int<lower = 1> n_params;",
    "  int<lower = 1> n_difeq;",
    "  array[n_obs] int y;",
    "  real t0;",
    "  array[n_obs] real ts;",
    "  vector[n_difeq] x0;",
    "}", sep = "\n")

  expect_equal(stan_data("y", "int"), expected_string)
})

test_that("stan_data() allows the user to remove y0", {

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  int<lower = 1> n_params;",
    "  int<lower = 1> n_difeq;",
    "  array[n_obs] int y;",
    "  real t0;",
    "  array[n_obs] real ts;",
    "}", sep = "\n")

  expect_equal(stan_data("y", "int", inits = FALSE), expected_string)
})

test_that("stan_data() allows the user to specify the var type", {

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  int<lower = 1> n_params;",
    "  int<lower = 1> n_difeq;",
    "  array[n_obs] real y;",
    "  real t0;",
    "  array[n_obs] real ts;",
    "}", sep = "\n")

  expect_equal(stan_data("y", "real", inits = FALSE), expected_string)
})

test_that("stan_data() returns an error when different sizes", {
  expect_error(stan_data(c("y1", "y2"), "real", inits = FALSE),
               "Different length sizes between 'vars_vector' & 'type' pars")
})

test_that("dist_type() returns the expected string", {

  dist_names <- c("poisson", "lognormal", "poisson", "neg_binomial_2",
                  "normal")

  actual     <- dist_type(dist_names)

  expected   <- c("int", "real", "int", "int", "real")

  expect_equal(actual, expected)
})

#------------------stan_params()------------------------------------------------

test_that("stan_params() returns the expected string", {

  p1 <- list(name = "par_beta",
             min  = 0)

  p2 <- list(name = "par_rho",
             min  = 0,
             max  = 1)

  s1 <- list(name = "I",
             min  = 0)

  m1 <- list(name      = "phi",
             par_trans = "inv",
             min       = 0)

  unk_list <- list(consts      = list(p1, p2),
                   stocks      = list(s1),
                   measurement = list(m1))

  actual <- stan_params(unk_list)

  expected <- paste(
    "parameters {",
    "  real<lower = 0> par_beta;",
    "  real<lower = 0, upper = 1> par_rho;",
    "  real<lower = 0> I0;",
    "  real<lower = 0> phi_inv;",
    "}", sep = "\n")

  expect_equal(actual, expected)
})
