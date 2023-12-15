
test_that("stan_data() returns the expected string", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  array[n_obs] int y;",
    "  array[n_obs] real ts;",
    "}", sep = "\n")

  expect_equal(stan_data(meas_mdl, TRUE, FALSE, NULL, NULL), expected_string)

})

test_that("stan_data() declares the vector for init values", {

  mm1      <- "y ~ neg_binomial_2(net_flow(C), phi)"
  meas_mdl <- list(mm1)

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  array[n_obs] int y;",
    "  array[n_obs] real ts;",
    "  vector[5] x0;",
    "}", sep = "\n")

  expect_equal(stan_data(meas_mdl, FALSE, FALSE, NULL, NULL, 5),
               expected_string)

  expected_string <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  array[n_obs] int y;",
    "  int y_ahead;",
    "  array[n_obs + 1] real ts;",
    "  vector[5] x0;",
    "}", sep = "\n")

  expect_equal(stan_data(meas_mdl, FALSE, TRUE, NULL, NULL, 5),
               expected_string)
})

test_that("stan_data() handles single measurements", {

  meas_mdl <- list("y ~ lognormal(log(Hares), sigma_1)",
                   "z ~ lognormal(log(Lynx), sigma_2)",
                   "y0 ~ lognormal(log(Hares[0]), sigma_1)")

  actual <- stan_data(meas_mdl, TRUE, FALSE, NULL, NULL)

  expected <- paste(
    "data {",
    "  int<lower = 1> n_obs;",
    "  array[n_obs] real y;",
    "  array[n_obs] real z;",
    "  real y0;",
    "  array[n_obs] real ts;",
    "}", sep = "\n")

  expect_equal(actual, expected)
})

# construct_data_decl()---------------------------------------------------------

test_that("construct_data_decl() returns the expected string", {

  meas_obj   <- "y ~ neg_binomial_2(net_flow(C), phi)"
  actual     <- construct_data_decl(meas_obj, FALSE)
  expected   <- "  array[n_obs] int y;"

  expect_equal(actual, expected)
})

test_that("construct_data_decl() returns the expected string", {
  meas_obj      <- "y ~ neg_binomial_2(net_flow(C), phi)"

  actual <- construct_data_decl(meas_obj, TRUE)

  expected <- paste("  array[n_obs] int y;",
                    "  int y_ahead;", sep = "\n")

  expect_equal(actual, expected)
})

test_that("construct_data_decl() handles single measurement", {

  meas_obj <- "y0 ~ lognormal(log(Hares[0]), sigma_1)"
  actual   <- construct_data_decl(meas_obj, FALSE)

  expected <- "  real y0;"

  expect_equal(actual, expected)
})
