test_that("array_equations() returns the expected list", {

  dims_obj  <- list(global_dims = list(Age = c("1", "2")),
                    dictionary  = list(I = c("Age")))

  vendor    <- "isee"
  dim_names <- "Age"

  aux_obj <- list(name     = "I_to_R",
                  equation = "par_gamma*I")

  actual <- array_equations(aux_obj, dims_obj, dim_names, vendor)

  expected <- list(equations  = c("par_gamma*I_1", "par_gamma*I_2"),
                    are_const  = c(FALSE, FALSE),
                    elems      = c("1", "2"))

  expect_equal(actual, expected)

  dims_obj <- list(global_dims = list(Age = 1:2),
                   dictionary  = list(lambda = "Age",
                                      S      = "Age"))

  aux_obj <- list(name     = "S_to_I",
                  equation = "lambda*S")

  actual <- array_equations(aux_obj, dims_obj, dim_names, vendor)

  expected <- list(equations  = c("lambda_1*S_1", "lambda_2*S_2"),
                   are_const  = c(FALSE, FALSE),
                   elems      = c("1", "2"))

  expect_equal(actual, expected)
})

test_that("devectorise_equation() returns the expected output", {

  dims_list     <- list(Region = c("Westeros", "Essos"),
                       Age    = c("Young", "Old"))

  raw_equation <- "Population[Region,Age] * growth_rate[Region,Age]"
  actual       <- devectorise_equation(raw_equation, dims_list)

  expected <- c("Population_Westeros_Young * growth_rate_Westeros_Young",
                "Population_Westeros_Old * growth_rate_Westeros_Old",
                "Population_Essos_Young * growth_rate_Essos_Young",
                "Population_Essos_Old * growth_rate_Essos_Old")

  expect_equal(actual, expected)
})
