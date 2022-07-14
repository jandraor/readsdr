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
