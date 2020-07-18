mdl <- list(description = list(
  constants = list(
    list(name = "birth_rate",
         value = 0.1),
    list(name = "death_rate",
         value = 0.1)
  ),
  levels = list(
    list(name = "Population",
         initValue = 100),
    list(name = "Deaths",
         initValue = 0)
  )))

test_that("sd_constants() returns the expected data frame", {
  expect_is(sd_constants(mdl), "data.frame")
  expect_equal(sd_constants(mdl),
               data.frame(name = c("birth_rate", "death_rate"),
                          value = c(0.1,0.1)))
})

test_that("sd_stocks() returns the expected data frame", {
  expect_is(sd_stocks(mdl), "data.frame")
  expect_equal(sd_stocks(mdl),
               data.frame(name       = c("Population", "Deaths"),
                          init_value = c(100, 0)))
})
