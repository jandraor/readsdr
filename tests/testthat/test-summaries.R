mdl <- list(description = list(
  constants = list(
    list(name = "birth_rate",
         value = 0.1),
    list(name = "death_rate",
         value = 0.1)
  )))

test_that("sd_constants() returns a data frame", {
  expect_is(sd_constants(mdl), "data.frame")
})

test_that("sd_constants() returns the expected data frame", {
  expect_equal(sd_constants(mdl),
               data.frame(name = c("birth_rate", "death_rate"),
                          value = c(0.1,0.1)))
})
