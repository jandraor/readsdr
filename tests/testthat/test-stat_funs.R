test_that("translate_NORMAL returns the expected string", {
  expect_equal(translate_NORMAL("NORMAL(1,10)", "isee"),
               "rnorm(1,1,10)")

  expect_equal(translate_NORMAL("x + NORMAL(1,10)", "isee"),
               "x + rnorm(1,1,10)")
})

test_that("translate_NORMAL throws an error when NORMAL has more than two params", {
  expect_error(translate_NORMAL("NORMAL(0,1,1)", "isee"),
               "readsdr is restricted to translate NORMAL functions with only two parameters: mean, std_dev.")
})
