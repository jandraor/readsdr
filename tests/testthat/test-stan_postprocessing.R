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
