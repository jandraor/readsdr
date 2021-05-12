test_that("sd_net_change() returns the expected data.frame", {
  test_output <- data.frame(time = seq(0, 2, by = 0.25),
                            C    = c(0, rep(5,4), rep(20, 4)))

  actual_df   <- sd_net_change(test_output, "C")
  expected_df <- data.frame(time = 1:2, value = c(5, 15), var = "delta_C")

  expect_equal(actual_df, expected_df)
})
