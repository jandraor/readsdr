test_that("call_graph_funs extract graph funs and put them on the global
environment", {
  test_mdl <- list(
    description = list(
      variables = list(
        list(
          name = "test_var",
          equation = "fun_test(input_var)",
          graph_fun = list(
            name = "fun_test",
            fun  = function(x) x)))))

  call_graphical_funs(test_mdl)
  actual <- exists("fun_test") & class(fun_test) == "function"
  expect_equal(actual, TRUE)
})
