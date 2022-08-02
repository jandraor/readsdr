test_that("sd_Bayes() returns the expected file", {

  filepath <- system.file("models/", "SEIR.stmx", package = "readsdr")

  mm1 <- list(measured_stock = "C",
              meas_type      = "net_change",
              meas_name      = "y",
              dist           = list(
                name       = "neg_binomial_2",
                stock_par  = "mu"))

  meas_mdl <- list(mm1)

  p1 <- list(name = "par_beta",
             min  = 0)

  p2 <- list(name = "rho",
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

  actual   <- sd_Bayes(filepath, meas_mdl, unk_list)

  fileName <- "SEIR_nbinom.stan"

  expected <- readChar(fileName, file.info(fileName)$size)

  expect_equal(actual, expected)

})
