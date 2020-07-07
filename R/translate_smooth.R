translate_SMOOTH1 <- function(name, equation, vendor) {

  pattern      <- stringr::regex("SMTH1\\((.+),(.+),(.+)\\)",
                                 dotall = TRUE)
  string_match <- stringr::str_match(equation, pattern)
  goal         <- trimws(string_match[[2]])
  delay        <- trimws(string_match[[3]])
  init         <- trimws(string_match[[4]])

  equation     <- stringr::str_glue("({goal}-{name})/{delay}")
  flow_name    <- stringr::str_glue("adjust_{name}")

  list(variable = list(name     = as.character(flow_name),
                       equation = as.character(equation)),
       stock    = list(name      = name,
                       equation  = as.character(flow_name),
                       initValue = as.numeric(init)))
}

translate_SMOOTH3 <- function(name, equation, vendor) {
  pattern      <- stringr::regex("SMTH3\\((.+),(.+),(.+)\\)",
                                 dotall = TRUE)

  string_match <- stringr::str_match(equation, pattern)
  goal         <- trimws(string_match[[2]])
  total_delay  <- trimws(string_match[[3]])
  total_delay  <- as.numeric(total_delay)
  init         <- trimws(string_match[[4]])

  variable_list <- vector(mode = "list", length = 3)
  stock_list    <- vector(mode = "list", length = 3)

  delay        <- total_delay / 3


  for(i in 1:3) {
    var_name     <- paste0("adjust_", name)
    stock_name   <- name

    if(i == 1) {
      var_equation <- stringr::str_glue("({name}_2-{name})/{delay}")
    }

    if(i == 2) {
      var_name     <- paste0(var_name, "_", i)
      var_equation <- stringr::str_glue("({name}_3-{name}_2)/{delay}")

      stock_name     <- paste0(stock_name, "_", i)

    }

    if(i == 3) {
      var_name     <- paste0(var_name, "_", i)
      var_equation <- stringr::str_glue("({goal}-{name}_3)/{delay}")

      stock_name     <- paste0(stock_name, "_", i)
    }

    variable_list[[i]] <- list(name     = var_name,
                               equation = as.character(var_equation))

    stock_list[[i]]    <- list(name      = stock_name,
                               equation  = var_name,
                               initValue = as.numeric(init))
  }

  list(variable_list = variable_list,
       stock_list = stock_list)
}

