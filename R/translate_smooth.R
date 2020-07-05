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

