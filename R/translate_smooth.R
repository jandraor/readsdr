translate_SMOOTH1 <- function(name, equation, vendor) {

  if(vendor == "isee") {
    pattern      <- stringr::regex("SMTH1\\((.+),(.+),(.+)\\)",
                                   dotall = TRUE)
    string_match <- stringr::str_match(equation, pattern)
    goal         <- trimws(string_match[[2]])
    delay        <- trimws(string_match[[3]])
    init         <- trimws(string_match[[4]])

    return(stc_vars_S1(name, goal, delay, init))
  }

  if(vendor == "Vensim") {
    pattern      <- stringr::regex("SMOOTH\\((.+),(.+)\\)",
                                   dotall = TRUE)
    string_match <- stringr::str_match(equation, pattern)
    goal         <- trimws(string_match[[2]])
    delay        <- trimws(string_match[[3]])

    stc_vars_S1(name, goal, delay, goal)

  }
}

stc_vars_S1 <- function(name, goal, delay, init) {

   equation     <- stringr::str_glue("({goal}-{name})/{delay}")
   flow_name    <- stringr::str_glue("adjust_{name}")

   init2 <- suppressWarnings(as.numeric(init))

   if(is.na(init2)) init2 <- init

   list(variable = list(name     = as.character(flow_name),
                        equation = as.character(equation)),
        stock    = list(name      = name,
                        equation  = as.character(flow_name),
                        initValue = init2))
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

translate_SMOOTHN <- function(name, equation, vendor) {
  pattern      <- stringr::regex("SMTHN\\((.+),(.+),(.+),(.+)\\)",
                                 dotall = TRUE)

  string_match <- stringr::str_match(equation, pattern)

  goal         <- trimws(string_match[[2]])
  total_delay  <- trimws(string_match[[3]])
  total_delay  <- as.numeric(total_delay)
  delay_order  <- trimws(string_match[[4]])
  delay_order  <- suppressWarnings(as.numeric(delay_order))
  init         <- trimws(string_match[[5]])

  if(is.na(delay_order)) {
    msg <- paste("The delay order parameter, n, must be an integer in variable",
                  name)
    stop(msg, call. = FALSE)
  }

  if(delay_order == 1) {
    stc_var_list <- stc_vars_S1(name, goal, total_delay, init)

    return(list(variable_list = list(stc_var_list$variable),
                stock_list    = list(stc_var_list$stock),
                delay_order   = 1))
  }

  variable_list <- vector(mode = "list", length = delay_order)
  stock_list    <- vector(mode = "list", length = delay_order)

  delay         <- total_delay / delay_order

  for(i in 1:delay_order) {
    var_name     <- paste0("adjust_", name)
    stock_name   <- name

    if(i == 1) {
      var_equation <- stringr::str_glue("({name}_2-{name})/{delay}")
    }

    if(i > 1 & i < delay_order) {
      var_name     <- paste0(var_name, "_", i)
      var_equation <- stringr::str_glue("({name}_{i+1}-{name}_{i})/{delay}")

      stock_name     <- paste0(stock_name, "_", i)

    }

    if(i == delay_order) {
      var_name     <- paste0(var_name, "_", i)
      var_equation <- stringr::str_glue("({goal}-{name}_{i})/{delay}")

      stock_name     <- paste0(stock_name, "_", i)
    }

    variable_list[[i]] <- list(name     = var_name,
                               equation = as.character(var_equation))

    stock_list[[i]]    <- list(name      = stock_name,
                               equation  = var_name,
                               initValue = as.numeric(init))
  }

  list(variable_list = variable_list,
       stock_list    = stock_list,
       delay_order   = delay_order)
}

