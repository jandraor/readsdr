translate_SMOOTH1 <- function(name, equation, vendor,
                              fun = NA) {

  if(vendor == "isee") {

    pat1         <- "SMTH1\\((.+?),(.+?),(.+?)\\)"
    pat2         <- "SMTH1\\((.+?),(.+?)\\)"

    if(stringr::str_detect(equation, pat1)) {

      reg_pat      <- stringr::regex(pat1, dotall = TRUE)
      string_match <- stringr::str_match(equation, reg_pat)
      goal         <- trimws(string_match[[2]])
      delay        <- trimws(string_match[[3]])
      init         <- trimws(string_match[[4]])

      return(stc_vars_S1(name, goal, delay, init))
    }

    if(stringr::str_detect(equation, pat2)) {
      reg_pat      <- stringr::regex(pat2, dotall = TRUE)
      string_match <- stringr::str_match(equation, reg_pat)
      goal         <- trimws(string_match[[2]])
      delay        <- trimws(string_match[[3]])

      return(stc_vars_S1(name, goal, delay, goal))
    }
  }

  if(vendor == "Vensim") {

    if(fun == "SMOOTH") {
      pattern      <- stringr::regex("SMOOTH\\((.+),(.+)\\)",
                                     dotall = TRUE)
      string_match <- stringr::str_match(equation, pattern)
      goal         <- trimws(string_match[[2]])
      delay        <- trimws(string_match[[3]])

      return(stc_vars_S1(name, goal, delay, goal))
    }

    if(fun == "SMOOTHI") {
      pattern      <- stringr::regex("SMOOTHI\\((.+),(.+),(.+)\\)",
                                     dotall = TRUE)
      string_match <- stringr::str_match(equation, pattern)
      goal         <- trimws(string_match[[2]])
      delay        <- trimws(string_match[[3]])
      init         <- trimws(string_match[[4]])

      return(stc_vars_S1(name, goal, delay, init))
    }
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

translate_SMOOTH3 <- function(name, equation, vendor, fun = NA) {

  if(vendor == "isee") {

    pat1         <- "SMTH3\\((.+?),(.+?),(.+?)\\)"
    pat2         <- "SMTH3\\((.+?),(.+?)\\)"

    if(stringr::str_detect(equation, pat1)) {
      reg_pat      <- stringr::regex(pat1, dotall = TRUE)
      string_match <- stringr::str_match(equation, reg_pat)
      goal         <- trimws(string_match[[2]])
      total_delay  <- trimws(string_match[[3]])
      init         <- trimws(string_match[[4]])

      return(stc_vars_SN(name, goal, total_delay, 3, init))
    }

    if(stringr::str_detect(equation, pat2)) {
      reg_pat      <- stringr::regex(pat2, dotall = TRUE)
      string_match <- stringr::str_match(equation, reg_pat)
      goal         <- trimws(string_match[[2]])
      total_delay  <- trimws(string_match[[3]])

      return(stc_vars_SN(name, goal, total_delay, 3, goal))
    }
  }

  if(vendor == "Vensim") {

    if(fun == "SMOOTH3") {
      pattern      <- stringr::regex("SMOOTH3\\((.+),(.+)\\)",
                                     dotall = TRUE)

      string_match <- stringr::str_match(equation, pattern)
      goal         <- trimws(string_match[[2]])
      total_delay  <- trimws(string_match[[3]])

      return(stc_vars_SN(name, goal, total_delay, 3, goal))
    }

    if(fun == "SMOOTH3I") {
      pattern      <- stringr::regex("SMOOTH3I\\((.+),(.+),(.+)\\)",
                                     dotall = TRUE)

      string_match <- stringr::str_match(equation, pattern)
      goal         <- trimws(string_match[[2]])
      total_delay  <- trimws(string_match[[3]])
      init         <- trimws(string_match[[4]])

      return(stc_vars_SN(name, goal, total_delay, 3, init))
    }
  }
}

translate_SMOOTHN <- function(name, equation, vendor) {

  pat1         <- "SMTHN\\((.+),(.+),(.+),(.+)\\)"
  pat2         <- "SMTHN\\((.+),(.+),(.+)\\)"

  if(stringr::str_detect(equation, pat1)) {
    reg_pat      <- stringr::regex(pat1, dotall = TRUE)
    string_match <- stringr::str_match(equation, reg_pat)
    goal         <- trimws(string_match[[2]])
    total_delay  <- trimws(string_match[[3]])
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

    return(stc_vars_SN(name, goal, total_delay, delay_order, init))
  }

  if(stringr::str_detect(equation, pat2)) {
    reg_pat      <- stringr::regex(pat2, dotall = TRUE)
    string_match <- stringr::str_match(equation, reg_pat)
    goal         <- trimws(string_match[[2]])
    total_delay  <- trimws(string_match[[3]])
    delay_order  <- trimws(string_match[[4]])
    delay_order  <- suppressWarnings(as.numeric(delay_order))
    init         <- goal
  }

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

  return(stc_vars_SN(name, goal, total_delay, delay_order, init))
}


#' Generate stocks and variables for SMOOTH functions of order higher than 1
#'
#' @param name  The auxiliary's name in the SD software.
#' @param goal A number or a string.
#' @param total_delay A number
#' @param delay_order An integer
#' @param init A number or a string
#'
#' @return A list with two elements: stocks and variables. Each element is a
#' list of size equal to the delay order.
#'
#' @noRd

stc_vars_SN <- function(name, goal, total_delay, delay_order, init) {

  variable_list <- vector(mode = "list", length = delay_order)
  stock_list    <- vector(mode = "list", length = delay_order)

  total_delay2  <- suppressWarnings(as.numeric(total_delay))
  delay         <- total_delay2 / delay_order

  if(is.na(total_delay2)) delay <- stringr::str_glue("({total_delay}/{delay_order})")

  init2 <- suppressWarnings(as.numeric(init))

  if(is.na(init2)) init2 <- init

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
                               initValue = init2)
  }

  list(variable_list = variable_list,
       stock_list    = stock_list,
       delay_order   = delay_order)
}

