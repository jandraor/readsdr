translate_DELAYN <- function(name, eq, vendor, consts) {

  if(vendor == "isee") {

    pat1         <- "DELAYN\\((.+?),(.+?),(.+?),(.+?)\\)"

    if(stringr::str_detect(eq, pat1)) {

      reg_pat      <- stringr::regex(pat1, dotall = TRUE)
      string_match <- stringr::str_match(eq, reg_pat)
      input        <- trimws(string_match[[2]])

      raw_duration <- trimws(string_match[[3]])
      duration     <- suppressWarnings(as.numeric(raw_duration))

      if(is.na(duration)) {

        val_list        <- as.list(purrr::map_dbl(consts, "value"))
        c_names         <- get_names(consts)
        names(val_list) <- c_names
        duration        <- eval(parse(text = raw_duration), envir = val_list)
      }

      raw_delay_order <- trimws(string_match[[4]])
      delay_order     <- suppressWarnings(as.numeric(raw_delay_order))

      if(is.na(delay_order)) {

        const_names <- get_names(consts)
        idx         <- which(raw_delay_order == const_names)
        delay_order <- consts[[idx]]$value
      }

      raw_init <- trimws(string_match[[5]])
      init     <- suppressWarnings(as.numeric(raw_init))

      if(is.na(init)) {

        val_list        <- as.list(purrr::map_dbl(consts, "value"))
        c_names         <- get_names(consts)
        names(val_list) <- c_names
        init            <- eval(parse(text = raw_init), envir = val_list)
      }

      return(stc_vars_DELAYN(name, input, duration, delay_order, init, eq))
    }

  }

}

stc_vars_DELAYN <- function(name, input, duration, delay_order, init, eq) {

  variable_list <- vector(mode = "list", length = delay_order + 1)
  stock_list    <- vector(mode = "list", length = delay_order)

  v1_eq <- stringr::str_glue("dly_{input}_{delay_order}_out")

  variable_list[[1]] <- list(name     = name,
                             equation = as.character(v1_eq))

  for(i in seq_len(delay_order)) {

    var_name <- stringr::str_glue("dly_{input}_{i}_out")
    var_eq   <- stringr::str_glue("dly_{input}_{i}/(({duration})/{delay_order}.0)")

    variable_list[[i + 1]] <- list(name     = as.character(var_name),
                                   equation = as.character(var_eq))

    stk_name <- stringr::str_glue("dly_{input}_{i}")

    stk_init <- (init * duration) / delay_order

    if(i == 1) {

      stk_eq <- stringr::str_glue("{input} - {var_name}")
    }

    if(i != 1) {

      var_name2 <- stringr::str_glue("dly_{input}_{i - 1}_out")

      stk_eq <- stringr::str_glue("{var_name2} - {var_name}")
    }

    stock_list[[i]] <- list(name      = as.character(stk_name),
                            equation  = as.character(stk_eq),
                            initValue = stk_init)
  }

  list(variable_list = variable_list,
       stock_list    = stock_list,
       delay_order   = delay_order)
}

translate_delay <- function(equation, vendor) {

  if(vendor == "Vensim") {
    pattern <- ".+==DELAY_FIXED\\((.+?),(\\d+),(\\d+)\\)"

    reg_pat      <- stringr::regex(pattern)
    string_match <- stringr::str_match(equation, reg_pat)
    input        <- trimws(string_match[[2]])
    delay        <- trimws(string_match[[3]])
    init         <- trimws(string_match[[4]])

    new_eq <- stringr::str_glue(
      "sd_fixed_delay('{input}',time,{delay},{init},.memory)") %>%
      as.character()

    return(new_eq)
  }
}

identify_delayed_vars <- function(variables) {

  lapply(variables, function(var_obj) {
    equation <- var_obj$equation

    if(stringr::str_detect(equation, "sd_fixed_delay")) {

      pattern      <- "sd_fixed_delay\\('(.+)',.+\\)"
      reg_pat      <- stringr::regex(pattern)
      string_match <- stringr::str_match(equation, reg_pat)
      return(trimws(string_match[[2]]))
    }

    NULL
  }) %>% remove_NULL() %>% unlist()
}

#' Fixed delay
#'
#' @param var A string that indicates the delayed variable.
#' @param time A number that indicates current simulation time.
#' @param delay A number that indicates the delay time.
#' @param init A number that indicates the function's output value of at the
#' start of the simulation.
#' @param .memory A data frame that keeps past values of delayed variables.
#'
#' @return A number.
#' @export
#'
#' @examples
#' .memory <- data.frame(time = 3, inflow = 3)
#' rownames(.memory) <- 3
#' sd_fixed_delay("inflow", 5, 2, 0, .memory)
sd_fixed_delay <- function(var, time, delay, init, .memory) {

  val <- .memory[as.character(time - delay), c(var)]
  ifelse(is.finite(val) && is.numeric(val), val, init)
}
