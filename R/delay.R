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
