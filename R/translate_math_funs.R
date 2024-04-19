translate_math_funs <- function(equation) {

  new_equation <- translate_ABS(equation)
  new_equation <- translate_SQRT(new_equation)
  new_equation <- translate_EXP(new_equation)
  new_equation
}

translate_ABS <- function(equation) {

  fun_name <- "ABS"
  downcase_fun_name(fun_name, equation)
}

translate_SQRT <- function(equation) {

  fun_name <- "SQRT"
  downcase_fun_name(fun_name, equation)
}

translate_EXP <- function(equation) {

  fun_name <- "EXP"
  downcase_fun_name(fun_name, equation)
}

downcase_fun_name <- function(fun_name, equation) {

  pattern <- paste0("\\b", fun_name, "\\b")
  rgx     <- stringr::regex(pattern, ignore_case = TRUE)
  stringr::str_replace_all(equation, rgx, tolower(fun_name))
}
