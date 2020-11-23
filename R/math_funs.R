translate_math_funs <- function(equation) {
  new_equation <- translate_ABS(equation)
  new_equation <- translate_SQRT(new_equation)
  new_equation
}

translate_ABS <- function(equation) {
  pattern <- stringr::regex("\\bABS\\b", ignore_case = TRUE)
  stringr::str_replace_all(equation, pattern, "abs")
}

translate_SQRT <- function(equation) {
  pattern <- stringr::regex("\\bSQRT\\b", ignore_case = TRUE)
  stringr::str_replace_all(equation, pattern, "sqrt")
}
