translate_math_funs <- function(equation) {
  translate_ABS(equation)
}

translate_ABS <- function(equation) {
  pattern <- stringr::regex("\\bABS\\b", ignore_case = TRUE)
  stringr::str_replace_all(equation, pattern, "abs")
}
