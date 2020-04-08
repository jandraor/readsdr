translate_comparison_operators <- function(equation) {
  equal_translated     <- translate_equal_sign(equation)
  not_equal_translated <- translate_not_equal_sign(equal_translated)
}

translate_equal_sign <- function(equation) {
  new_equation <- stringr::str_replace_all(equation, "(?<!<|>|=|!)=(?!=)", "==")
}

translate_not_equal_sign <- function(equation) {
  new_equation <- stringr::str_replace_all(equation, "<>", "!=")
}
