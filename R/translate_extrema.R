translate_extrema <- function(equation) {
  translate_MIN(equation) %>%
    translate_MAX()
}

translate_MIN <- function(equation) {
  pattern <- stringr::regex("\\bMIN\\b", ignore_case = TRUE)
  stringr::str_replace_all(equation, pattern, "min")
}

translate_MAX <- function(equation) {
  pattern <- stringr::regex("\\bMAX\\b", ignore_case = TRUE)
  stringr::str_replace_all(equation, pattern, "max")
}
