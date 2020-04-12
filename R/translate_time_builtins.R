translate_time_builtins <- function(equation) {
  new_equation <- equation %>% translate_time() %>%
    translate_dt()
}


translate_time <- function(equation) {
  new_equation <- stringr::str_replace_all(equation, "\\bTime\\b|\\bTIME\\b",
                                           "time")
}

translate_dt <- function(equation) {
  new_equation <- stringr::str_replace_all(equation, "\\bDT\\b", "timestep()")
}
