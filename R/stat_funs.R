translate_stat_funs <- function(equation, vendor) {
  translate_NORMAL(equation, vendor)
}

translate_NORMAL <- function(equation, vendor) {
  new_equation <- equation

  if(vendor == "isee") {
    detection_pattern <- "\\bNORMAL\\b"
    pattern_found     <- stringr::str_detect(equation, detection_pattern)

    if(pattern_found) {
      pattern_normal <- stringr::regex("NORMAL\\((.+?),(.+?)\\)",
                                       dotall = TRUE, ignore_case = TRUE)

      string_match <- stringr::str_match(equation, pattern_normal)
      norm_mean    <- string_match[[2]]
      norm_sd      <- string_match[[3]]
      replacement  <- stringr::str_glue("rnorm(1,{norm_mean},{norm_sd})")

      new_equation <- stringr::str_replace(equation, pattern_normal,
                                           replacement)
    }
  }

  new_equation
}

