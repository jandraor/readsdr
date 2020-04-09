
translate_if_else_functions <- function(equation, vendor) {
  translated_equation <- translate_ifelse(equation, vendor) %>%
    translate_step() %>%
    translate_pulse(vendor)

  if(vendor == "Vensim") {
    translated_equation <- translate_pulse_train(translated_equation)
  }

  translated_equation
}

translate_ifelse <- function(equation, vendor) {

  if(vendor == "isee") {
    there_is_if_statement <- stringr::str_detect(equation, "\\bIF\\b")

    if(there_is_if_statement) {
      pattern      <- stringr::regex("IF\\((.+)\\).*THEN(.*)ELSE(.*)",
                                     dotall = TRUE)
      string_match <- stringr::str_match(equation, pattern)
      condition    <- string_match[[2]]
      if_true      <- string_match[[3]]
      if_false     <- string_match[[4]]
      body_ifelse  <- paste(condition, if_true, if_false, sep = ", ")
      equation     <- paste0("ifelse(", body_ifelse, ")")
      return(equation)
    }

  }

  if(vendor == "Vensim") {
    equation <- stringr::str_replace(equation, "IF_THEN_ELSE", "ifelse")
  }

  equation
}

translate_step <- function(equation) {
  pattern_step  <- stringr::regex("STEP\\((.+?),(.+?)\\)",
                                  ignore_case = TRUE, dotall = TRUE)
  there_is_step <- stringr::str_detect(equation, pattern_step)

  if(there_is_step) {
    new_equation <- stringr::str_replace(equation, pattern_step,
                                         "ifelse(time >=\\2, \\1, 0)")

    new_equation <- translate_step(new_equation)
    equation <- new_equation
  }

  equation
}

translate_pulse_train <- function(equation) {
  # pattern pulse train
  pattern_pt  <- stringr::regex("PULSE_TRAIN\\((.+?),(.+?),(.+?),(.+?)\\)",
                                ignore_case = TRUE, dotall = TRUE)
  # is there a pulse train?
  there_is_pt <- stringr::str_detect(equation, pattern_pt)

  if(there_is_pt) {
    match_result <- stringr::str_match(equation, pattern_pt)
    start_pt    <- match_result[[2]]
    duration_pt <- match_result[[3]]
    repeat_pt   <- match_result[[4]]
    end_pt      <- match_result[[5]]


    translation <- stringr::str_glue(
        "sd_pulse_train(time, {start_pt},{duration_pt},{repeat_pt},{end_pt})")

    new_equation <- stringr::str_replace(equation, pattern_pt, translation)
    return(new_equation)
  }

  equation
}

# Translate Pulse

translate_pulse <- function(equation, vendor) {

  if(vendor == "Vensim") {
    pattern_pulse  <- stringr::regex("(.*?)PULSE\\((.+?),(.+?)\\)(.*?)",
                                     dotall = TRUE)
    there_is_pulse <- stringr::str_detect(equation, pattern_pulse)

    if(there_is_pulse) {
      string_match <- stringr::str_match(equation, pattern_pulse)
      text_before  <- string_match[[2]]
      text_after   <- string_match[[5]]
      start_pulse  <- as.numeric(string_match[[3]])
      width_pulse  <- as.numeric(string_match[[4]])
      end_pulse    <- start_pulse + width_pulse
      if_true      <- paste0('== ', start_pulse)
      if_false     <- stringr::str_glue(">= {start_pulse} & time < {end_pulse}")
      condition    <- ifelse(width_pulse == 0L, if_true, if_false)
      return(stringr::str_glue("{text_before}ifelse(time {condition}, 1, 0){text_after}"))
    }
  }

  if(vendor == "isee") {
    pattern1 <- stringr::regex("PULSE\\((.+),(.+),(.+)\\)",
                               dotall = TRUE, ignore_case = TRUE)
    there_is_p1 <- stringr::str_detect(equation, pattern1)

    if(there_is_p1) {
      string_match <- stringr::str_match(equation, pattern1)
      volume_p     <- string_match[[2]] # volume pulse
      start_p      <- string_match[[3]] # start pulse
      interval     <- string_match[[4]]

      evaluated_interval <- eval_constant_expr(interval)

      if(evaluated_interval == 0L) {
        replacement <- stringr::str_glue(
          "ifelse(time =={start_p }, {volume_p} / timestep(), 0)")

        new_equation <- stringr::str_replace(equation, pattern1, replacement)
        return(new_equation)
      }

      if(evaluated_interval > 0) {

        pulse_points <- stringr::str_glue(
          "seq({start_p}, max(time, {start_p}), {evaluated_interval})")
        replacement <- stringr::str_glue(
          "ifelse(time %in% {pulse_points}, {volume_p} / timestep(), 0)")
        new_equation <- stringr::str_replace(equation, pattern1, replacement)
        return(new_equation)
      }
    }
  }
  equation
}
