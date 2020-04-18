
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
    detection_pattern     <- "\\bIF\\b"
    there_is_if_statement <- stringr::str_detect(equation, detection_pattern)

    n_ifs <- stringr::str_count(equation, detection_pattern)

    if(n_ifs > 1) stop("Only one IF-ELSE statement per variable is permitted")

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
    pattern      <- stringr::regex("IF_THEN_ELSE", ignore_case = TRUE)
    n_ifs        <- stringr::str_count(equation, pattern)

    if(n_ifs > 1) stop("Only one IF-ELSE statement per variable is permitted")

    equation     <- stringr::str_replace(equation, pattern, "ifelse")
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
    equation     <- new_equation
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
    n_pt        <- stringr::str_count(equation, pattern_pt)

    if(n_pt > 1) stop("Only one PULSE_TRAIN statement per variable is permitted")

    match_result <- stringr::str_match(equation, pattern_pt)
    start_pt     <- match_result[[2]]
    duration_pt  <- match_result[[3]]
    repeat_pt    <- match_result[[4]]
    end_pt       <- match_result[[5]]

    translation <- stringr::str_glue(
        "sd_pulse_train(time, {start_pt},{duration_pt},{repeat_pt},{end_pt})")

    new_equation <- stringr::str_replace(equation, pattern_pt, translation)
    return(new_equation)
  }

  equation
}

# Translate Pulse

translate_pulse <- function(equation, vendor) {

  # Screening
  pattern_screen  <- stringr::regex("PULSE\\(.+?\\)",
                                    dotall = TRUE, ignore_case = TRUE)

  n_pulses        <- stringr::str_count(equation, pattern_screen)

  if(n_pulses > 1) stop("Only one PULSE statement per variable is permitted")

  if(vendor == "Vensim") {
    pattern_pulse  <- stringr::regex("PULSE\\((.+?),(.+?)\\)",
                                     dotall = TRUE, ignore_case = TRUE)
    there_is_pulse <- stringr::str_detect(equation, pattern_pulse)

    if(there_is_pulse) {
      string_match <- stringr::str_match(equation, pattern_pulse)
      pulse_start  <- string_match[[2]]
      pulse_width  <- string_match[[3]]
      start_num    <- suppressWarnings(as.numeric(pulse_start))
      width_num    <- suppressWarnings(as.numeric(pulse_width))

      if(is.na(start_num) | is.na(width_num)) {
        replacement  <- stringr::str_glue(
          "sd_pulse_v(time,{pulse_start},{pulse_width})"
        )
        new_equation <- stringr::str_replace(equation, pattern_pulse,
                                             replacement)
        return(new_equation)
      }

      replacement  <- get_pulse_v_statement(start_num, width_num)
      new_equation <- stringr::str_replace(equation, pattern_pulse,
                                           replacement)
      return(new_equation)
    }
  }

  if(vendor == "isee") {

    # Pattern 1 is a PULSE with three args
    # It is a pulse train
    pattern1 <- stringr::regex("PULSE\\((.+),(.+),(.+)\\)",
                               dotall = TRUE, ignore_case = TRUE)
    there_is_p1 <- stringr::str_detect(equation, pattern1)

    if(there_is_p1) {
      string_match <- stringr::str_match(equation, pattern1)
      volume_p     <- string_match[[2]] # volume pulse
      start_p      <- string_match[[3]] # start pulse
      interval     <- string_match[[4]]

      interval_num <- suppressWarnings(as.numeric(interval))

      if(is.na(interval_num)) {
        replacement <- stringr::str_glue(
          "sd_pulse_s(time, {volume_p},{start_p},{interval})"
        )
        new_equation <- stringr::str_replace(equation, pattern1,
                                             replacement)
        return(new_equation)
      }

      replacement  <- get_pulse_s_statement(volume_p, start_p, interval_num)
      new_equation <- stringr::str_replace(equation, pattern1, replacement)
      return(new_equation)
    }

    # Pattern 2 is a PULSE with two args
    # It is a magnified step

    pattern2 <- stringr::regex("PULSE\\((.+),(.+)\\)",
                               dotall = TRUE, ignore_case = TRUE)

    there_is_p2 <- stringr::str_detect(equation, pattern2)

    if(there_is_p2) {
      string_match <- stringr::str_match(equation, pattern2)
      volume_p     <- string_match[[2]] # volume pulse
      start_p      <- string_match[[3]] # start pulse
      replacement <- stringr::str_glue(
        "ifelse(time >= {start_p}, {volume_p} / timestep(), 0)")
      new_equation <- stringr::str_replace(equation, pattern2, replacement)
      return(new_equation)
    }

    # Pattern 3 is a PULSE with one arg
    # It magnifies the variable

    pattern3 <- stringr::regex("PULSE\\((.+)\\)",
                               dotall = TRUE, ignore_case = TRUE)

    there_is_p3 <- stringr::str_detect(equation, pattern3)

    if(there_is_p3) {
      string_match <- stringr::str_match(equation, pattern3)
      volume_p     <- string_match[[2]] # volume pulse

      replacement  <- stringr::str_glue("{volume_p} / timestep()")
      new_equation <- stringr::str_replace(equation, pattern3, replacement)
      return(new_equation)
    }
  }

  equation
}

get_pulse_s_statement <- function(volume_p, start_p, interval_num) {

  if(interval_num == 0L) {
    statement <- stringr::str_glue(
      "ifelse(time =={start_p}, {volume_p} / timestep(), 0)")
    return(statement)
  }

  if(interval_num > 0) {
    pulse_points <- stringr::str_glue(
      "seq({start_p}, max(time, {start_p}), {interval_num})")
    statement <- stringr::str_glue(
      "ifelse(time %in% {pulse_points}, {volume_p} / timestep(), 0)")
  }
}

get_pulse_v_statement <- function(pulse_start, pulse_width) {
  end_pulse <- pulse_start  + pulse_width
  if_true   <- paste0('== ', pulse_start)
  if_false  <- stringr::str_glue(">= {pulse_start} & time < {end_pulse}")
  condition <- ifelse(pulse_width == 0L, if_true, if_false)
  statement <- stringr::str_glue("ifelse(time {condition}, 1, 0)")
}
