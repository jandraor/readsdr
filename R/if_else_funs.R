translate_ifelse <- function(equation) {

  ifelse_Stella <- stringr::str_detect(equation, "\\bIF\\b")

  if(ifelse_Stella) {
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

  ifelse_Vensim <- stringr::str_detect(equation, "IF_THEN_ELSE")

  if(ifelse_Vensim) {
    equation <- stringr::str_replace(equation, "IF_THEN_ELSE", "ifelse")
  }

  equation
}

translate_step <- function(equation) {
  pattern_step  <- stringr::regex("STEP\\((.+?),(.+?)\\)")
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
  pattern_pt  <- stringr::regex("PULSE_TRAIN\\((.+?),(.+?),(.+?),(.+?)\\)")
  # is there a pulse train?
  there_is_pt <- stringr::str_detect(equation, pattern_pt)

  if(there_is_pt) {
    pt_condition <- create_pt_condition(equation, pattern_pt)
    translation  <- stringr::str_glue("ifelse({pt_condition}, 1, 0)")
    new_equation <- stringr::str_replace(equation, pattern_pt, translation)

    equation     <- new_equation
  }

  equation
}

# Create condition for pulse train
create_pt_condition <- function(equation, pattern_pt) {
  match_result <- stringr::str_match(equation, pattern_pt)
  start_pt     <- as.numeric(match_result[[2]])
  duration_pt  <- as.numeric(match_result[[3]])
  repeat_pt    <- as.numeric(match_result[[4]])
  end_pt       <- as.numeric(match_result[[5]])

  intervals_start      <- seq(from = start_pt, to = end_pt, by = repeat_pt)
  intervals_should_end <- intervals_start + duration_pt
  intervals_actual_end <- ifelse(intervals_should_end > end_pt, end_pt,
                                 intervals_should_end)

  comparison_end_intv  <- mapply(c, intervals_should_end, intervals_actual_end,
                                 SIMPLIFY = FALSE, USE.NAMES = FALSE)

  conditions <- purrr::map2_chr(intervals_start, comparison_end_intv, ~ {
    operator     <- ifelse(.y[[1]] == .y[[2]], "<", "<=")

    stringr::str_glue("(time >= {.x} & time {operator} {.y[[2]]})")
  })

  paste(conditions, collapse = " | ")
}
