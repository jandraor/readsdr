
#' PULSE TRAIN
#'
#' @param time A numeric argument that indicates the current simulation time
#' @param start_pulse A numeric argument that indicates the start of the pulse
#' @param duration_pulse A numeric argument that indicates the width of the pulse
#' @param repeat_pt A numeric argument that indicates the repetition pattern
#' @param end_pulse A numeric argument that indicates the end of the sequence
#'
#' @return 1 during the pulse, 0 otherwise.
#' @export
#'
#' @examples
#' sd_pulse_train(5, 5, 3, 10, 20)
sd_pulse_train <- function(time, start_pulse, duration_pulse,
                           repeat_pt, end_pulse) {

  if(time < start_pulse | time > end_pulse) return (0)

  start_candidates <- seq(start_pulse, time, repeat_pt)
  pos              <- findInterval(time, start_candidates)
  optim_start      <- start_candidates[pos] # Avoids unnecessary previous intervals

  optim_end <- min(time, end_pulse) # Avoids unnecessary forward intervals
  pt_statement <- create_pt_statement(optim_start, duration_pulse,
                                     repeat_pt, optim_end)

  eval(parse(text = pt_statement))
}

#' Create Pulse Train statement
#'
#' @param start_pt Numeric
#' @param duration_pt Numeric
#' @param repeat_pt Numeric
#' @param end_pt Numeric
#'
#' @return A string
#'
#' @noRd
create_pt_statement <- function(start_pt, duration_pt, repeat_pt, end_pt) {

  if(duration_pt == 0L) {

    pulse_points <- stringr::str_glue("seq({start_pt}, {end_pt}, {repeat_pt})")
    pt_statement <- stringr::str_glue("ifelse(time %in% {pulse_points}, 1, 0)")
    return(pt_statement)
  }

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

  pt_condition <- paste(conditions, collapse = " | ")
  stringr::str_glue("ifelse({pt_condition}, 1, 0)")
}


#' Replicate the behaviour of the PULSE function from Stella
#'
#' This function must be placed inside the object that will be passed as the
#' argument \code{func} to deSolve's \code{ode} function.
#'
#' @param time A number
#' @param volume A number
#' @param start_p A number
#' @param interval A number
#'
#' @return A number
#' @export
#'
#' @examples
#' timestep <- function() 0.25 # replicates timestep() from deSolve
#' sd_pulse_s(2, 1, 2, 0)
#'
sd_pulse_s <- function(time, volume, start_p, interval) {
  pulse_s_statement <- get_pulse_s_statement(volume, start_p, interval)
  eval(parse(text = pulse_s_statement))
}

#' Replicate the behaviour of the PULSE function from Vensim
#'
#' @param time A number
#' @param startPulse A number
#' @param duration A number
#'
#' @return A number
#' @export
#'
#' @examples
#' timestep <- function() 0.25 # replicates timestep() from deSolve
#' sd_pulse_v(1, 1, 2)
sd_pulse_v <- function(time, startPulse, duration) {
  pulse_v_statement <- get_pulse_v_statement(startPulse, duration)
  eval(parse(text = pulse_v_statement))
}
