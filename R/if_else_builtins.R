
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

  pt_statement <- create_pt_statement(start_pulse, duration_pulse,
                                     repeat_pt, end_pulse)

  eval(parse(text = pt_statement))
}
