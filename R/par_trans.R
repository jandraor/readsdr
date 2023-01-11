#' Expit transformation
#'
#' @param x A real number
#'
#' @return A number in the range 0 to 1
#' @export
#'
#' @examples
#' expit(-3)
expit <- function(x) 1 / (1 + exp(-x))


#' Logit transformation
#'
#' @param p A real number that represents a probability
#'
#' @return An unconstrained real number
#' @export
#'
#' @examples
#' logit(0.5)
logit <- function(p) log(p / (1 - p))


#' Inverse of a number
#'
#' @param x A real number
#'
#' @return A real number
#' @export
#'
#' @examples
#' inv(0.5) # Should return 2
inv <- function(x) 1/x
