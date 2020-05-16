#' Extract variables from an equation
#'
#' \code{extract_variables} returns the unique elements in an equation.
#'
#' This function assumes an expression in the canonical form \code{y = f(x)},
#' where x is a vector of n elements. \code{extract_variables} identify all
#' arguments in \code{f}. This is how it should be used:
#'
#' #' lhs <- "z"
#' rhs <- "x ^ 2 + 2 * x * y + y ^ 2"
#' extract_variables(lhs, rhs)
#'
#'
#' @param lhs A string with the name of the equation's subject, represented by
#'   \code{y} in the canonical form.
#' @param rhs The equation or expression for the equation's subject, represented
#' by \code{f(x)} in the canonical form.
#'
#' @return A character vector with the unique elements in the \code{rhs}
#' @noRd

extract_variables <- function(lhs, rhs) {
  raw_elements <- stringr::str_split(rhs, "\\b")[[1]] %>%
    stringi::stri_remove_empty()

  # Elements that start with alphabetical characters
  elems_alpha <- raw_elements[stringr::str_detect(raw_elements, "[:alpha:]+")]

  # Filtering out functions min & max
  detected_vars <- stringr::str_remove_all(elems_alpha, "\\bmin\\b|\\bmax\\b")
  detected_vars <- detected_vars[detected_vars != ""]

  # Filtering out graph functions
  potential_gf  <- paste0("f_", lhs)
  detected_vars <- detected_vars[detected_vars != potential_gf]

  # Filtering out ifelse
  detected_vars <- detected_vars[detected_vars != "ifelse"]

  unique(detected_vars)
}
