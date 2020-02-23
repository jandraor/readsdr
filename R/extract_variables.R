extract_variables <- function(rhs) {
  raw_elements <- stringr::str_split(rhs, "\\b")[[1]] %>%
    stringi::stri_remove_empty()

  # Elements that start with alphabetical characters
  elems_alpha <- raw_elements[stringr::str_detect(raw_elements, "[:alpha:]+")]

  # Filtering out functions min & max
  detected_vars <- stringr::str_remove_all(elems_alpha, "\\bmin\\b|\\bmax\\b")
  detected_vars <- detected_vars[detected_vars != ""]
}
