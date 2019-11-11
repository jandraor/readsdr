extract_variables <- function(rhs) {
  raw_elements   <- stringr::str_split(rhs, "\\b")[[1]] %>%
    stringi::stri_remove_empty()

  boolean_filter <- stringr::str_detect(raw_elements, "/|\\*|\\+|-|\\(|\\)")
  detected_vars  <- raw_elements[!boolean_filter]
}
