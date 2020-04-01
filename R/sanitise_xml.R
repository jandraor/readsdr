sanitise_xml <- function(xml_as_text) {
  # pattern equations xml
  pattern_eq <- stringr::regex("<eqn>(.+?)</eqn>", dotall = TRUE)
  equations  <- stringr::str_match_all(xml_as_text, pattern_eq)[[1]][, 2]
  n_eq       <- length(equations)

  # Sanitised equations
  snt_equations <- equations %>% stringr::str_replace_all(">", "&gt;") %>%
    stringr::str_replace_all("<", "&lt;")
  sanitised_xml <- paste0("<eqn>", snt_equations, "</eqn>")

  pos_tags <- stringr::str_locate_all(xml_as_text, pattern_eq)[[1]]

  for(i in seq_len(nrow(pos_tags))) {
    pos_tags <- stringr::str_locate_all(xml_as_text, pattern_eq)[[1]]
    pos      <- pos_tags[i, ]
    stringr::str_sub(xml_as_text, pos[[1]], pos[[2]]) <- sanitised_xml[[i]]
  }

  xml_as_text
}
