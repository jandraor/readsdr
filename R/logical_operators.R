
translate_logical_operators <- function(equation, vendor) {
  equation %>% translate_AND(vendor) %>%
    translate_OR(vendor)
}

translate_AND <- function(equation, vendor) {
  if(vendor == "Vensim") {
    equation <- equation %>% stringr::str_replace_all(":AND:", "&")
  }

  if(vendor == "isee") {
    equation <- equation %>% stringr::str_replace_all("AND(?=\\(.+\\))", "&")
  }

  equation
}

translate_OR <- function(equation, vendor) {
  if(vendor == "Vensim") {
    equation <- stringr::str_replace_all(equation, ":OR:", "|")
  }

  if(vendor == "isee") {
    equation <- stringr::str_replace_all(equation , "OR(?=\\(.+\\))", "|")
  }

  equation
}
