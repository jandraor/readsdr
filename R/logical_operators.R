
translate_logical_operators <- function(equation, vendor) {
  equation %>%
    translate_AND(vendor) %>%
    translate_OR(vendor) %>%
    translate_NOT(vendor)
}

translate_AND <- function(equation, vendor) {
  if(vendor == "Vensim") {
    equation <- stringr::str_replace_all(equation, ":AND:", "&")
  }

  if(vendor == "isee") {
    pattern  <- stringr::regex("AND(?=\\(.+\\))", ignore_case = TRUE)
    equation <- stringr::str_replace_all(equation, pattern, "&")
  }

  equation
}

translate_OR <- function(equation, vendor) {
  if(vendor == "Vensim") {
    equation <- stringr::str_replace_all(equation, ":OR:", "|")
  }

  if(vendor == "isee") {
    pattern  <- stringr::regex("OR(?=\\(.+\\))", ignore_case = TRUE)
    equation <- stringr::str_replace_all(equation , pattern, "|")
  }

  equation
}

translate_NOT <- function(equation, vendor) {

  if(vendor == "Vensim") {
    equation <- stringr::str_replace_all(equation, ":NOT:", "!")
  }

  if(vendor == "isee") {
    pattern  <- stringr::regex("NOT(?=\\(.+\\))", ignore_case = TRUE)
    equation <- stringr::str_replace_all(equation , pattern, "!")
  }
  equation
}
