
translate_logical_operators <- function(equation) {
  equation %>% translate_AND() %>%
    translate_OR()
}

translate_AND <- function(equation) {
  equation %>% stringr::str_replace_all(":AND:", "&") %>%
    stringr::str_replace_all("AND(?=\\(.+\\))", "&")
}

translate_OR <- function(equation) {
  equation %>% stringr::str_replace_all(":OR:", "|")%>%
    stringr::str_replace_all("OR(?=\\(.+\\))", "|")
}
