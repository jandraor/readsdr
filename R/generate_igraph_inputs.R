generate_nodes_df <- function(stocks, variables) {
  stocks_df <- purrr::map_df(stocks, function(stock) {

    data.frame(name = stock$name,
               type = "stock",
               equation = stock$equation,
               stringsAsFactors = F)
  })

  variables_df <- purrr::map_df(variables, function(variable) {
    data.frame(name = variable$name,
               type = "variable",
               equation = variable$equation,
               stringsAsFactors = F)
  })

  dplyr::bind_rows(stocks_df, variables_df)
}

generate_edges_df <- function(stocks, variables, constants) {

  stocks_edges <- purrr::map_df(stocks, function(stock) {
    rhs <- stringr::str_split(stock$equation, pattern = "\\+|-|\\*|/")[[1]]
    rhs <- rhs[rhs != ""]
    data.frame(from = rhs,
               to = rep(stock$name, length(rhs)),
               type = "flow",
               stringsAsFactors = F)
  })

  const_names <- sapply(constants, function(constant) constant$name)

  variables_edges <- purrr::map_df(
    variables, const_names = const_names,
    function(variable, const_names) {
      raw_elements   <- stringr::str_split(variable$equation, "\\b")[[1]] %>%
        stringi::stri_remove_empty()

      boolean_filter <- stringr::str_detect(raw_elements, "/|\\*|\\+|-|\\(|\\)")
      rhs            <- raw_elements[!boolean_filter]
      rhs            <- rhs[!rhs %in% const_names ]

      if(length(rhs) == 0L) return(NULL)

      data.frame(from = rhs, to = rep(variable$name, length(rhs)),
                 type = "info_link", stringsAsFactors = F)})

  dplyr::bind_rows(stocks_edges, variables_edges)
}
