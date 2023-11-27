translate_graph_func <- function(gf_xml) {

  ypts_xml <- gf_xml |> xml2::xml_find_first(".//d1:ypts")

  ypts <- xml2::xml_text(ypts_xml) |>
    stringr::str_split(",", simplify = TRUE) |> as.vector() |> as.numeric()

  length_y <- length(ypts)

  xpts_xml <- gf_xml |> xml2::xml_find_first(".//d1:xpts")

  if(length(xpts_xml) > 0) {

    x_points <- xml2::xml_text(xpts_xml) |>
      stringr::str_split(",", simplify = TRUE) |> as.vector() |> as.numeric()
  } else{

    xscale_xml <- gf_xml |> xml2::xml_find_first(".//d1:xscale")

    x_min <- xml2::xml_attr(xscale_xml, "min") |>  as.numeric()
    x_max <- xml2::xml_attr(xscale_xml, "max") |>  as.numeric()

    x_points <- seq(x_min, x_max, length.out = length_y)
  }

  graph_fun <- stats::approxfun(
    x = x_points,
    y = ypts,
    method = "linear",
    yleft  = ypts[[1]],
    yright = ypts[[length_y]])
}

translate_Vensim_graph_func <- function(equation){
  match_output <- stringr::str_match(
    equation, "WITHLOOKUP\\((\\w+),\\(\\[.+\\],(.+)\\)\\)")

  match_data_points <- match_output[[3]] %>%
    stringr::str_match_all("\\((.+?),(.+?)\\),")

  x_points <- match_data_points[[1]][, 2] %>% as.numeric()
  y_points <- match_data_points[[1]][, 3] %>% as.numeric()

  graph_fun <- stats::approxfun(
    x = x_points,
    y = y_points,
    method = "linear",
    yleft  = y_points[[1]],
    yright = y_points[[length(y_points)]])

  list(input      = match_output[[2]],
       graph_fun  = graph_fun)
}
