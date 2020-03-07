translate_Vensim_graph_func <- function(equation){
  match_output <- stringr::str_match(
    equation, "WITHLOOKUP\\((\\w+),\\(\\[.+\\],(.+)\\)\\)")
  
  match_data_points <- match_output[[3]] %>% 
    stringr::str_match_all("\\((.+?),(.+?)\\),")
  
  x_points <- match_data_points[[1]][, 2] %>% as.numeric()
  y_points <- match_data_points[[1]][, 3] %>% as.numeric()
  
  graph_fun <- approxfun(
      x = x_points,
      y = y_points,
      method = "linear",
      yleft  = y_points[[1]],
      yright = y_points[[length(y_points)]])
  
  list(input      = match_output[[2]],
       graph_fun  = graph_fun)
}