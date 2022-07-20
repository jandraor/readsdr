devectorise_equation <- function(raw_equation, dims_list) {

  dim_names   <- names(dims_list)
  pattern     <- paste0("\\[", paste(dim_names, collapse = ","), "\\]")

  elems       <- combine_dims(dims_list)
  replacement <- paste0("_", elems)

  stringr::str_replace_all(raw_equation, pattern, replacement)

}

combine_dims <- function(dims_list) {

  rev_dims_list <- rev(dims_list)
  rev_combs_df  <- expand.grid(rev_dims_list, stringsAsFactors = FALSE)
  combs_df      <- rev(rev_combs_df)
  do.call(paste, c(combs_df, sep = "_"))

}

create_array_pattern <- function(dims_list) {

  n_dims      <- length(dims_list)
  dim1_length <- length(dims_list[[1]])
  rgx_elems   <- rep(".+?", dim1_length)
  rgx_array   <- paste(rgx_elems, collapse = ",")

  if(n_dims == 2) {
    rgx_row    <- paste0(rgx_array, ";")
    rgx_matrix <- rep(rgx_row, length(dims_list[[2]]))
    rgx_array  <- paste(rgx_matrix, collapse = "")
  }

  rgx_array

}
