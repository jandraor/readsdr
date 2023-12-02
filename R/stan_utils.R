# Stan's data block for ODE models
stan_data <- function(meas_mdl, unk_inits, LFO_CV, data_params, data_inits,
                      n_difeq = NULL) {

  external_params <- c(data_params, data_inits)

  decl <- "  int<lower = 1> n_obs;"

  data_decl <- lapply(meas_mdl, construct_data_decl, LFO_CV) %>%
    paste(collapse = "\n")

  final_decl <- ifelse(LFO_CV,
                       "  array[n_obs + 1] real ts;",
                       "  array[n_obs] real ts;")

  body_block <- paste(decl, data_decl, final_decl, sep = "\n")

  if(!unk_inits) {
    body_block <- paste(body_block,
                        stringr::str_glue("  vector[{n_difeq}] x0;"),
                        sep = "\n")
  }

  if(!is.null(external_params)) {

    data_params_lines <- stringr::str_glue("  real {external_params};") %>%
      paste(collapse = "\n")

    body_block <- paste(body_block, data_params_lines, sep = "\n")
  }

  paste("data {", body_block, "}", sep = "\n")
}

construct_data_decl <- function(meas_obj, LFO_CV) {

  split_strings <- strsplit(meas_obj, "~")[[1]]

  lhs <- split_strings[[1]] %>% stringr::str_trim()

  rhs  <- split_strings[[2]] %>% stringr::str_trim()
  type <- get_dist_type(rhs)

  inference_data_line <- stringr::str_glue("  array[n_obs] {type} {lhs};")

  if(!LFO_CV) return(inference_data_line)

  pred_data_line <- stringr::str_glue("  {type} {lhs}_ahead;")

  paste(inference_data_line, pred_data_line, sep = "\n")
}

get_dist_type <- function(rhs) {

  pattern       <- "(.+?)\\(.+\\)"
  string_match  <- stringr::str_match(rhs, pattern)
  dist_name     <- string_match[[2]]

  dist_db <- data.frame(id = c("neg_binomial",
                               "neg_binomial_2",
                               "poisson",
                               "normal",
                               "lognormal"),
                        type = c("int",
                                 "int",
                                 "int",
                                 "real",
                                 "real"))

  index <- match(dist_name, dist_db$id)

  dist_db[index, "type"]
}

get_dist_obj <- function(rhs, language = "Stan") {

  pattern       <- "(.+?)(\\(.+\\))"
  string_match  <- stringr::str_match(rhs, pattern)
  dist_name     <- string_match[[2]]

  args_text <- string_match[[3]] %>%
    stringr::str_remove("^\\(") %>%
    stringr::str_remove("\\)$")

  args_list <- strsplit(args_text, ",")[[1]] %>% stringr::str_trim() |>
    as.list()

  args_names <- get_dist_args(dist_name, language)

  names(args_list) <- args_names

  if(language == "R") dist_name <- Stan_to_R(dist_name)

  c(list(dist_name = dist_name), args_list)

}

get_dist_args <- function(dist, language = "Stan") {

  if(dist == "beta") {

    if(language == "R") return(c("shape1", "shape2"))

    return (c("alpha", "beta"))
  }

  if(dist == "exponential") {

    if(language == "R") return (c("rate"))

    return (c("beta"))
  }

  if(dist == "lognormal") {

    if(language == "R") return(c("meanlog", "sdlog"))

    return (c("mu", "sigma"))
  }


  if(dist == "normal") {

    if(language == "R") return(c("mean", "sd"))

    return (c("mu", "sigma"))
  }

  if(dist == "neg_binomial_2") {

    if(language == "R") return(c("mu", "size"))

    return (c("mu", "phi"))
  }


  if(dist == "poisson") return (c("lambda"))

  msg <- stringr::str_glue("Distribution '{dist}' not supported")
  stop(msg, call. = FALSE)
}

decompose_meas <- function(meas_obj) {

  split_strings <- strsplit(meas_obj, "~")[[1]]
  lhs           <- split_strings[[1]] %>% stringr::str_trim()
  rhs           <- split_strings[[2]] %>% stringr::str_trim()

  list(lhs = lhs,
       rhs = rhs)
}

Stan_to_R <- function(dist_name, type = "r") {

  translation_db <- list(beta           = "beta",
                         exponential    = "exp",
                         lognormal      = "lnorm",
                         normal         = "norm",
                         neg_binomial_2 = "nbinom",
                         poisson        = "pois")

  paste0(type, translation_db[[dist_name]])
}

get_meas_params <- function(meas_mdl, estimated_params) {

  pars_names    <- get_names(estimated_params, "par_name")

  extra_params <- lapply(meas_mdl, extract_extra_params) |> remove_NULL()
  extra_params <- extra_params[!duplicated(extra_params)]

  if(length(extra_params) > 0) {

    for(extra_par_obj in extra_params) {

      extra_par_name <- extra_par_obj$par_name

      if(extra_par_name %in% pars_names) {

        pos_prior      <- which(extra_par_name == pars_names)
        estimated_params[[pos_prior]]$type <- "meas_par"
      } else estimated_params <- c(estimated_params, list(extra_par_obj))
    }
  }

  estimated_params
}

translate_stock_text <- function(stock_txt, delta_counter, lvl_names) {

  nf_pattern <- get_pattern_regex("net_flow")
  is_nf      <- stringr::str_detect(stock_txt, nf_pattern)

  if(is_nf) {

    stock_txt      <- stringr::str_glue("delta_x_{delta_counter}")
    delta_counter  <- delta_counter + 1
  }

  trans_pattern <- get_pattern_regex("var_trans")
  var_trans <- stringr::str_detect(stock_txt, trans_pattern)

  if(var_trans) {

    ptrn         <- "([:alpha:]+)\\((.+?)\\)"
    string_match <- stringr::str_match(stock_txt, ptrn)
    trans        <- string_match[[2]]
    stk_name     <- string_match[[3]]
    stk_trsn     <- translate_stock(stk_name, lvl_names) # Translated
    stock_txt    <- stringr::str_glue("{trans}({stk_trsn})")
  }

  if(!is_nf & !var_trans) stock_txt <- translate_stock(stock_txt, lvl_names)

  list(stock_txt     = stock_txt,
       delta_counter = delta_counter)
}
