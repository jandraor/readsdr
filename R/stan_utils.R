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

  args_text <- string_match[[3]] |>
    stringr::str_remove("^\\(") |>
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
  lhs           <- split_strings[[1]] |> stringr::str_trim()
  rhs           <- split_strings[[2]] |> stringr::str_trim()

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

  for(meas_obj in meas_mdl) {

    estimated_params <- tidy_meas_params(meas_obj, estimated_params)

  }

  estimated_params <- estimated_params[!duplicated(estimated_params)]

  estimated_params
}

tidy_meas_params <- function(meas_obj, estimated_params) {

  decomposed_meas <- decompose_meas(meas_obj)
  dist_obj        <- get_dist_obj(decomposed_meas$rhs)
  pars_names      <- get_names(estimated_params, "par_name")
  dn              <- dist_obj$dist_name

  if(dn == "neg_binomial_2") {

    if(is_string_numeric(dist_obj$phi)) return(estimated_params)

    par_name <- as.character(stringr::str_glue("inv_{dist_obj$phi}"))

    prior_obj <- list(par_name  = par_name,
                      dist      = "exponential",
                      beta      = 5,
                      min       = 0,
                      type      = "meas_par",
                      par_trans = "inv")

    estimated_params <- c(estimated_params, list(prior_obj))

    message(stringr::str_glue("readsdr added the prior `exp(5)` to the inverse of {dist_obj$phi}"))
  }

  if(dn == "normal" | dn == "lognormal") {

    meas_par_name <- as.character(stringr::str_glue("{dist_obj$sigma}"))

    if(!meas_par_name %in% pars_names) {

      msg <- stringr::str_glue("Please add a prior for `{meas_par_name}`.")
      stop(msg, call. = FALSE)
    }

    pos_prior                          <- which(meas_par_name == pars_names)
    estimated_params[[pos_prior]]$type <- "meas_par"
  }

  estimated_params
}


translate_stock_text <- function(stock_txt, delta_counter, lvl_names,
                                 array_name = "x") {

  nf_pattern <- get_pattern_regex("net_flow")
  is_nf      <- stringr::str_detect(stock_txt, nf_pattern)

  if(is_nf) {

    stock_txt      <- stringr::str_glue("delta_{array_name}_{delta_counter}")
    delta_counter  <- delta_counter + 1
  }

  trans_pattern <- get_pattern_regex("var_trans")
  var_trans <- stringr::str_detect(stock_txt, trans_pattern)

  if(var_trans) {

    ptrn         <- "([:alpha:]+)\\((.+?)\\)"
    string_match <- stringr::str_match(stock_txt, ptrn)
    trans        <- string_match[[2]]
    stk_name     <- string_match[[3]]
    stk_trsn     <- translate_stock(stk_name, lvl_names, array_name) # Translated
    stock_txt    <- stringr::str_glue("{trans}({stk_trsn})")
  }

  if(!is_nf & !var_trans) stock_txt <- translate_stock(stock_txt, lvl_names,
                                                       array_name)

  list(stock_txt     = as.character(stock_txt),
       delta_counter = delta_counter)
}

translate_stock <- function(stk_txt, lvl_names, array_name) {

  stk_txt   <- stringr::str_trim(stk_txt)
  meas_size <- get_meas_size(stk_txt)

  if(is.infinite(meas_size)) {

    idx      <- which(stk_txt == lvl_names)
    trs_stk  <- stringr::str_glue("{array_name}[:, {idx}]")
  }

  if(meas_size == 1) {

    subset_ptrn  <- "([:alpha:]+)\\[[:digit:]+\\]"
    string_match <- stringr::str_match(stk_txt, subset_ptrn)
    stk_name     <- string_match[2]
    idx          <- which(stk_name == lvl_names)
    trs_stk      <- stringr::str_glue("{array_name}0[{idx}]")
  }

  trs_stk # Translated stock
}


determine_meas_size <- function(rhs) {

  dist_obj  <- get_dist_obj(rhs)
  stock     <- dist_obj[[2]]

  ptrn           <- "([:alpha:]+)\\((.+?)\\)"
  wrapped_in_fun <- stringr::str_detect(stock, ptrn)

  if(wrapped_in_fun) {

    string_match <- stringr::str_match(stock, ptrn)
    stock        <- string_match[[3]]
  }

  single_meas_ptrn <- "[:alpha:]+\\[[:digit:]+\\]"
  is_single_meas   <- stringr::str_detect(stock, single_meas_ptrn)

  meas_size <- ifelse(is_single_meas, 1, Inf)
}

get_meas_size <- function(stk_txt) {

  meas_size <- Inf

  single_meas_ptrn <- "[:alpha:]+\\[[:digit:]+\\]"
  is_subset_meas   <- stringr::str_detect(stk_txt, single_meas_ptrn)

  if(is_subset_meas) meas_size <- 1

  meas_size
}

# This function identifies measurements of net flows
subset_delta_meas <- function(meas_mdl) {

  lapply(meas_mdl, function(meas_obj) {

    pattern          <- "net_flow\\(.+?\\)"
    pattern_detected <- stringr::str_detect(meas_obj, pattern)

    if(!pattern_detected) return (NULL)

    meas_obj
  })
}

extract_delta_decl <- function(meas_mdl, n_var = "n_obs", var_name = "x") {

  lapply(seq_along(meas_mdl), function(i) {

    meas_obj <- meas_mdl[[1]]
    as.character(stringr::str_glue("  array[{n_var}] real delta_{var_name}_{i};"))
  })
}
