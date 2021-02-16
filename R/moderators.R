mod_data <- function(metalab_data, moderators, specs_derived) {
  if (is.null(moderators)) {
    metalab_data %>%
      combine_mods(categorical_mods(moderators, specs_derived), moderators, specs_derived)
  } else { 
    metalab_data %>%
      filter(!is.na(moderators)) %>%
      combine_mods(categorical_mods(moderators, specs_derived), moderators, specs_derived)
  }
}


categorical_mods <- function(moderators, specs_derived) {
  if (is.null(moderators)) {
    return(NULL)
  }
  
  ret <- purrr::keep(moderators, function(mod) {
    # assumes all derived fields are non-categorical, which may change?
    !(mod %in% specs_derived$field) &&
      purrr::keep(specs, ~.x$field == mod)[[1]]$type %in% c("string", "options")
  })

  str(ret)
  return(ret)
}

mod_group <- function(moderators, specs_derived) {
  if (length(categorical_mods(moderators, specs_derived))) {
    paste(categorical_mods(moderators, specs_derived), collapse = "_")
  } else {
    "all_mod"
  }
}

combine_mods <- function(df, cols, moderators,specs_derived) {
  if (mod_group(moderators, specs_derived) != "all_mod" && length(cols) > 1) {
    df[[mod_group(moderators, specs_derived)]] <- do.call(paste, c(map(cols, ~df[[.x]]), sep = "\n"))
  }
  df
}

no_mod_model <- function(metalab_data, es_col, es_var_col) {
  if (ma_method == "REML_mv") {
    metafor::rma.mv(yi = metalab_data[[es_col]], V = metalab_data[[es_var_col]],
                    random = ~ 1 | metalab_data[["short_cite"]] / metalab_data[["same_infant_calc"]] /
                      metalab_data[["unique_row"]],
                    slab = make.unique(metalab_data[["short_cite"]]),
                    method = "REML")
  } else {
    metafor::rma(yi = metalab_data[[es_var_col]], vi = metalab_data[[es_var_col]],
                 slab = make.unique(metalab_data[["short_cite"]]),
                 method = ma_method)
  }
}
