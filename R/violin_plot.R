mod_data <- function(ml_data, moderators, fields_derived) {
  if (is.null(moderators)) {
    ml_data %>%
      combine_mods(categorical_mods(moderators, fields_derived), moderators, fields_derived)
  } else { 
    ml_data %>%
      filter(!is.na(moderators)) %>%
      combine_mods(categorical_mods(moderators, fields_derived), moderators, fields_derived)
  }
}


categorical_mods <- function(moderators, fields_derived) {
  if (is.null(moderators)) {
    return(NULL)
  }
  
  ret <- purrr::keep(moderators, function(mod) {
    # assumes all derived fields are non-categorical, which may change?
    !(mod %in% fields_derived$field) &&
      purrr::keep(field_info, ~.x$field == mod)[[1]]$type %in% c("string", "options")
  })

  str(ret)
  return(ret)
}

mod_group <- function(moderators, fields_derived) {
  if (length(categorical_mods(moderators, fields_derived))) {
    paste(categorical_mods(moderators, fields_derived), collapse = "_")
  } else {
    "all_mod"
  }
}

combine_mods <- function(df, cols, moderators,fields_derived) {
  if (mod_group(moderators, fields_derived) != "all_mod" && length(cols) > 1) {
    df[[mod_group(moderators, fields_derived)]] <- do.call(paste, c(map(cols, ~df[[.x]]), sep = "\n"))
  }
  df
}

violin_plot <- function(metalab_data, short_names, effect_size_type, moderators, fields_derived) {
  valid_effect_size_types <- c("g_calc")
  if (!effect_size_type %in% valid_effect_size_types) {
    stop("effect size specified incorrectly")
  }
  
  plt_data <- mod_data(metalab_data, moderators, fields_derived)
  mod_factor <- factor(plt_data[[mod_group(moderators,fields_derived)]])
  plt_data[[mod_group(moderators,fields_derived)]] <- factor(plt_data[[mod_group(moderators,fields_derived)]],
                                                             levels = rev(levels(mod_factor)))
  

  plt_data <- metalab_data %>% filter(short_name %in% short_names)
  
  ggplot(plt_data, aes_string(x = moderators, y = effect_size_type,
                              colour = moderators)) +
    coord_flip() +
    geom_violin() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_jitter() +
    xlab("") +
    ylab("Effect Size\n")
}

