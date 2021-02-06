#' @export
ml_violin_plot <- function(metalab_data, short_names, effect_size_type, moderators, specs_derived) {
  valid_effect_size_types <- c("g_calc")
  if (!effect_size_type %in% valid_effect_size_types) {
    stop("effect size specified incorrectly")
  }
  
  plt_data <- mod_data(metalab_data, moderators, specs_derived)
  mod_factor <- factor(plt_data[[mod_group(moderators,specs_derived)]])
  plt_data[[mod_group(moderators,specs_derived)]] <- factor(plt_data[[mod_group(moderators,specs_derived)]],
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

