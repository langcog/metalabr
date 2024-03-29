#' @export
metalab_forest_plot <- function(metalab_data, short_name, es_col, es_var_col,
                                moderators = NULL,
                                specs_derived = get_metalab_derived_specs(),
                                sort_order = "effects",
                                alpha = 0.05) {

  metalab_data <- mod_data(metalab_data, moderators, specs_derived)
  mod_factor <- factor(metalab_data[[mod_group(moderators,specs_derived)]])
  metalab_data[[mod_group(moderators,specs_derived)]] <-
    factor(metalab_data[[mod_group(moderators,specs_derived)]],
           levels = rev(levels(mod_factor)))
  sn <- short_name
  metalab_data <- metalab_data %>% dplyr::filter(short_name == sn)
  
  f <- fitted(model(metalab_data, moderators, es_col, es_var_col))
  p <- predict(model(metalab_data, moderators, es_col, es_var_col))

  forest_data <- data.frame(effects = as.numeric(model(metalab_data, moderators, es_col, es_var_col)$yi.f),
                            variances = model(metalab_data, moderators, es_col, es_var_col)$vi.f, stringsAsFactors = FALSE) %>%
    mutate(effects.cil = effects -
             qnorm(alpha / 2, lower.tail = FALSE) * sqrt(variances),
           effects.cih = effects +
             qnorm(alpha / 2, lower.tail = FALSE) * sqrt(variances),
           estimate = as.numeric(f),
           short_cite = names(f),
           estimate.cil = p$ci.lb,
           estimate.cih = p$ci.ub,
           inverse_vars = 1/variances,
           identity = 1) %>%
    left_join(mutate(mod_data(metalab_data, moderators, specs_derived), short_cite = make.unique(short_cite))) %>%
    arrange_(.dots = list(sprintf("desc(%s)", sort_order),
                          "desc(effects)")) %>%
    mutate(short_cite = factor(short_cite, levels = short_cite))

  labels <-
    if (mod_group(moderators,specs_derived) == "all_mod")
      NULL
  else
    setNames(paste(mod_data(metalab_data, moderators, specs_derived)[[mod_group(moderators,specs_derived)]], "  "),
             mod_data(metalab_data, moderators, specs_derived)[[mod_group(moderators,specs_derived)]])
  
  guide <- if (mod_group(moderators,specs_derived) == "all_mod") FALSE else "legend"

  ggplot(data = forest_data, aes(text = paste0("Experiment #", expt_num))) +
    geom_point(aes(x = short_cite, y = effects, size = inverse_vars)) +
    geom_linerange(aes(x = short_cite, y = effects, ymin = effects.cil, ymax = effects.cih)) +
    geom_point(aes_string(x = "short_cite", y = "estimate", colour = mod_group(moderators,specs_derived)),
               shape = 17) +
    geom_linerange(aes_string(x = "short_cite", y = "estimate", ymin = "estimate.cil",
                              ymax = "estimate.cih", colour = mod_group(moderators,specs_derived))) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    coord_flip() +
    scale_size_continuous(range = c(1, 3), guide = FALSE) +
    xlab("") +
    ylab("Effect Size")
}
