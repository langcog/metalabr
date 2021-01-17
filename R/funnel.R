alpha <- .05
CRIT_95 <- 1.96
CRIT_99 <- 2.58
ma_method <- "REML_mv"

funnel_plot <- function(metalab_data, short_names, es_col, es_var_col, moderators, fields_derived) {
  metalab_data <- mod_data(metalab_data, moderators, fields_derived)
  mod_factor <- factor(metalab_data[[mod_group(moderators,fields_derived)]])
  metalab_data[[mod_group(moderators,fields_derived)]] <-
    factor(metalab_data[[mod_group(moderators,fields_derived)]],
           levels = rev(levels(mod_factor)))
  metalab_data <- metalab_data %>% filter(short_name %in% short_names)
  
  cat(es_var_col, '\n')
  if (length(moderators) == 0) {
    d <- data_frame(se = sqrt(model(metalab_data, moderators, es_col, es_var_col)$vi),
                    es = model(metalab_data, moderators, es_col, es_var_col)$yi)
    center <- mean(d$es)
    xlabel <- "\nEffect Size"
    ylabel <- "Standard Error\n"
  } else {
    r <- rstandard(model(metalab_data, moderators, es_col, es_var_col))
    d <- data_frame(se = r$se, es = r$resid)
    center <- 0
    xlabel <- "\nResidual Effect Size"
    ylabel <- "Residual Standard Error\n"
  }

  d[[mod_group(moderators, fields_derived)]] <-
    mod_data(metalab_data, moderators, fields_derived)[[mod_group(moderators, fields_derived)]]

  lower_lim <- max(d$se) + .05 * max(d$se)
  funnel95 <- data.frame(x = c(center - lower_lim * CRIT_95, center,
                               center + lower_lim * CRIT_95),
                         y = c(-lower_lim, 0, -lower_lim),
                         stringsAsFactors = FALSE)

  left_lim99 <- ifelse(center - lower_lim * CRIT_99 < min(d$es),
                       center - lower_lim * CRIT_99,
                       min(d$es))
  right_lim99 <- ifelse(center + lower_lim * CRIT_99 > max(d$es),
                        center + lower_lim * CRIT_99,
                        max(d$es))
  funnel99 <- data.frame(x = c(center - lower_lim * CRIT_99, center,
                               center + lower_lim * CRIT_99),
                         y = c(-lower_lim, 0, -lower_lim),
                         stringsAsFactors = FALSE)

  labels <- if (mod_group(moderators, fields_derived) == "all_mod") NULL
  else
    setNames(paste(mod_data(metalab_data, moderators, fields_derived)
                   [[mod_group(moderators, fields_derived)]], "  "),
             mod_data(metalab_data, moderators, fields_derived)[[mod_group(moderators, fields_derived)]])
  
  guide <- if (mod_group(moderators, fields_derived) == "all_mod") FALSE else "legend"

  ggplot(d) +
    geom_polygon(aes(x = x, y = y), data = funnel95, alpha = .5,
                 fill = "grey80") +
    geom_polygon(aes(x = x, y = y), data = funnel99, alpha = .5,
                 fill = "grey70") +
    geom_point(aes_string(x = "es", y = "-se", colour = mod_group(moderators, fields_derived))) +
    geom_vline(aes(), xintercept = center, linetype = "dotted", color = "black") +
    xlab(xlabel) +
    ylab(ylabel) +
    scale_x_continuous(limits = c(left_lim99, right_lim99)) +
    scale_y_continuous(labels = function(x){abs(x)}) +
    theme(panel.background = element_rect(fill = "grey"),
          panel.grid.major =  element_line(colour = "darkgrey", size = 0.2),
          panel.grid.minor =  element_line(colour = "darkgrey", size = 0.5))
}

model <- function(metalab_data, moderators, es_col, es_var_col) {
  cat(es_var_col, '\n')
  if (length(moderators) == 0) {
    no_mod_model(metalab_data, es_col, es_var_col)
  } else {
    mods <- paste(moderators, collapse = "+")
    rma_formula <- as.formula(sprintf("%s ~ %s", es_col, mods))
    if (ma_method == "REML_mv") {
      metafor::rma.mv(rma_formula, V = mod_data(metalab_data, moderators, fields_derived)[[es_var_col]],
                      random = ~ 1 | short_cite / same_infant_calc / unique_row,
                      #Cluster by paper, then participant group, then add random effect for each effect size
                      slab = make.unique(short_cite), data = mod_data(metalab_data, moderators, fields_derived),
                      method = "REML")
    } else {
      metafor::rma(rma_formula, vi = mod_data(metalab_data, moderators, fields_derived)[[es_var_col]],
                   slab = make.unique(short_cite), data = mod_data(metalab_data, moderators, fields_derived),
                   method = ma_method)
    }
  }
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

