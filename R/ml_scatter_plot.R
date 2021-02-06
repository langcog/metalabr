#' @export
ml_scatter_plot <- function(scatter_curve) {

  labels <- if (mod_group() == "all_mod") NULL else
                                                 setNames(paste(mod_data()[[mod_group()]], "  "), mod_data()[[mod_group()]])

  guide <- if (mod_group() == "all_mod") FALSE else "legend"
  p <- ggplot(mod_data(), aes_string(x = "mean_age_months", y = es(),
                                     colour = mod_group())) +
    geom_jitter(aes(size = n, text = paste(short_cite, expt_num)), alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    scale_colour_solarized(name = "", labels = labels, guide = guide) +
    scale_size_continuous(guide = FALSE) +
    xlab("\nMean Subject Age (Months)") +
    ylab("Effect Size\n")

  if (scatter_curve == "lm") {
    p <- p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
                         method = "lm", se = FALSE)
  } else if (scatter_curve == "loess") {
    p <- p + geom_smooth(aes_string(weight = sprintf("1 / %s", es_var())),
                         method = "loess", se = FALSE, span = 1)
  }

  p <- ggplotly(p, tooltip = c("text"))

  if (mod_group() != "all_mod") {
    p
  } else {
    p %>%
      layout(showlegend = FALSE)
  }

}
