## the four exported visualizations, mirroring the plots on
## metalab.stanford.edu (scatter, violin, forest, funnel)

prep_plot_data <- function(metalab_data, short_names, moderators) {
  metalab_data %>%
    filter(short_name %in% short_names) %>%
    mod_data(moderators)
}

group_scale <- function(dat, group_col) {
  if (group_col == "all_mod") {
    ggplot2::scale_colour_manual(values = solarized_palette[1], guide = "none")
  } else {
    ggplot2::scale_colour_manual(
      values = rep(solarized_palette, length.out =
                     length(unique(dat[[group_col]]))))
  }
}

#' Scatter plot of effect sizes across age
#'
#' Effect sizes against mean participant age (months), with a weighted
#' smoothing curve, colored by the selected categorical moderators.
#'
#' @param metalab_data MetaLab effect-size data (from
#'   \code{\link{get_metalab_data}}).
#' @param short_name A dataset short name.
#' @param es_col,es_var_col Effect size and variance columns (e.g. `"g_calc"`,
#'   `"g_var_calc"`).
#' @param moderators Optional character vector of moderator columns.
#' @param scatter_curve `"loess"` (default) or `"lm"`; smoothing is weighted
#'   by inverse variance.
#' @return A ggplot object.
#' @export
metalab_scatter_plot <- function(metalab_data, short_name, es_col = "g_calc",
                                 es_var_col = "g_var_calc", moderators = NULL,
                                 scatter_curve = "loess") {
  dat <- prep_plot_data(metalab_data, short_name, moderators)
  group_col <- mod_group(dat, moderators)

  p <- ggplot2::ggplot(dat,
      ggplot2::aes(x = .data$mean_age_months, y = .data[[es_col]],
                   colour = .data[[group_col]])) +
    ggplot2::geom_jitter(ggplot2::aes(size = .data$n), alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    group_scale(dat, group_col) +
    ggplot2::scale_size_continuous(guide = "none") +
    ggplot2::labs(x = "\nMean Subject Age (Months)", y = "Effect Size\n",
                  colour = "")

  smooth_method <- if (identical(scatter_curve, "lm")) "lm" else "loess"
  p + ggplot2::geom_smooth(
    ggplot2::aes(weight = 1 / .data[[es_var_col]]),
    method = smooth_method, se = FALSE,
    span = if (smooth_method == "loess") 1 else NULL,
    formula = y ~ x)
}

#' Violin plot of effect-size density
#'
#' @inheritParams metalab_scatter_plot
#' @param short_names One or more dataset short names.
#' @param es_col Effect size column (e.g. `"g_calc"`).
#' @return A ggplot object.
#' @export
metalab_violin_plot <- function(metalab_data, short_names, es_col = "g_calc",
                                moderators = NULL) {
  dat <- prep_plot_data(metalab_data, short_names, moderators)
  group_col <- mod_group(dat, moderators)
  dat[[group_col]] <- factor(dat[[group_col]])
  dat[[group_col]] <- factor(dat[[group_col]],
                             levels = rev(levels(dat[[group_col]])))

  ggplot2::ggplot(dat, ggplot2::aes(x = .data[[group_col]],
                                    y = .data[[es_col]],
                                    colour = .data[[group_col]])) +
    ggplot2::coord_flip() +
    ggplot2::geom_violin() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::geom_jitter(height = 0) +
    group_scale(dat, group_col) +
    ggplot2::labs(x = "", y = "Effect Size\n") +
    ggplot2::theme(legend.position = "none")
}

#' Forest plot with multilevel model estimates
#'
#' Observed effect sizes (with CIs, sized by precision) alongside the fitted
#' estimates of the canonical MetaLab multilevel model.
#'
#' @inheritParams metalab_scatter_plot
#' @param sort_order One of `"effects"`, `"variances"`, `"estimate"`,
#'   `"study_ID"`, `"year"`.
#' @param alpha CI level (default 0.05 for 95% CIs).
#' @return A ggplot object.
#' @export
metalab_forest_plot <- function(metalab_data, short_name, es_col = "g_calc",
                                es_var_col = "g_var_calc", moderators = NULL,
                                sort_order = "effects", alpha = 0.05) {
  dat <- prep_plot_data(metalab_data, short_name, moderators)
  group_col <- mod_group(dat, moderators)

  fit <- metalab_model(dat, moderators, es_col, es_var_col)
  f <- stats::fitted(fit)
  p <- stats::predict(fit)
  crit <- stats::qnorm(alpha / 2, lower.tail = FALSE)

  forest_data <- dat %>%
    mutate(short_cite = make.unique(.data$short_cite),
           effects = as.numeric(fit$yi.f),
           variances = fit$vi.f,
           effects.cil = .data$effects - crit * sqrt(.data$variances),
           effects.cih = .data$effects + crit * sqrt(.data$variances),
           estimate = as.numeric(f),
           estimate.cil = p$ci.lb,
           estimate.cih = p$ci.ub,
           inverse_vars = 1 / .data$variances)

  sort_col <- match.arg(sort_order,
                        c("effects", "variances", "estimate", "study_ID", "year"))
  if (sort_col == "variances") sort_col <- "inverse_vars"
  forest_data <- forest_data %>%
    arrange(dplyr::desc(.data[[sort_col]]), dplyr::desc(.data$effects)) %>%
    mutate(short_cite = factor(.data$short_cite, levels = .data$short_cite))

  ggplot2::ggplot(forest_data) +
    ggplot2::geom_point(ggplot2::aes(x = .data$short_cite, y = .data$effects,
                                     size = .data$inverse_vars)) +
    ggplot2::geom_linerange(ggplot2::aes(x = .data$short_cite,
                                         ymin = .data$effects.cil,
                                         ymax = .data$effects.cih)) +
    ggplot2::geom_point(ggplot2::aes(x = .data$short_cite, y = .data$estimate,
                                     colour = .data[[group_col]]), shape = 17) +
    ggplot2::geom_linerange(ggplot2::aes(x = .data$short_cite,
                                         ymin = .data$estimate.cil,
                                         ymax = .data$estimate.cih,
                                         colour = .data[[group_col]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    ggplot2::coord_flip() +
    ggplot2::scale_size_continuous(range = c(1, 3), guide = "none") +
    group_scale(forest_data, group_col) +
    ggplot2::labs(x = "", y = "Effect Size", colour = "")
}

#' Funnel plot of bias in effect sizes
#'
#' With no moderators, a classic funnel (effect size against standard error,
#' with 95% and 99% pseudo-confidence regions around the mean). With
#' moderators, a residual funnel around zero.
#'
#' @inheritParams metalab_violin_plot
#' @param es_var_col Effect size variance column.
#' @return A ggplot object.
#' @export
metalab_funnel_plot <- function(metalab_data, short_names, es_col = "g_calc",
                                es_var_col = "g_var_calc", moderators = NULL) {
  CRIT_95 <- 1.96
  CRIT_99 <- 2.58

  dat <- prep_plot_data(metalab_data, short_names, moderators)
  group_col <- mod_group(dat, moderators)
  fit <- metalab_model(dat, moderators, es_col, es_var_col)

  if (length(moderators) == 0) {
    d <- dplyr::tibble(se = sqrt(fit$vi), es = as.numeric(fit$yi))
    center <- mean(d$es)
    xlabel <- "\nEffect Size"
    ylabel <- "Standard Error\n"
  } else {
    r <- stats::rstandard(fit)
    d <- dplyr::tibble(se = r$se, es = r$resid)
    center <- 0
    xlabel <- "\nResidual Effect Size"
    ylabel <- "Residual Standard Error\n"
  }
  d[[group_col]] <- dat[[group_col]]

  lower_lim <- max(d$se) + .05 * max(d$se)
  funnel95 <- data.frame(x = c(center - lower_lim * CRIT_95, center,
                               center + lower_lim * CRIT_95),
                         y = c(-lower_lim, 0, -lower_lim))
  funnel99 <- data.frame(x = c(center - lower_lim * CRIT_99, center,
                               center + lower_lim * CRIT_99),
                         y = c(-lower_lim, 0, -lower_lim))
  left_lim99 <- min(center - lower_lim * CRIT_99, min(d$es))
  right_lim99 <- max(center + lower_lim * CRIT_99, max(d$es))

  ggplot2::ggplot(d) +
    ggplot2::geom_polygon(ggplot2::aes(x = .data$x, y = .data$y),
                          data = funnel95, alpha = .5, fill = "grey80") +
    ggplot2::geom_polygon(ggplot2::aes(x = .data$x, y = .data$y),
                          data = funnel99, alpha = .5, fill = "grey70") +
    ggplot2::geom_point(ggplot2::aes(x = .data$es, y = -.data$se,
                                     colour = .data[[group_col]])) +
    ggplot2::geom_vline(xintercept = center, linetype = "dotted",
                        color = "black") +
    ggplot2::labs(x = xlabel, y = ylabel, colour = "") +
    group_scale(d, group_col) +
    ggplot2::scale_x_continuous(limits = c(left_lim99, right_lim99)) +
    ggplot2::scale_y_continuous(labels = function(x) abs(x)) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "grey"),
      panel.grid.major = ggplot2::element_line(colour = "darkgrey",
                                               linewidth = 0.2),
      panel.grid.minor = ggplot2::element_line(colour = "darkgrey",
                                               linewidth = 0.5))
}

#' Egger-style test for funnel-plot asymmetry
#'
#' Adds the square root of each effect size's variance as a fixed-effect
#' moderator to the canonical multilevel model and reports its coefficient —
#' the standard multilevel adaptation of Egger's regression test. (The legacy
#' application attempted \code{metafor::regtest()}, which does not support
#' multilevel models.)
#'
#' @inheritParams metalab_funnel_plot
#' @return A one-row data.frame with columns `z` and `p`.
#' @export
metalab_funnel_test <- function(metalab_data, short_names, es_col = "g_calc",
                                es_var_col = "g_var_calc", moderators = NULL) {
  dat <- prep_plot_data(metalab_data, short_names, moderators)
  dat$sqrt_vi_egger <- sqrt(dat[[es_var_col]])
  fit <- metalab_model(dat, c(moderators, "sqrt_vi_egger"), es_col, es_var_col)
  i <- which(rownames(fit$b) == "sqrt_vi_egger")
  data.frame(z = unname(fit$zval[i]), p = unname(fit$pval[i]))
}
