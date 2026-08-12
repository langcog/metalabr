#' Deprecated: interactive visualization app
#'
#' The Shiny applications formerly bundled with metalabr are superseded by
#' the interactive visualizations on the MetaLab website, which run in the
#' browser against released data.
#'
#' @param ... Ignored.
#' @return Called for its message; returns `NULL` invisibly.
#' @export
viz_app <- function(...) {
  .Deprecated(msg = paste0(
    "viz_app() is deprecated: the interactive visualizations now live at ",
    "https://metalab.stanford.edu (no server required). For plots in R, see ",
    "metalab_scatter_plot(), metalab_violin_plot(), metalab_forest_plot(), ",
    "and metalab_funnel_plot()."))
  invisible(NULL)
}
