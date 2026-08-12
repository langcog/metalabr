#' metalabr: access MetaLab community-augmented meta-analysis data
#'
#' Read released, versioned MetaLab data (effect sizes from meta-analyses of
#' language acquisition and cognitive development), validate candidate
#' datasets against the MetaLab field specification, and draw the standard
#' MetaLab visualizations. See \url{https://metalab.stanford.edu}.
#'
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select arrange bind_rows bind_cols
#'   left_join distinct group_by ungroup rowwise summarise rename pull n
#'   tibble
#' @importFrom rlang .data
#' @importFrom stats na.omit setNames weighted.mean
#' @importFrom utils head
"_PACKAGE"

## columns referenced via tidyverse non-standard evaluation
utils::globalVariables(c(
  ".", "dataset", "study_ID", "same_infant", "n_1", "n_2", "name",
  "short_name", "domain", "mean_age", "mean_age_1", "mean_age_2", "d_calc",
  "expt_condition", "corr", "row", "short_cite", "same_infant_calc",
  "unique_row", "method"
))
