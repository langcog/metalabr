# Characterization: the full per-dataset pipeline (spec projection, type
# coercion, seed-111 correlation imputation, per-row effect sizes, derived
# columns) must reproduce the released idspref rows exactly from the archived
# raw sheet. The post-tidy steps replicate get_and_validate_sheets() (which
# additionally fetches from the network, so it is not called directly here).

test_that("tidy_dataset + post-processing reproduces released idspref", {
  raw <- suppressWarnings(readr::read_csv(
    test_path("fixtures", "idspref_raw.csv"), show_col_types = FALSE))
  expected <- readRDS(test_path("fixtures", "idspref_expected.rds"))
  specs <- metalabr:::get_metalab_specs(test_path("fixtures", "spec.yaml"))
  meta <- readRDS(test_path("fixtures", "datasets_meta.rds"))
  meta <- meta[meta$short_name == "idspref", ]

  tidied <- suppressWarnings(
    metalabr:::tidy_dataset(meta, raw, specs))

  # replicate get_and_validate_sheets() post-processing
  avg_month <- 365.2425 / 12.0
  got <- tidied %>%
    dplyr::mutate(all_mod = "", mean_age_months = mean_age / avg_month) %>%
    dplyr::filter(!is.na(d_calc)) %>%
    dplyr::mutate(
      year = ifelse(grepl("submitted", study_ID), Inf,
                    stringr::str_extract(study_ID, "([:digit:]{4})")),
      study_ID = as.character(study_ID))

  expect_equal(nrow(got), nrow(expected))

  compare_cols <- c("study_ID", "expt_num", "n_1", "n_2", "mean_age",
                    "mean_age_months", "n", "same_infant_calc", "unique_row",
                    "method", "year", "dataset", "short_name", "domain",
                    "corr_imputed",
                    "d_calc", "d_var_calc", "g_calc", "g_var_calc",
                    "r_calc", "r_var_calc", "z_calc", "z_var_calc",
                    "log_odds_calc", "log_odds_var_calc", "es_method")
  for (col in compare_cols) {
    expect_same_value(got[[col]], expected[[col]], label = col)
  }
})
