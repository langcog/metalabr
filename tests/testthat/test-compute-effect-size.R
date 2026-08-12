# Characterization: compute_effect_size() must reproduce the released
# MetaLab data (datapages.metalab 2026.1) exactly, across every effect-size
# decision-tree branch that occurs in the database, including the hardcoded
# special-case studies and the prosocial log-odds branch.

test_that("compute_effect_size reproduces released values on all branches", {
  cases <- readRDS(test_path("fixtures", "es_cases.rds"))
  out_cols <- c("d_calc", "d_var_calc", "g_calc", "g_var_calc", "r_calc",
                "r_var_calc", "z_calc", "z_var_calc", "log_odds_calc",
                "log_odds_var_calc", "es_method")

  for (i in seq_len(nrow(cases))) {
    row <- cases[i, ]
    got <- metalabr:::compute_effect_size(
      participant_design = row$participant_design,
      x_1 = row$x_1, x_2 = row$x_2, x_dif = row$x_dif,
      SD_1 = row$SD_1, SD_2 = row$SD_2, SD_dif = row$SD_dif,
      n_1 = row$n_1, n_2 = row$n_2,
      t = row$t, f = row$F, d = row$d, d_var = row$d_var,
      corr = row$corr, corr_imputed = row$corr_imputed,
      r = row$r, r_var = NULL,
      study_ID = row$study_ID, expt_num = row$expt_num,
      special_cases_measures = row$special_cases_measures,
      contrast_sampa = row$contrast_sampa, short_name = row$short_name
    )
    for (col in out_cols) {
      expect_same_value(got[[col]], row[[col]],
                        label = sprintf("case %d (%s/%s) %s",
                                        i, row$study_ID, row$es_method, col))
    }
  }
})
