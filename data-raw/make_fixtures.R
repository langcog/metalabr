#!/usr/bin/env Rscript
# Generate characterization-test fixtures from the released MetaLab data
# (datapages.metalab 2026.1 == 2023.1). Run from the metalabr repo root with
# the metalab-datapage worktree checked out as a sibling:
#   Rscript data-raw/make_fixtures.R
#
# Produces:
#   tests/testthat/fixtures/es_cases.rds   input args + expected *_calc
#                                          outputs for compute_effect_size(),
#                                          sampled to cover every es_method x
#                                          participant_design branch, all
#                                          hardcoded special cases, and the
#                                          prosocial log-odds branch
#   tests/testthat/fixtures/idspref_raw.csv     raw sheet archive (2026-08-11)
#   tests/testthat/fixtures/idspref_expected.rds  processed pipeline output
#   tests/testthat/fixtures/datasets_meta.rds     parsed registry (32 rows)

suppressMessages({
  library(dplyr)
  library(arrow)
})

staging <- "../metalab-datapage/etl/staging/v2026"
stopifnot(dir.exists(staging))
dir.create("tests/testthat/fixtures", recursive = TRUE, showWarnings = FALSE)

es <- read_parquet(file.path(staging, "effect_sizes.parquet"))

input_cols <- c("participant_design", "x_1", "x_2", "x_dif", "SD_1", "SD_2",
                "SD_dif", "n_1", "n_2", "t", "F", "d", "d_var", "corr",
                "corr_imputed", "r", "study_ID", "expt_num",
                "special_cases_measures", "contrast_sampa", "short_name")
output_cols <- c("d_calc", "d_var_calc", "g_calc", "g_var_calc", "r_calc",
                 "r_var_calc", "z_calc", "z_var_calc", "log_odds_calc",
                 "log_odds_var_calc", "es_method")

set.seed(20260811)
sampled <- bind_rows(
  es %>% group_by(es_method, participant_design) %>%
    slice_sample(n = 5) %>% ungroup(),
  es %>% filter(es_method == "special_case"),
  es %>% filter(short_name == "prosocial") %>% slice_sample(n = 10),
  es %>% filter(study_ID %in% c("Kuhl1982", "Polka1996", "Swoboda1976",
                                "Grieser1989"))
) %>% distinct(short_name, unique_row, .keep_all = TRUE)

es_cases <- sampled %>% select(all_of(c(input_cols, output_cols)))
cat(sprintf("es_cases: %d rows covering methods: %s\n", nrow(es_cases),
            paste(sort(unique(es_cases$es_method)), collapse = ", ")))
saveRDS(es_cases, "tests/testthat/fixtures/es_cases.rds", version = 2)

## full-dataset pipeline fixture: idspref (raw sheet archive + expected output)
file.copy(file.path(staging, "..", "raw_2026", "idspref.csv"),
          "tests/testthat/fixtures/idspref_raw.csv", overwrite = TRUE)
idspref_expected <- es %>% filter(short_name == "idspref")
saveRDS(idspref_expected, "tests/testthat/fixtures/idspref_expected.rds",
        version = 2)
cat(sprintf("idspref: %d expected rows\n", nrow(idspref_expected)))

## registry fixture
datasets <- read_parquet(file.path(staging, "datasets.parquet"))
saveRDS(datasets, "tests/testthat/fixtures/datasets_meta.rds", version = 2)
cat("fixtures written\n")
