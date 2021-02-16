get_and_validate_sheets <- function(metalab_metadata, specs) {
  cat("Getting raw MetaLab data from Google Sheets for dataset:", metalab_metadata$name, "\n")
  metalab_dataset <- fetch_metalab_data(metalab_metadata$key)

  if (is.null(metalab_dataset)) {
    return()
  }

  is_valid_dataset <- is_valid_metalab_data(metalab_metadata,
                                            metalab_dataset,
                                            specs)

  if (!is_valid_dataset) {
    return()
  }

  avg_month <- 365.2425 / 12.0
  ## NB: do we need all_mod here? what is the d_calc filter?
  tidy_dataset(metalab_metadata, metalab_dataset, specs) %>%
    mutate(all_mod = "",
           mean_age_months = mean_age / avg_month) %>%
    filter(!is.na(d_calc)) %>%
    mutate(
      year = ifelse(
        test = grepl("submitted", study_ID),
        yes = Inf,
        no = stringr::str_extract(study_ID, "([:digit:]{4})")),
      study_ID = as.character(study_ID),
      same_infant = as.character(same_infant),
      expt_condition = as.character(expt_condition))
}
