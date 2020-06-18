load_cached_dataset <- function(filename) {
  read.csv(
    file.path(here("data", paste0(filename, ".csv"))),
    stringsAsFactors = FALSE) %>%
    mutate(
      filename = filename,
      year = ifelse(
        test = grepl("submitted", study_ID),
        yes = Inf,
        no = stringr::str_extract(study_ID, "([:digit:]{4})"))
    ) %>%
    mutate(
      study_ID = as.character(study_ID),
      same_infant = as.character(same_infant),
      expt_condition = as.character(expt_condition))
}
