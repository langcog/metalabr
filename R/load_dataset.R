get_data <- function(datasets) {
  datasets %>% pmap_dfr(function(...) {
    load_and_validate_dataset(list(...))
  })
}

load_and_validate_dataset <- function(dataset_info) {
  dataset_contents <- fetch_dataset(dataset_info$key)

  if (is.null(dataset_contents)) {
    return()
  }

  is_valid_dataset <- validate_dataset(dataset_info, dataset_contents)

  if (!is_valid_dataset) {
    return()
  }

  avg_month <- 365.2425 / 12.0
  ## NB: do we need all_mod here? what is the d_calc filter?
  tidy_df <- tidy_dataset(dataset_info, dataset_contents) %>%
    mutate(mean_age_months = mean_age / avg_month) %>%
    filter(!is.na(d_calc))
}

add_summary <- function(dataset_info, metalab_data) {
  studies <- metalab_data %>%
    group_by(dataset) %>%
    summarise(
      num_experiments = n(),
      num_papers = length(unique(study_ID)))

  subjects <- metalab_data %>%
    distinct(dataset, study_ID, same_infant, .keep_all = TRUE) %>%
    group_by(dataset) %>%
    summarise(num_subjects = sum(n_1, n_2, na.rm = TRUE))

  dataset_info %>%
    rename(dataset = name) %>%
    left_join(studies, by = "dataset") %>%
    left_join(subjects, by = "dataset") %>%
    rename(name = dataset)
}
