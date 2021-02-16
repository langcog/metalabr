add_metalab_summary <- function(metalab_dataset_info, metalab_data) {
  studies <- metalab_data %>%
    group_by(dataset) %>%
    summarise(
      num_experiments = n(),
      num_papers = length(unique(study_ID)))

  subjects <- metalab_data %>%
    distinct(dataset, study_ID, same_infant, .keep_all = TRUE) %>%
    group_by(dataset) %>%
    summarise(num_subjects = sum(n_1, n_2, na.rm = TRUE))

  metalab_dataset_info %>%
    rename(dataset = name) %>%
    left_join(studies, by = "dataset") %>%
    left_join(subjects, by = "dataset") %>%
    rename(name = dataset)
}
