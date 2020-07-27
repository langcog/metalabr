#' Get raw data used by Metalab
#'
#' @param dataset_info A data.frame of datasets created with get_metalab_dataset_info()
#' @export
#' @return A data.frame of raw data read from Google Sheets
#' @examples
#' \dontrun{
#'   ml_dataset_info <- metalabr::get_metalab_dataset_info()
#'   ml_data <- metalabr::get_metalab_data(ml_dataset_info)
#' }
#' 
get_metalab_data <- function(dataset_info) {
  dataset_info %>%
    purrr::pmap_dfr(function(...) {
        load_and_validate_dataset(list(...))
    }) 
}

load_and_validate_dataset <- function(dataset_info) {
  cat("Getting raw MetaLab data from Google Sheets for dataset:", dataset_info$name, "\n")
  dataset_contents <- fetch_dataset(dataset_info$key)
  

  field_info <- get_metalab_field_info()

  if (is.null(dataset_contents)) {
    return()
  }

  is_valid_dataset <-
    validate_dataset(dataset_info, dataset_contents, field_info)

  if (!is_valid_dataset) {
    return()
  }

  avg_month <- 365.2425 / 12.0
  ## NB: do we need all_mod here? what is the d_calc filter?
  tidy_dataset(dataset_info, dataset_contents, field_info) %>%
    mutate(mean_age_months = mean_age / avg_month) %>%
    filter(!is.na(d_calc))
}

#' @export
add_metalab_summary_info <- function(metalab_dataset_info, metalab_data) {
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
