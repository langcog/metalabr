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
get_metalab_data <- function(dataset_info, short_names, domains, field_info) {
  if (missing(field_info)) {
    field_info <- get_metalab_field_info()
  }
  
  if (!missing(short_names) && !missing(domains)) {
    stop("Only provide one of short_names or domains")
  }

  if (!missing(short_names)) {
    dataset_info <- dataset_info %>% filter(short_name %in% short_names)
  }

  if (!missing(domains)) {
    dataset_info <- dataset_info %>% filter(domain %in% domains)
  }
  
  dataset_info %>%
    purrr::pmap_dfr(function(...) {
        load_and_validate_dataset(list(...), field_info)
    }) 
}

load_and_validate_dataset <- function(dataset_info, field_info) {
  cat("Getting raw MetaLab data from Google Sheets for dataset:", dataset_info$name, "\n")
  dataset_contents <- fetch_dataset(dataset_info$key)

  if (is.null(dataset_contents)) {
    return()
  }

  is_valid_dataset <- is_valid_dataset(dataset_info,
                                       dataset_contents,
                                       field_info)

  if (!is_valid_dataset) {
    return()
  }

  avg_month <- 365.2425 / 12.0
  ## NB: do we need all_mod here? what is the d_calc filter?
  tidy_dataset(dataset_info, dataset_contents, field_info) %>%
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

#' @export
#' @param metalab_dataset_info, metalab_data
#' @return 
#' 
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

