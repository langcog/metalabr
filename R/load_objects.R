#' @importFrom magrittr "%>%"

ml_metadata_url <- "https://raw.githubusercontent.com/langcog/metalab2/master/metadata/"

get_metalab_domain_info <- function(domain_file = paste0(ml_metadata_url, "domains.yaml")) {
  yaml::yaml.load_file(domain_file)
}

get_metalab_report_info <- function(report_file = paste0(ml_metadata_url, "reports.yaml")) {
  yaml::yaml.load_file(report_file)
}

get_metalab_field_info <- function(field_file = paste0(ml_metadata_url, "spec.yaml")) {
  yaml::yaml.load_file(field_file)
}

get_metalab_derived_field_info <- function(derived_field_file =
                                             paste0(ml_metadata_url, "spec_derived.yaml")) {
  yaml::yaml.load_file(derived_field_file) %>%
    transpose() %>%
    simplify_all() %>%
    dplyr::as_data_frame()
}

#' Get raw data used by Metalab
#'
#' @param dataset_file A file or URL with MetaLab dataset metadata, defaults to the current MetaLab dataset metadata hosted in the langcog/metalab2 repository on Github
#' @export
#' @return A data.frame of MetaLab dataset metadata
#' @examples
#' \dontrun{
#'   ml_dataset_info <- metalabr::get_metalab_dataset_info()
#'   ml_data <- metalabr::get_metalab_data(ml_dataset_info)
#' }
#' 
get_metalab_dataset_info <- function(dataset_file = paste0(ml_metadata_url, "datasets.yaml")) {
  datasets <- yaml::yaml.load_file(dataset_file)

  datasets <- datasets %>% purrr::map(function(x) {
    x$moderators <- list(x$moderators)
    x$subset <- list(x$subset)
    x$reliability <- as.logical(x$reliability)
    x
  })

  bind_rows(datasets)
}
