#' @importFrom magrittr "%>%"

metalab_metadata_url <- "https://raw.githubusercontent.com/langcog/metalab/main/metadata/"

get_metalab_domains <- function(domain_file = paste0(metalab_metadata_url, "domains.yaml")) {
  yaml::yaml.load_file(domain_file)
}

get_metalab_reports <- function(report_file = paste0(metalab_metadata_url, "reports.yaml")) {
  yaml::yaml.load_file(report_file)
}

get_metalab_specs <- function(specs = paste0(metalab_metadata_url, "spec.yaml")) {
  yaml::yaml.load_file(specs)
}

get_metalab_derived_specs <- function(specs_derived =
                                             paste0(metalab_metadata_url, "spec_derived.yaml")) {
  yaml::yaml.load_file(specs_derived) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    dplyr::as_data_frame()
}

#' Get raw data used by Metalab
#'
#' @param dataset_file A file or URL with MetaLab dataset metadata, defaults to the current MetaLab dataset metadata hosted in the langcog/metalab2 repository on Github
#' @export
#' @return A data.frame of MetaLab dataset metadata
#' @examples
#' \dontrun{
#'   metadata <- get_metalab_metadata()
#'   metalab_data <- get_metalab_data(metadata)
#' }
#' 
get_metalab_metadata <- function(dataset_file = paste0(metalab_metadata_url, "datasets.yaml")) {
  datasets <- yaml::yaml.load_file(dataset_file)

  datasets <- datasets %>% purrr::map(function(x) {
    x$moderators <- list(x$moderators)
    x$subset <- list(x$subset)
    x$reliability <- as.logical(x$reliability)
    x
  })

  dplyr::bind_rows(datasets)
}
