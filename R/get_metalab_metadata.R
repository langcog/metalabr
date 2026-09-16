#' @importFrom magrittr "%>%"

metalab_metadata_url <- "https://raw.githubusercontent.com/langcog/metalab/main/metadata/"

get_metalab_domains <- function(domain_file = paste0(metalab_metadata_url, "domains.yaml")) {
  load_yaml_gracefully(domain_file, "MetaLab domains")
}

get_metalab_reports <- function(report_file = paste0(metalab_metadata_url, "reports.yaml")) {
  load_yaml_gracefully(report_file, "MetaLab reports")
}

get_metalab_specs <- function(specs = paste0(metalab_metadata_url, "spec.yaml")) {
  load_yaml_gracefully(specs, "MetaLab field spec")
}

get_metalab_derived_specs <- function(specs_derived =
                                             paste0(metalab_metadata_url, "spec_derived.yaml")) {
  parsed <- load_yaml_gracefully(specs_derived, "MetaLab derived field spec")
  if (is.null(parsed)) return(NULL)
  parsed %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    tibble::as_tibble()
}

#' Get MetaLab dataset metadata
#'
#' By default, reads the dataset registry (one row per dataset: names,
#' domains, citations, curators, summary counts, and data provenance) from
#' the released MetaLab data on Redivis. Passing `dataset_file` instead
#' parses a `datasets.yaml` registry file (the legacy curator path used to
#' build releases).
#'
#' @param dataset_file Optional path or URL of a `datasets.yaml` registry
#'   file; if provided, the registry is parsed from YAML instead of read from
#'   the released data.
#' @param version A MetaLab release name (e.g. `"2026.1"`), or `"current"`
#'   (default) for the latest release (Redivis path only).
#' @return A data.frame of MetaLab dataset metadata (`moderators` and
#'   `subset` are list-columns), or `NULL` (with a message) if released data
#'   could not be fetched.
#' @export
#' @examples
#' \dontrun{
#'   metadata <- get_metalab_metadata()
#'   metalab_data <- get_metalab_data(metadata)
#' }
#'
get_metalab_metadata <- function(dataset_file = NULL, version = "current") {
  if (is.null(dataset_file)) {
    datasets <- read_metalab_table("datasets", version = version)
    if (is.null(datasets)) return(NULL)
    return(restore_list_cols(datasets, c("moderators", "subset")))
  }

  datasets <- load_yaml_gracefully(dataset_file, "MetaLab dataset registry")
  if (is.null(datasets)) return(NULL)

  datasets <- datasets %>% purrr::map(function(x) {
    x$moderators <- list(x$moderators)
    x$subset <- list(x$subset)
    x$reliability <- as.logical(x$reliability)
    x
  })

  dplyr::bind_rows(datasets)
}
