#' Get raw data used by Metalab
#'
#' @param dataset_info A data.frame of datasets created with get_metalab_dataset_info()
#' @export
#' @return A data.frame of raw data read from Google Sheets
#' @examples
#' \dontrun{
#'   metadata <- get_metalab_metadata()
#'   metalab_data <- get_metalab_data(metadata)
#' }
#' 
get_metalab_data <- function(metalab_metadata, short_names, domains, specs, perform_validation = TRUE) {
  if (missing(specs)) {
    specs <- get_metalab_specs()
  }
  
  if (!missing(short_names) && !missing(domains)) {
    stop("Only provide one of short_names or domains")
  }

  if (!missing(short_names)) {
    metalab_metadata <- metalab_metadata %>% filter(short_name %in% short_names)
  }

  if (!missing(domains)) {
    metalab_metadata <- metalab_metadata %>% filter(domain %in% domains)
  }
  
  metalab_metadata %>%
    purrr::pmap_dfr(function(...) {
        get_and_validate_sheets(list(...), specs, perform_validation = perform_validation)
    }) 
}
