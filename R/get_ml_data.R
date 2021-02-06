#' Get raw data used by Metalab
#'
#' @param dataset_info A data.frame of datasets created with get_metalab_dataset_info()
#' @export
#' @return A data.frame of raw data read from Google Sheets
#' @examples
#' \dontrun{
#'   ml_metadata <- metalabr::get_ml_metadata()
#'   ml_data <- metalabr::get_ml_data(ml_metadata)
#' }
#' 
get_ml_sheets <- function(ml_metadata, short_names, domains, specs) {
  if (missing(specs)) {
    specs <- get_ml_specs()
  }
  
  if (!missing(short_names) && !missing(domains)) {
    stop("Only provide one of short_names or domains")
  }

  if (!missing(short_names)) {
    ml_metadata <- ml_metadata %>% filter(short_name %in% short_names)
  }

  if (!missing(domains)) {
    ml_metadata <- ml_metadata %>% filter(domain %in% domains)
  }
  
  ml_metadata %>%
    purrr::pmap_dfr(function(...) {
        get_and_validate_sheets(list(...), specs)
    }) 
}
