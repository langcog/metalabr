#' @export
load_ml_data <- function(rdata_file = get_published_data_url()) {
  load(url(rdata_file), envir = .GlobalEnv)
}

load_ml_cached <- function(rdata_file = get_cached_data_file()) {
  load(rdata_file, envir = .GlobalEnv)
}

get_published_data_url <- function() {
  return("https://raw.githubusercontent.com/langcog/metalab/main/shinyapps/site_data/Rdata/metalab.Rdata")
}

get_cached_data_file <- function() {
  return(here::here("shinyapps", "site_data", "Rdata", "metalab.Rdata"))
}
