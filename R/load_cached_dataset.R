#' @export
load_cached_data <- function(rdata_file) {
  if(file.exists(rdata_file)) {
    load(rdata_file, envir = .GlobalEnv)
  } else {
    cat("File ", rdata_file, " not found.")
  }
}
