#' @export
validate_ml_data <- function(ml_metadata, ml_data, specs) {
  purrr::map(specs, function(spec) {
    validate_ml_field(ml_metadata$name, ml_data, spec)
  })
}

is_valid_ml_data <- function(ml_metadata, ml_data, specs) {
  valid_fields <- validate_ml_data(ml_metadata, ml_data, specs)
  all(unlist(valid_fields))
}
