validate_metalab_data <- function(metalab_metadata, metalab_data, specs) {
  purrr::map(specs, function(spec) {
    validate_metalab_field(metalab_metadata$name, metalab_data, spec)
  })
}

is_valid_metalab_data <- function(metalab_metadata, metalab_data, specs) {
  valid_fields <- validate_metalab_data(metalab_metadata, metalab_data, specs)
  all(unlist(valid_fields))
}
