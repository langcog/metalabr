validate_dataset <- function(dataset_meta, dataset_contents, field_info) {
  valid_fields <- purrr::map(field_info, function(field) {
    validate_dataset_field(dataset_meta$name, dataset_contents, field)
  })
  valid_dataset <- all(unlist(valid_fields))
  return(valid_dataset)
}
