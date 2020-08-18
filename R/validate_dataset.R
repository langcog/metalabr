#' @export
validate_dataset <- function(dataset_meta, dataset_contents, field_info) {
  purrr::map(field_info, function(field) {
    validate_dataset_field(dataset_meta$name, dataset_contents, field)
  })
}

#' @export
is_valid_dataset <- function(dataset_meta, dataset_contents, field_info) {
  valid_fields <- validate_dataset(dataset_meta, dataset_contents, field_info)
  all(unlist(valid_fields))
}
