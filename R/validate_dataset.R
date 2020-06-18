validate_dataset <- function(dataset_meta, dataset_contents) {
  valid_fields <- map(fields, function(field) {
                        validate_dataset_field(dataset_meta$name, dataset_contents, field)
  })
  valid_dataset <- all(unlist(valid_fields))
  return(valid_dataset)
}
